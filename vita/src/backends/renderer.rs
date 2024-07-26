#![deny(clippy::unwrap_used)]
// Remove this when we start using `Rc` when compiling for wasm
#![allow(clippy::arc_with_non_send_sync)]

use std::ffi::CStr;
use crate::vitalib::*;
use ruffle_render::backend::ShapeHandleImpl;
use ruffle_render::tessellator::Mesh as OtherMesh;
use std::sync::Arc;
use std::ffi::c_void;
use ruffle_render::bitmap::RgbaBufRead;
use ruffle_render::backend::RenderBackend;
use ruffle_core::ViewportDimensions;
use ruffle_render::bitmap::BitmapSource;
use ruffle_render::backend::ShapeHandle;
use ruffle_render::bitmap::BitmapHandle;
use ruffle_render::commands::CommandList;
use ruffle_render::error::Error as BitmapError;
use ruffle_render::quality::StageQuality;
use ruffle_render::bitmap::PixelRegion;
use ruffle_render::bitmap::SyncHandle;
use ruffle_render::backend::BitmapCacheEntry;
use ruffle_render::backend::Context3DProfile;
use ruffle_render::backend::Context3D;
use std::borrow::Cow;
use ruffle_render::pixel_bender::PixelBenderShader;
use ruffle_render::pixel_bender::PixelBenderShaderHandle;
use ruffle_render::pixel_bender::PixelBenderShaderArgument;
use ruffle_render::backend::PixelBenderTarget;
use ruffle_render::backend::PixelBenderOutput;
use ruffle_render::shape_utils::{DistilledShape, GradientType};
use ruffle_render::tessellator::{
    Gradient as TessGradient, ShapeTessellator,
};
use swf::Color as SwfColor;
use std::ffi::CString;

use sdl2::keyboard::Keycode;
use sdl2::mouse::MouseButton;
use sdl2::pixels::Color;
use sdl2::rect::{Point, Rect};
use sdl2::render::{Canvas, Texture, TextureCreator};
use sdl2::video::{Window, WindowContext};
use sdl2::{controller::Button, event::Event};

const MAX_GRADIENT_COLORS: usize = 15;
const NUM_VERTEX_ATTRIBUTES: u32 = 2;

#[derive(Clone, Debug)]
struct Gradient {
    matrix: [[f32; 3]; 3],
    gradient_type: i32,
    ratios: [f32; MAX_GRADIENT_COLORS],
    colors: [[f32; 4]; MAX_GRADIENT_COLORS],
    repeat_mode: i32,
    focal_point: f32,
    interpolation: swf::GradientInterpolation,
}

impl Gradient {
    fn new(gradient: TessGradient, matrix: [[f32; 3]; 3]) -> Self {
        // TODO: Support more than MAX_GRADIENT_COLORS.
        let num_colors = gradient.records.len().min(MAX_GRADIENT_COLORS);
        let mut ratios = [0.0; MAX_GRADIENT_COLORS];
        let mut colors = [[0.0; 4]; MAX_GRADIENT_COLORS];
        for i in 0..num_colors {
            let record = &gradient.records[i];
            let mut color = [
                f32::from(record.color.r) / 255.0,
                f32::from(record.color.g) / 255.0,
                f32::from(record.color.b) / 255.0,
                f32::from(record.color.a) / 255.0,
            ];
            // Convert to linear color space if this is a linear-interpolated gradient.
            match gradient.interpolation {
                swf::GradientInterpolation::Rgb => {}
                swf::GradientInterpolation::LinearRgb => srgb_to_linear(&mut color),
            }

            colors[i] = color;
            ratios[i] = f32::from(record.ratio) / 255.0;
        }

        for i in num_colors..MAX_GRADIENT_COLORS {
            ratios[i] = ratios[i - 1];
            colors[i] = colors[i - 1];
        }

        Self {
            matrix,
            gradient_type: match gradient.gradient_type {
                GradientType::Linear => 0,
                GradientType::Radial => 1,
                GradientType::Focal => 2,
            },
            ratios,
            colors,
            repeat_mode: match gradient.repeat_mode {
                swf::GradientSpread::Pad => 0,
                swf::GradientSpread::Repeat => 1,
                swf::GradientSpread::Reflect => 2,
            },
            focal_point: gradient.focal_point.to_f32().clamp(-0.98, 0.98),
            interpolation: gradient.interpolation,
        }
    }
}

#[derive(Clone, Debug)]
struct BitmapDraw {
    matrix: [[f32; 3]; 3],
    handle: Option<BitmapHandle>,
    is_repeating: bool,
    is_smoothed: bool,
}

#[derive(Debug)]
enum DrawType {
    Color,
    Gradient(Box<Gradient>),
    Bitmap(BitmapDraw),
}

#[allow(dead_code)]
#[derive(Debug)]
struct Draw {
    draw_type: DrawType,
    vao: u32,
    vertex_buffer: u32,
    index_buffer: u32,
    num_indices: i32,
    num_mask_indices: i32,
}

const COLOR_VERTEX_GLSL: &str = include_str!("../../../render/webgl/shaders/color.vert");
const COLOR_FRAGMENT_GLSL: &str = include_str!("../../../render/webgl/shaders/color.frag");
const TEXTURE_VERTEX_GLSL: &str = include_str!("../../../render/webgl/shaders/texture.vert");
const GRADIENT_FRAGMENT_GLSL: &str = include_str!("../../../render/webgl/shaders/gradient.frag");
const BITMAP_FRAGMENT_GLSL: &str = include_str!("../../../render/webgl/shaders/bitmap.frag");

#[derive(Clone, Debug)]
struct ShaderProgram {
    id : u32,
    vertex_shader : u32,
    fragment_shader : u32,
    vertex_position_location: u32,
    vertex_color_location: u32,
    num_vertex_attributes: u32,
}

impl ShaderProgram {
    pub fn new() -> Self {
        Self { id: u32::MAX, vertex_shader: u32::MAX, fragment_shader: u32::MAX, vertex_position_location: 0, vertex_color_location: 0, num_vertex_attributes: 0 }
    }

    fn create_program(&mut self, _f : u32, _v : u32) -> u32 {
        let mut _result : u32 = 0;

        unsafe {
            self.id = vitaGL::glCreateProgram();

            vitaGL::glAttachShader(self.id, _f);
            vitaGL::glAttachShader(self.id, _v);

            self.fragment_shader = _f;
            self.vertex_shader = _v;

            vitaGL::glLinkProgram(self.id);
            vitaGL::glGetProgramiv(self.id, 0x8B82, &mut _result);

            self.vertex_position_location = vitaGL::glGetAttribLocation(self.id, CString::new("position").unwrap().as_ptr()) as u32;
            self.vertex_color_location = vitaGL::glGetAttribLocation(self.id, CString::new("color").unwrap().as_ptr()) as u32;
            self.num_vertex_attributes = if self.vertex_position_location != 0xffff_ffff {
                1
            } else {
                0
            } + if self.vertex_color_location != 0xffff_ffff {
                1
            } else {
                0
            };
        }

        println!("PROG ID: {}", self.id);
        println!("PROG RESULT: {}", _result);

        return _result;
    }
}

fn srgb_to_linear(color: &mut [f32; 4]) {
    for n in &mut color[..3] {
        *n = if *n <= 0.04045 {
            *n / 12.92
        } else {
            f32::powf((*n + 0.055) / 1.055, 2.4)
        };
    }
}

pub struct VitaRenderBackend {
    screen_width: u32,
    screen_height: u32,

    color_program : ShaderProgram,
    bitmap_program : ShaderProgram,
    gradient_program : ShaderProgram,

    shape_tessellator: ShapeTessellator,
    // This is currently unused - we just hold on to it
    // to expose via `get_viewport_dimensions`
    viewport_scale_factor: f64,

    movie_width: u32,
    movie_height: u32,

    canvas: Canvas<Window>
    //window: *mut SDL2::SDL_Window
}

pub fn create_and_compile_shader(_shader: &mut u32, _count: i32, _type: u32, _string: *const *const i8, _length: i32) -> u32 {
    let mut result : u32 = 0;

    unsafe {
        *_shader = vitaGL::glCreateShader(_type);

        vitaGL::glShaderSource(*_shader, _count, _string, std::ptr::null());
        vitaGL::glCompileShader(*_shader);

        vitaGL::glGetShaderiv(*_shader, 0x8B81, &mut result);
    }

    println!("SHADER ID: {}", *_shader);
    println!("SHADER RESULT: {}", result);

    return result;
}

impl VitaRenderBackend {
     pub fn new(c: Canvas<Window>, w: u32, h: u32) -> Self {
        unsafe {
            /*
            let init_return = SDL2::SDL_Init(0x00000020u32);
            println!("SDL2 init: {}", init_return);
            if init_return < 0 {
                println!("SDL ERROR. {}", CStr::from_ptr(SDL2::SDL_GetError()).to_str().expect("to_str() failed :((").to_owned());
            }*/
            let mut instance = Self {
                screen_width: 960,
                screen_height: 544,

                color_program: ShaderProgram::new(),
                bitmap_program: ShaderProgram::new(),
                gradient_program: ShaderProgram::new(),

                shape_tessellator: ShapeTessellator::new(),

                viewport_scale_factor: 1.0,

                movie_width: w,
                movie_height: h,

                canvas: c
                //window: SDL2::SDL_CreateWindow(CString::new("ruffle").expect("CString has failed :(").as_ptr(), 0x2FFF0000u32, 0x2FFF0000u32, 960, 544, 0x00000004)
            };
            instance.canvas.set_logical_size(instance.movie_width, instance.movie_height);
            /*println!("SDL_Window init: {:#?}", instance.window);
            if instance.window == std::ptr::null_mut() {
                println!("SDL ERROR. {}", CStr::from_ptr(SDL2::SDL_GetError()).to_str().expect("to_str() failed :((").to_owned());
            }*/
            instance
        }
    }
    fn build_quad_mesh(&mut self, _program: &ShaderProgram) -> Vec<Draw> {
        todo!()
    }
}

impl Drop for VitaRenderBackend {
    fn drop(&mut self) {
        unsafe {
            //SDL2::SDL_DestroyWindow(self.window);
            //SDL2::SDL_Quit();
        }
    }
}

#[derive(Debug)]
struct Mesh {
    vao_ext: u32,
    draws: Vec<Draw>,
}
impl ShapeHandleImpl for Mesh {}


impl RenderBackend for VitaRenderBackend {
    fn viewport_dimensions(&self) -> ViewportDimensions {
        ViewportDimensions {
            width: 960 as u32,
            height: 544 as u32,
            scale_factor: 1.0,
        }
    }
    fn set_viewport_dimensions(&mut self, _: ViewportDimensions) {
    }
    fn register_shape(&mut self, _shape: DistilledShape, _bitmap_source: &dyn BitmapSource,) -> ShapeHandle {
        let mesh = Mesh {vao_ext: 0, draws: Vec::new()};
        ShapeHandle(Arc::new(mesh))
    }
    fn render_offscreen(&mut self, _: BitmapHandle, _: CommandList, _: StageQuality, _: PixelRegion) -> Option<Box<(dyn SyncHandle + 'static)>> {
        todo!()
    }

    fn submit_frame(&mut self, _clear: SwfColor, _commands: CommandList, cache_entries: Vec<BitmapCacheEntry>,) {
        //println!("rendering frame");
        self.canvas.set_draw_color(Color::RGB(_clear.r, _clear.g, _clear.b));
        self.canvas.clear();
        self.canvas.present();
    }
    fn create_empty_texture(&mut self, _: u32, _: u32) -> Result<BitmapHandle, BitmapError> {
        todo!()
    }
    fn register_bitmap(&mut self, _: ruffle_render::bitmap::Bitmap) -> Result<BitmapHandle, BitmapError> {
        todo!()
    }
    fn update_texture(&mut self, _: &BitmapHandle, _: ruffle_render::bitmap::Bitmap, _: PixelRegion) -> Result<(), BitmapError> {
        todo!()
    }
    fn create_context3d(&mut self, _profile: Context3DProfile,) -> Result<Box<dyn Context3D>, BitmapError> {
        Err(BitmapError::Unimplemented("createContext3D".into()))
    }
    fn context3d_present(&mut self, _: &mut (dyn Context3D + 'static)) -> Result<(), BitmapError> {
        todo!()
    }
    fn debug_info(&self) -> Cow<'static, str> {
        todo!()
    }
    fn name(&self) -> &'static str {
        "vitagl"
    }
    fn set_quality(&mut self, _: StageQuality) {

    }
    fn compile_pixelbender_shader(&mut self, _: PixelBenderShader) -> Result<PixelBenderShaderHandle, BitmapError> {
        todo!()
    }
    fn run_pixelbender_shader(&mut self, _shader: PixelBenderShaderHandle, _arguments: &[PixelBenderShaderArgument], _target: &PixelBenderTarget,) -> Result<PixelBenderOutput, BitmapError> {
        todo!()
    }
    fn resolve_sync_handle(&mut self, _handle: Box<dyn SyncHandle>, _with_rgba: RgbaBufRead,) -> Result<(), BitmapError> {
        todo!()
    }
}