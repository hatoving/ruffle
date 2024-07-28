#![deny(clippy::unwrap_used)]
// Remove this when we start using `Rc` when compiling for wasm
#![allow(clippy::arc_with_non_send_sync)]

use ruffle_render::bitmap::BitmapHandleImpl;
use ruffle_render::transform::Transform;
use ruffle_render::bitmap::PixelSnapping;
use bytemuck::Pod;
use bytemuck::Zeroable;
use thiserror::Error;
use std::ffi::CStr;
use ruffle_render::backend::ShapeHandleImpl;
use ruffle_render::tessellator::Mesh as OtherMesh;
use std::sync::Arc;
use std::ffi::c_void;
use ruffle_render::bitmap::RgbaBufRead;
use ruffle_render::commands::{CommandHandler, CommandList, RenderBlendMode};
use ruffle_render::backend::RenderBackend;
use ruffle_core::ViewportDimensions;
use ruffle_render::bitmap::BitmapSource;
use ruffle_render::backend::ShapeHandle;
use ruffle_render::bitmap::BitmapHandle;
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
    Gradient as TessGradient, ShapeTessellator, Vertex as TessVertex
};
use swf::Color as SwfColor;
use swf::BlendMode;
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

#[link(name = "vitaGL", kind = "static")]
#[link(name = "SDL2", kind = "static")]
#[link(name = "vitashark", kind = "static")]
#[link(name = "SceShaccCgExt", kind = "static")]
#[link(name = "mathneon", kind = "static")]
#[link(name = "taihen_stub", kind = "static")]
#[link(name = "SceAppMgr_stub", kind = "static")]
#[link(name = "SceAudio_stub", kind = "static")]
#[link(name = "SceAudioIn_stub", kind = "static")]
#[link(name = "SceCtrl_stub", kind = "static")]
#[link(name = "SceTouch_stub", kind = "static")]
#[link(name = "SceMotion_stub", kind = "static")]
#[link(name = "SceCommonDialog_stub", kind = "static")]
#[link(name = "SceDisplay_stub", kind = "static")]
#[link(name = "SceKernelDmacMgr_stub", kind = "static")]
#[link(name = "SceGxm_stub", kind = "static")]
#[link(name = "SceShaccCg_stub", kind = "static")]
#[link(name = "SceIme_stub", kind = "static")]
#[link(name = "SceHid_stub", kind = "static")]
extern "C" {}


#[derive(Error, Debug)]
pub enum Error {
    #[error("Couldn't create GL context")]
    CantCreateGLContext,

    #[error("Couldn't create frame buffer")]
    UnableToCreateFrameBuffer,

    #[error("Couldn't create program")]
    UnableToCreateProgram,

    #[error("Couldn't create texture")]
    UnableToCreateTexture,

    #[error("Couldn't compile shader")]
    UnableToCreateShader,

    #[error("Couldn't create render buffer")]
    UnableToCreateRenderBuffer,

    #[error("Couldn't create vertex array object")]
    UnableToCreateVAO,

    #[error("Couldn't create buffer")]
    UnableToCreateBuffer,

    #[error("OES_element_index_uint extension not available")]
    OESExtensionNotFound,

    #[error("VAO extension not found")]
    VAOExtensionNotFound,

    #[error("Couldn't link shader program: {0}")]
    LinkingShaderProgram(String),

    #[error("GL Error in {0}: {1}")]
    GLError(&'static str, u32),
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
    vertex_buffer: u32,
    index_buffer: u32,
    vao: u32,
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

    fn create_program(&mut self, _f : u32, _v : u32) -> i32 {
        let mut _result : i32 = 0;

        unsafe {
            self.id = gl::CreateProgram();

            gl::AttachShader(self.id, _f);
            gl::AttachShader(self.id, _v);

            self.fragment_shader = _f;
            self.vertex_shader = _v;

            gl::LinkProgram(self.id);
            gl::GetProgramiv(self.id, 0x8B82, &mut _result);

            self.vertex_position_location = gl::GetAttribLocation(self.id, CString::new("position").unwrap().as_ptr()) as u32;
            self.vertex_color_location = gl::GetAttribLocation(self.id, CString::new("color").unwrap().as_ptr()) as u32;
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

    fn uniform1i(&self, uniform: &str, value: i32) {
        //gl.uniform1i(self.uniforms[uniform as usize].as_ref(), value);
        unsafe {
            gl::ProgramUniform1i(
                self.id,
                gl::GetUniformLocation(self.id, CString::new(uniform).unwrap().as_ptr()),
                value
            );
            let error = unsafe { gl::GetError() };
            if error != gl::NO_ERROR {
                println!("OpenGL Error {}: {:?}", line!(), error);
            }
        }
    }

    fn uniform1f(&self, uniform: &str, value: f32) {
        //gl.uniform1i(self.uniforms[uniform as usize].as_ref(), value);
        unsafe {
            gl::ProgramUniform1f(
                self.id,
                gl::GetUniformLocation(self.id, CString::new(uniform).unwrap().as_ptr()),
                value
            );
            let error = unsafe { gl::GetError() };
            if error != gl::NO_ERROR {
                println!("OpenGL Error {}: {:?}", line!(), error);
            }
        }
    }

    fn uniform1fv(&self, uniform: &str, values: &[f32]) {
        unsafe {
            gl::ProgramUniform1fv(
                self.id,
                gl::GetUniformLocation(self.id, CString::new(uniform).unwrap().as_ptr()),
                1,
                values.as_ptr() as *const f32,
            );
            let error = unsafe { gl::GetError() };
            if error != gl::NO_ERROR {
                println!("OpenGL Error {}: {:?}", line!(), error);
            }
        }
    }


    fn uniform4fv(&self, uniform: &str, values: &[f32]) {
        unsafe {
            gl::ProgramUniform4fv(
                self.id,
                gl::GetUniformLocation(self.id, CString::new(uniform).unwrap().as_ptr()),
                1,
                values.as_ptr() as *const f32,
            );
            let error = unsafe { gl::GetError() };
            if error != gl::NO_ERROR {
                println!("OpenGL Error {}: {:?}", line!(), error);
            }
        }
    }

    fn uniform_matrix3fv(&self, uniform: &str, values: &[[f32; 3]; 3]) {
        unsafe {
            gl::ProgramUniformMatrix3fv(
                self.id,
                gl::GetUniformLocation(self.id, CString::new(uniform).unwrap().as_ptr()),
                1,
                0,
                bytemuck::cast_slice(values).as_ptr() as *const f32,
            );
            let error = unsafe { gl::GetError() };
            if error != gl::NO_ERROR {
                println!("OpenGL Error {}: {:?}", line!(), error);
            }
        }
    }

    fn uniform_matrix4fv(&self, uniform: &str, values: &[[f32; 4]; 4]) {
        unsafe {
            gl::ProgramUniformMatrix4fv(
                self.id,
                gl::GetUniformLocation(self.id, CString::new(uniform).unwrap().as_ptr()),
                1,
                0,
                bytemuck::cast_slice(values).as_ptr() as *const f32,
            );
            let error = unsafe { gl::GetError() };
            if error != gl::NO_ERROR {
                println!("OpenGL Error {}: {:?}", line!(), error);
            }
        }
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

    color_quad_draws: Vec<Draw>,
    bitmap_quad_draws: Vec<Draw>,

    mask_state: MaskState,
    num_masks: u32,
    mask_state_dirty: bool,
    is_transparent: bool,

    active_program: *const ShaderProgram,
    blend_modes: Vec<RenderBlendMode>,
    mult_color: Option<[f32; 4]>,
    add_color: Option<[f32; 4]>,

    renderbuffer_width: i32,
    renderbuffer_height: i32,
    view_matrix: [[f32; 4]; 4],

    window: Window,

    vao: u32
    //window: *mut SDL2::SDL_Window
}

pub fn create_and_compile_shader(_shader: &mut u32, _count: i32, _type: u32, _string: *const *const i8, _length: i32) -> i32 {
    let mut result : i32 = 0;

    unsafe {
        *_shader = gl::CreateShader(_type);

        gl::ShaderSource(*_shader, _count, _string, std::ptr::null());
        gl::CompileShader(*_shader);

        gl::GetShaderiv(*_shader, 0x8B81, &mut result);

    }

    println!("SHADER ID: {}", *_shader);
    println!("SHADER RESULT: {}", result);

    return result;
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
struct Vertex {
    position: [f32; 2],
    color: u32,
}

impl From<TessVertex> for Vertex {
    fn from(vertex: TessVertex) -> Self {
        Self {
            position: [vertex.x, vertex.y],
            color: u32::from_le_bytes([
                vertex.color.r,
                vertex.color.g,
                vertex.color.b,
                vertex.color.a,
            ]),
        }
    }
}

#[derive(Debug)]
struct Buffer {
    buffer: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MaskState {
    NoMask,
    DrawMaskStencil,
    DrawMaskedContent,
    ClearMaskStencil,
}

fn same_blend_mode(first: Option<&RenderBlendMode>, second: &RenderBlendMode) -> bool {
    match (first, second) {
        (Some(RenderBlendMode::Builtin(old)), RenderBlendMode::Builtin(new)) => old == new,
        _ => false,
    }
}

impl VitaRenderBackend {
    fn apply_blend_mode(&mut self, mode: RenderBlendMode) {
        let (blend_op, src_rgb, dst_rgb) = match mode {
            RenderBlendMode::Builtin(BlendMode::Normal) => {
                // src + (1-a)
                (gl::FUNC_ADD, gl::ONE, gl::ONE_MINUS_SRC_ALPHA)
            }
            RenderBlendMode::Builtin(BlendMode::Add) => {
                // src + dst
                (gl::FUNC_ADD, gl::ONE, gl::ONE)
            }
            RenderBlendMode::Builtin(BlendMode::Subtract) => {
                // dst - src
                (gl::FUNC_REVERSE_SUBTRACT, gl::ONE, gl::ONE)
            }
            _ => {
                // TODO: Unsupported blend mode. Default to normal for now.
                (gl::FUNC_ADD, gl::ONE, gl::ONE_MINUS_SRC_ALPHA)
            }
        };
        unsafe {
            gl::BlendEquationSeparate(blend_op, gl::FUNC_ADD);
            gl::BlendFuncSeparate(src_rgb, dst_rgb, gl::ONE, gl::ONE_MINUS_SRC_ALPHA);
        }
    }

    fn push_blend_mode(&mut self, blend: RenderBlendMode) {
        if !same_blend_mode(self.blend_modes.last(), &blend) {
            self.apply_blend_mode(blend.clone());
        }
        self.blend_modes.push(blend);
    }

    pub fn new(_window: Window) -> Self {
        unsafe {
            /*
            let init_return = SDL2::SDL_Init(0x00000020u32);
            println!("SDL2 init: {}", init_return);
            if init_return < 0 {
                println!("SDL ERROR. {}", CStr::from_ptr(SDL2::SDL_GetError()).to_str().expect("to_str() failed :((").to_owned());
            }*/

            let mut color_vert_shader : u32 = 0;
            let mut color_frag_shader : u32 = 0;

            let mut tex_vert_shader : u32 = 0;

            let mut gradient_frag_shader : u32 = 0;
            let mut bitmap_frag_shader : u32 = 0;

            create_and_compile_shader(&mut color_vert_shader, 1, gl::VERTEX_SHADER, [CString::new(COLOR_VERTEX_GLSL).unwrap().as_ptr()].as_ptr(), 0);
            create_and_compile_shader(&mut color_frag_shader, 1, gl::FRAGMENT_SHADER, [CString::new(COLOR_FRAGMENT_GLSL).unwrap().as_ptr()].as_ptr(), 0);
            create_and_compile_shader(&mut tex_vert_shader, 1, gl::VERTEX_SHADER, [CString::new(TEXTURE_VERTEX_GLSL).unwrap().as_ptr()].as_ptr(), 0);
            create_and_compile_shader(&mut gradient_frag_shader, 1, gl::FRAGMENT_SHADER, [CString::new(GRADIENT_FRAGMENT_GLSL).unwrap().as_ptr()].as_ptr(), 0);
            create_and_compile_shader(&mut bitmap_frag_shader, 1, gl::FRAGMENT_SHADER, [CString::new(BITMAP_FRAGMENT_GLSL).unwrap().as_ptr()].as_ptr(), 0);

            let mut instance = Self {
                screen_width: 960,
                screen_height: 544,

                color_program: ShaderProgram::new(),
                bitmap_program: ShaderProgram::new(),
                gradient_program: ShaderProgram::new(),

                shape_tessellator: ShapeTessellator::new(),

                viewport_scale_factor: 1.0,

                color_quad_draws: vec![],
                bitmap_quad_draws: vec![],
                view_matrix: [[0.0; 4]; 4],

                mask_state: MaskState::NoMask,
                num_masks: 0,
                mask_state_dirty: true,
                is_transparent: false,

                active_program: std::ptr::null(),
                blend_modes: vec![],
                mult_color: None,
                add_color: None,

                renderbuffer_width: 1,
                renderbuffer_height: 1,

                window: _window,

                vao: 0
                //window: SDL2::SDL_CreateWindow(CString::new("ruffle").expect("CString has failed :(").as_ptr(), 0x2FFF0000u32, 0x2FFF0000u32, 960, 544, 0x00000004)
            };

            instance.color_program.create_program(color_frag_shader, color_vert_shader);
            instance.bitmap_program.create_program(bitmap_frag_shader, tex_vert_shader);
            instance.gradient_program.create_program(gradient_frag_shader, tex_vert_shader);

            gl::Enable(gl::BLEND);
            gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);

            instance.push_blend_mode(RenderBlendMode::Builtin(BlendMode::Normal));

            //let bitmap_programd = instance.bitmap_program;
            // Use the clone for the immutable borrow
            // Immutable borrows
            let color_program = instance.color_program.clone();
            let bitmap_program = instance.bitmap_program.clone();

            // Mutably borrow `instance` to call `build_quad_mesh`
            let mut instance = instance; // Make sure instance is mutable here

            let mut color_quad_mesh = instance.build_quad_mesh(&color_program);
            let mut bitmap_quad_mesh = instance.build_quad_mesh(&bitmap_program);

            instance.color_quad_draws.append(&mut color_quad_mesh);
            instance.bitmap_quad_draws.append(&mut bitmap_quad_mesh);

            instance.set_viewport_dimensions(ViewportDimensions {
                width: 1,
                height: 1,
                scale_factor: 1.0,
            });
            /*println!("SDL_Window init: {:#?}", instance.window);
            if instance.window == std::ptr::null_mut() {
                println!("SDL ERROR. {}", CStr::from_ptr(SDL2::SDL_GetError()).to_str().expect("to_str() failed :((").to_owned());
            }*/
            instance
        }
    }

    fn build_quad_mesh(&mut self, _program: &ShaderProgram) -> Vec<Draw> {
        let _vao = self.create_vertex_array();
        let _vertex_buf = self.create_buffer();

        unsafe {
            gl::BindBuffer(gl::ARRAY_BUFFER, _vertex_buf);
            let vertex_data: &[u8] = bytemuck::cast_slice(&[
                Vertex {
                    position: [0.0, 0.0],
                    color: 0xffff_ffff,
                },
                Vertex {
                    position: [1.0, 0.0],
                    color: 0xffff_ffff,
                },
                Vertex {
                    position: [1.0, 1.0],
                    color: 0xffff_ffff,
                },
                Vertex {
                    position: [0.0, 1.0],
                    color: 0xffff_ffff,
                },
            ]);

            unsafe {
                gl::BufferData(
                    gl::ARRAY_BUFFER,
                    vertex_data.len() as gl::types::GLsizeiptr,
                    vertex_data.as_ptr() as *const gl::types::GLvoid,
                    gl::STATIC_DRAW,
                );
            }

            let index_buffer = self.create_buffer();
            
            unsafe {
                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, index_buffer);
            }

            let indexes = [0u32, 1, 2, 3];
            //let mut index_data = Vec::with_capacity(indexes.len() * mem::size_of::<u32>());
            let index_data : &[u8] = bytemuck::cast_slice(&[0u32, 1, 2, 3]);

            unsafe {
                gl::BufferData(
                    gl::ELEMENT_ARRAY_BUFFER,
                    index_data.len() as gl::types::GLsizeiptr,
                    index_data.as_ptr() as *const gl::types::GLvoid,
                    gl::STATIC_DRAW,
                );
            }

            if _program.vertex_position_location != 0xffff_ffff {
                unsafe {
                    gl::VertexAttribPointer(
                        _program.vertex_position_location,
                        2,
                        gl::FLOAT,
                        0,
                        12,
                        std::ptr::null(),
                    );
                    gl::EnableVertexAttribArray(_program.vertex_position_location);
                }
                //gl
                    //.enable_vertex_attrib_array(program.vertex_position_location);
            }

            if _program.vertex_color_location != 0xffff_ffff {
                unsafe {
                    gl::VertexAttribPointer(
                        _program.vertex_color_location,
                        4,
                        gl::UNSIGNED_BYTE,
                        1,
                        12,
                        8 as *const c_void,
                    );
                    gl::EnableVertexAttribArray(_program.vertex_color_location);
                }
                //gl
                    //.enable_vertex_attrib_array(program.vertex_position_location);
            }

            self.bind_vertex_array(0);

            for i in _program.num_vertex_attributes..NUM_VERTEX_ATTRIBUTES {
                unsafe {
                    gl::DisableVertexAttribArray(i);
                }
            }

            let mut draws = vec![];
            draws.push(Draw {
                draw_type: if _program.fragment_shader == self.bitmap_program.fragment_shader {
                    DrawType::Bitmap(BitmapDraw {
                        matrix: [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]],
                        handle: None,
                        is_smoothed: true,
                        is_repeating: false,
                    })
                } else {
                    DrawType::Color
                },
                vao: _vao,
                vertex_buffer: _vertex_buf,
                index_buffer: index_buffer,
                num_indices: 4,
                num_mask_indices: 4,
            });

            draws
        }
    }

    fn create_vertex_array(&mut self) -> u32 {
        let mut vao : u32 = 0;
        
        unsafe {
            gl::GenVertexArrays(1, &mut vao);
            gl::BindVertexArray(vao);
        }
        println!("glGenVertexArrays: {}", vao);
        self.vao = vao;
        vao
    }

    fn bind_vertex_array(&mut self, vao: u32) {
        unsafe {
            gl::BindVertexArray(vao);
        }
        println!("glBindVertexArray: {}", vao);
        self.vao = vao;
    }

    fn set_stencil_state(&mut self) {
        // Set stencil state for masking, if necessary.
        if self.mask_state_dirty {
            unsafe {
                match self.mask_state {
                    MaskState::NoMask => {
                        gl::Disable(gl::STENCIL_TEST);
                        gl::ColorMask(1, 1, 1, 1);
                    }
                    MaskState::DrawMaskStencil => {
                        gl::Enable(gl::STENCIL_TEST);
                        gl::StencilFunc(gl::EQUAL, (self.num_masks - 1) as i32, 0xff);
                        gl::StencilOp(gl::KEEP, gl::KEEP, gl::INCR);
                        gl::ColorMask(0, 0, 0, 0);
                    }
                    MaskState::DrawMaskedContent => {
                        gl::Enable(gl::STENCIL_TEST);
                        gl::StencilFunc(gl::EQUAL, self.num_masks as i32, 0xff);
                        gl::StencilOp(gl::KEEP, gl::KEEP, gl::KEEP);
                        gl::ColorMask(1, 1, 1, 1);
                    }
                    MaskState::ClearMaskStencil => {
                        gl::Enable(gl::STENCIL_TEST);
                        gl::StencilFunc(gl::EQUAL, self.num_masks as i32, 0xff);
                        gl::StencilOp(gl::KEEP, gl::KEEP, gl::DECR);
                        gl::ColorMask(0, 0, 0, 0);
                    }
                }
            }
        }
    }

    fn create_buffer(&mut self) -> u32 {
        let mut buf = 0;
        
        unsafe {
            gl::GenBuffers(1, &mut buf);
        }
        println!("glGenBuffers: {}", buf);
        buf
    }

    fn register_shape_internal(&mut self, shape: DistilledShape, bitmap_source: &dyn BitmapSource) -> Result<Vec<Draw>, Error> {
        use ruffle_render::tessellator::DrawType as TessDrawType;

        let lyon_mesh = self
            .shape_tessellator
            .tessellate_shape(shape, bitmap_source);

        let mut draws: Vec<Draw> = Vec::with_capacity(lyon_mesh.draws.len());
        for draw in lyon_mesh.draws {
            let num_indices = draw.indices.len() as i32;
            let num_mask_indices = draw.mask_index_count as i32;
            println!("num_indices: {}", num_indices);

            let _vao = self.create_vertex_array();
            let vertex_buffer = self.create_buffer();
            let error = unsafe { gl::GetError() };
            unsafe {
                gl::BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
            }

            let vertices: Vec<_> = draw.vertices.into_iter().map(Vertex::from).collect();
            let vertex_data: &[u8] = bytemuck::cast_slice(&vertices);
            unsafe {
                gl::BufferData(
                    gl::ARRAY_BUFFER,
                    vertex_data.len() as gl::types::GLsizeiptr,
                    vertex_data.as_ptr() as *const gl::types::GLvoid,
                    gl::STATIC_DRAW,
                );
            }

            let index_buffer = self.create_buffer();
            unsafe {
                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, index_buffer);
            }

            //let vertices: Vec<_> = draw.vertices.into_iter().map(|arg0: ruffle_render::tessellator::Vertex| Vertex::from).collect();
            let indices_data: &[u8] = bytemuck::cast_slice(&draw.indices);
            unsafe {
                gl::BufferData(
                    gl::ELEMENT_ARRAY_BUFFER,
                    indices_data.len() as gl::types::GLsizeiptr,
                    indices_data.as_ptr() as *const gl::types::GLvoid,
                    gl::STATIC_DRAW,
                );
            }

            let program = match draw.draw_type {
                TessDrawType::Color => &self.color_program,
                TessDrawType::Gradient { .. } => &self.gradient_program,
                TessDrawType::Bitmap(_) => &self.bitmap_program,
            };

            // Unfortunately it doesn't seem to be possible to ensure that vertex attributes will be in
            // a guaranteed position between shaders in WebGL1 (no layout qualifiers in GLSL in OpenGL ES 1.0).
            // Attributes can change between shaders, even if the vertex layout is otherwise "the same".
            // This varies between platforms based on what the GLSL compiler decides to do.
            unsafe {
                if program.vertex_position_location != 0xffff_ffff {
                    gl::VertexAttribPointer(
                        program.vertex_position_location,
                        2,
                        gl::FLOAT,
                        0,
                        12,
                        std::ptr::null(),
                    );
                    gl::EnableVertexAttribArray(program.vertex_position_location);
                }

                if program.vertex_color_location != 0xffff_ffff {
                    gl::VertexAttribPointer(
                        program.vertex_color_location,
                        4,
                        gl::UNSIGNED_BYTE,
                        1,
                        12,
                        8 as *const c_void,
                    );
                    gl::EnableVertexAttribArray(program.vertex_color_location);
                }

            }

            let num_vertex_attributes = program.num_vertex_attributes;

            draws.push(match draw.draw_type {
                TessDrawType::Color => Draw {
                    draw_type: DrawType::Color,
                    vao: _vao,
                    vertex_buffer: vertex_buffer,
                    index_buffer: index_buffer,
                    num_indices: num_indices,
                    num_mask_indices: num_mask_indices,
                },
                TessDrawType::Gradient { matrix, gradient } => Draw {
                    draw_type: DrawType::Gradient(Box::new(Gradient::new(
                        lyon_mesh.gradients[gradient].clone(), // TODO: Gradient deduplication
                        matrix,
                    ))),
                    vao: _vao,
                    vertex_buffer: vertex_buffer,
                    index_buffer: index_buffer,
                    num_indices: num_indices,
                    num_mask_indices: num_mask_indices,
                },
                TessDrawType::Bitmap(bitmap) => Draw {
                    draw_type: DrawType::Bitmap(BitmapDraw {
                        matrix: bitmap.matrix,
                        handle: bitmap_source.bitmap_handle(bitmap.bitmap_id, self),
                        is_smoothed: bitmap.is_smoothed,
                        is_repeating: bitmap.is_repeating,
                    }),
                    vao: _vao,
                    vertex_buffer: vertex_buffer,
                    index_buffer: index_buffer,
                    num_indices: num_indices,
                    num_mask_indices: num_mask_indices,
                },
            });

            self.bind_vertex_array(0);

            for i in num_vertex_attributes..NUM_VERTEX_ATTRIBUTES {
                unsafe {
                    gl::DisableVertexAttribArray(i);
                    let error = unsafe { gl::GetError() };
                    if error != gl::NO_ERROR {
                        println!("OpenGL Error {}: {:?}", line!(), error);
                    }

                }
            }
        }
        Ok(draws)
    }
    fn begin_frame(&mut self, clear: SwfColor) {
        self.active_program = std::ptr::null();
        self.mask_state = MaskState::NoMask;
        self.num_masks = 0;
        self.mask_state_dirty = true;

        self.mult_color = None;
        self.add_color = None;

        unsafe {
            //gl::Viewport(0, 0, self.renderbuffer_width, self.renderbuffer_height);
        }

        self.set_stencil_state();
        unsafe {
            if self.is_transparent {
                gl::ClearColor(0.0, 0.0, 0.0, 0.0);
            } else {
                gl::ClearColor(
                    clear.r as f32 / 255.0,
                    clear.g as f32 / 255.0,
                    clear.b as f32 / 255.0,
                    clear.a as f32 / 255.0,
                );
            }
            gl::StencilMask(0xff);
            gl::Clear(gl::COLOR_BUFFER_BIT | gl::STENCIL_BUFFER_BIT);
        }
    }
    fn end_frame(&mut self) {   
        unsafe {
            self.window.gl_swap_window();
        }
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
            width: self.renderbuffer_width as u32,
            height: self.renderbuffer_height as u32,
            scale_factor: self.viewport_scale_factor,
        }
    }
    fn set_viewport_dimensions(&mut self, dimensions: ViewportDimensions){
        println!("Setting dimenstions to: {}, {}", dimensions.width, dimensions.height);
        // Build view matrix based on canvas size.
        self.view_matrix = [
            [1.0 / (dimensions.width as f32 / 2.0), 0.0, 0.0, 0.0],
            [0.0, -1.0 / (dimensions.height as f32 / 2.0), 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [-1.0, 1.0, 0.0, 1.0],
        ];

        // Setup GL viewport and renderbuffers clamped to reasonable sizes.
        // We don't use `.clamp()` here because `gl.drawing_buffer_width()` and
        // `gl.drawing_buffer_height()` return zero when the WebGL context is lost,
        // then an assertion error would be triggered.
        self.renderbuffer_width =
            (dimensions.width.max(1) as i32).min(960);
        self.renderbuffer_height =
            (dimensions.height.max(1) as i32).min(544);

        println!("Setting renderbuffer to: {}, {}", self.renderbuffer_width, self.renderbuffer_height);

        //let _ = self.build_msaa_buffers();
        unsafe {
        	self.window.set_size(self.renderbuffer_width as u32, self.renderbuffer_height as u32);
            gl::Viewport(0, 0, self.renderbuffer_width, self.renderbuffer_height);
            self.viewport_scale_factor = dimensions.scale_factor;
        }
    }
    fn register_shape(&mut self, _shape: DistilledShape, _bitmap_source: &dyn BitmapSource,) -> ShapeHandle {
        //let mesh = Mesh {vao_ext: 0, draws: Vec::new()};
        println!("Registering shape");
        let mesh = match self.register_shape_internal(_shape, _bitmap_source) {
            Ok(draws) => Mesh {
                draws,
                vao_ext: self.vao,
            },
            Err(e) => {
                println!("Couldn't register shape: {}", e);
                Mesh {
                    draws: vec![],
                    vao_ext: self.vao,
                }
            }
        };
        ShapeHandle(Arc::new(mesh))
    }
    fn render_offscreen(&mut self, _: BitmapHandle, _: CommandList, _: StageQuality, _: PixelRegion) -> Option<Box<(dyn SyncHandle + 'static)>> {
        todo!()
    }

    fn submit_frame(&mut self, _clear: SwfColor, _commands: CommandList, cache_entries: Vec<BitmapCacheEntry>,) {
        //println!("rendering frame");
        unsafe {
            //println!("{}, {}, {}", _clear.r, _clear.g, _clear.b);
            //gl::ClearColor((_clear.r / 255) as f32, (_clear.g / 255) as f32, (_clear.b / 255) as f32, 1.0);
            //gl::Clear(gl::COLOR_BUFFER_BIT);
        }
        self.begin_frame(_clear);
        _commands.execute(self);
        self.end_frame();

        //self.end_frame();
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

fn as_mesh(handle: &ShapeHandle) -> &Mesh {
    <dyn ShapeHandleImpl>::downcast_ref(&*handle.0).expect("Shape handle must be a WebGL ShapeData")
}

#[derive(Debug)]
struct RegistryData {
    width: u32,
    height: u32,
    texture: u32,
}

impl Drop for RegistryData {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteTextures(1, self.texture as *const u32);
        }
    }
}

impl BitmapHandleImpl for RegistryData {}

fn as_registry_data(handle: &BitmapHandle) -> &RegistryData {
    <dyn BitmapHandleImpl>::downcast_ref(&*handle.0)
        .expect("Bitmap handle must be webgl RegistryData")
}

impl CommandHandler for VitaRenderBackend {
    fn render_bitmap(&mut self, _: BitmapHandle, _: Transform, _: bool, _: PixelSnapping) { todo!() }
    fn render_stage3d(&mut self, _: BitmapHandle, _: Transform) { todo!() }
    fn render_shape(&mut self, shape: ShapeHandle, transform: Transform) {
        println!("rendering shape: {}, {}", transform.matrix.tx.to_pixels() as f32, transform.matrix.ty.to_pixels() as f32);
        
        let world_matrix = [
            [transform.matrix.a, transform.matrix.b, 0.0, 0.0],
            [transform.matrix.c, transform.matrix.d, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [
                transform.matrix.tx.to_pixels() as f32,
                transform.matrix.ty.to_pixels() as f32,
                0.0,
                1.0,
            ],
        ];

        let mult_color = transform.color_transform.mult_rgba_normalized();
        let add_color = transform.color_transform.add_rgba_normalized();

        self.set_stencil_state();

        let mut error = 0;

        let mesh = as_mesh(&shape);
        for draw in &mesh.draws {
            // Ignore strokes when drawing a mask stencil.
            let num_indices = if self.mask_state != MaskState::DrawMaskStencil
                && self.mask_state != MaskState::ClearMaskStencil
            {
                draw.num_indices
            } else {
                draw.num_mask_indices
            };
            if num_indices == 0 {
                continue;
            }

            self.bind_vertex_array(draw.vao);
            error = unsafe { gl::GetError() };
            if error != gl::NO_ERROR {
                println!("OpenGL Error {}: {:?}", line!(), error);
            }

            let program = match &draw.draw_type {
                DrawType::Color => &self.color_program,
                DrawType::Gradient(_) => &self.gradient_program,
                DrawType::Bitmap { .. } => &self.bitmap_program,
            };

            if program as *const ShaderProgram != self.active_program {
                unsafe {
                    gl::UseProgram(program.id);
                }
                error = unsafe { gl::GetError() };
                if error != gl::NO_ERROR {
                    println!("OpenGL Error {}: {:?}", line!(), error);
                }

                self.active_program = program as *const ShaderProgram;

                program.uniform_matrix4fv("view_matrix", &self.view_matrix);

                self.mult_color = None;
                self.add_color = None;
            }

            program.uniform_matrix4fv("world_matrix", &world_matrix);
            if Some(mult_color) != self.mult_color {
                program.uniform4fv("mult_color", &mult_color);
                self.mult_color = Some(mult_color);
            }
            if Some(add_color) != self.add_color {
                program.uniform4fv("add_color", &add_color);
                self.add_color = Some(add_color);
            }

            //println!("drawtype color {:?}", &draw.draw_type);
            match &draw.draw_type {
                DrawType::Color => (),
                DrawType::Gradient(_) | DrawType::Bitmap(_) => todo!(),
            }

            error = unsafe { gl::GetError() };
            if error != gl::NO_ERROR {
                println!("OpenGL Error {}: {:?}", line!(), error);
            }

            unsafe {
                gl::DrawElements(gl::TRIANGLES, num_indices, gl::UNSIGNED_INT, std::ptr::null());
            }
        }
    }
    fn draw_rect(&mut self, _: ruffle_core::Color, _: ruffle_render::matrix::Matrix) { todo!() }
    fn draw_line(&mut self, _: ruffle_core::Color, _: ruffle_render::matrix::Matrix) { todo!() }
    fn draw_line_rect(&mut self, _: ruffle_core::Color, _: ruffle_render::matrix::Matrix) { todo!() }
    fn push_mask(&mut self) { todo!() }
    fn activate_mask(&mut self) { todo!() }
    fn deactivate_mask(&mut self) { todo!() }
    fn pop_mask(&mut self) { todo!() }
    fn blend(&mut self, _: CommandList, _: RenderBlendMode) { todo!() }
}