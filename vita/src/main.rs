mod backends;

use crate::backends::VitaRenderBackend;
use anyhow::Result;

use std::time::Duration;
use std::time::Instant;

use std::sync::Arc;
use std::sync::Mutex;
use ruffle_core::PlayerBuilder;
use std::fs::read;

use ruffle_core::tag_utils::SwfMovie;
use ruffle_core::Player;

use crate::backends::{VitaLogBackend};

use sdl2::keyboard::Keycode;
use sdl2::mouse::MouseButton;
use sdl2::pixels::Color;
use sdl2::rect::{Point, Rect};
use sdl2::render::{Canvas, Texture, TextureCreator};
use sdl2::video::{Window, WindowContext};
use sdl2::{controller::Button, event::Event};

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let _file_bytes = read("ux0:data/ruffle/game.swf").unwrap();
    let _swf : SwfMovie = SwfMovie::from_data(&_file_bytes, "ux0:data/ruffle/game.swf".to_string(), None).unwrap();


    let window = video_subsystem
        .window(
            "ruffle-vita",
            960,
            544,
        )
        .opengl()
        .build()
        .unwrap();

    let _gl_context = window.gl_create_context().unwrap();
    let _gl = gl::load_with(|s| video_subsystem.gl_get_proc_address(s) as *const std::os::raw::c_void);

    unsafe {
        gl::ClearColor(0.3, 0.3, 0.5, 1.0);
    }
    /*let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
        backends: wgpu::Backends::GL,
        ..Default::default()
    });
    let (adapter, device, queue) = futures::executor::block_on(request_adapter_and_device(
        wgpu::Backends::GL,
        &instance,
        None,
        wgpu::PowerPreference::HighPerformance,
        None,
    ))
    .map_err(|e| anyhow!(e.to_string()))?;

    let descriptors = Arc::new(Descriptors::new(instance, adapter, device, queue));
    let target = TextureTarget::new(&descriptors.device, (960, 544))
        .map_err(|e| anyhow!(e.to_string()))?;*/

    let _player : Arc<Mutex<Player>> = PlayerBuilder::new()
        .with_log(VitaLogBackend::new())
        .with_renderer(VitaRenderBackend::new(window))
        .with_movie(_swf.clone())
        .with_autoplay(true)
        .build();

    println!("movie_width: {}, movie_height: {}", _swf.width().to_pixels(), _swf.height().to_pixels());

    let target_fps: u32 = 60;
    let frame_duration: Duration = Duration::from_secs_f32(1.0 / target_fps as f32);
    
    let mut last_time = Instant::now();

    'main: loop {
        let frame_start_time = Instant::now();

        // Calculate delta time
        let delta_time = frame_start_time.duration_since(last_time).as_micros();
        last_time = frame_start_time;

        _player.lock().unwrap().tick(delta_time as f64 / 1000.0);

        if let Some(value) = _player.lock().unwrap().current_frame() {
            //println!("The current frame is {}", value);
        }
        _player.lock().unwrap().render();

        let elapsed = frame_start_time.duration_since(last_time);
        let sleep_duration = frame_duration.saturating_sub(elapsed);

        // Sleep to maintain target frame rate
        std::thread::sleep(sleep_duration);
    }
}