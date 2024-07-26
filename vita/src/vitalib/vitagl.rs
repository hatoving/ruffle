use std::ffi::c_void;
use std::ffi::c_char;

#[link(name = "vitaGL", kind = "static")]
extern "C" {
    pub fn vglInit(legacy_pool_size: u32) -> bool;
    pub fn vglInitExtended(legacy_pool_size: u32, width: u32, height: u32, ram_threshold: u32, msaa: u32) -> bool;

    pub fn glEnable(cap: u32);
    pub fn glPixelStorei(pname: u32, param: i32);

    pub fn glClear(mask: u32);
    pub fn glClearColor(red: f32, green: f32, blue: f32, alpha: f32);

    pub fn glViewport(x: u32, y: u32, width: i32, height: i32);

    pub fn glCreateShader(shader_type: u32) -> u32;
    pub fn glShaderSource(handle: u32, size: i32, string: *const *const c_char, length: *const i32);
    pub fn glCompileShader(shader: u32);
    pub fn glGetShaderiv(shader: u32, pname: u32, params: *mut u32);

    pub fn glCreateProgram() -> u32;
    pub fn glAttachShader(prog: u32, shad: u32);
    pub fn glLinkProgram(prog: u32);
    pub fn glGetProgramiv(prog: u32, pname: u32, params: *mut u32);

    pub fn glGetAttribLocation(prog: u32, name: *const i8) -> i32;
    pub fn glGetUniformLocation(prog: u32, name: *const i8) -> i32;

    pub fn glGenVertexArrays(size: i32, res: *mut u32);
    pub fn glBindVertexArray(array: u32);

    pub fn glGenBuffers(size: i32, buffers: *mut u32);
    pub fn glBindBuffer(target: u32, buffer: u32);
    pub fn glBufferData(target: u32, size: i32, data: *const c_void, usage: u32);

    pub fn glVertexAttribPointer(index: u32, size: i32, _type: u32, normalized: u8, stride: i32, pointer: *const c_void);
    pub fn glEnableVertexAttribArray(index: u32);
    pub fn glDisableVertexAttribArray(index: u32);

    pub fn glBlendEquationSeparate(mode_rgb: u32, mode_alpha: u32);
    pub fn glBlendFuncSeparate(src_rgb: u32, dst_rgb: u32, src_alpha: u32, dst_alpha: u32);
}