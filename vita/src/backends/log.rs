// Import the required trait
use ruffle_core::backend::log::LogBackend;
/// Logging backend that just reroutes traces to the log crate
pub struct VitaLogBackend {}

impl VitaLogBackend {
    pub fn new() -> Self {
        Self {}
    }
}

// Implement your own LogBackend trait (if needed)
impl LogBackend for VitaLogBackend {
    fn avm_trace(&self, message: &str) {
        println!("[avm_trace] {}", message);
    }
}

impl Default for VitaLogBackend {
    fn default() -> Self {
        VitaLogBackend::new()
    }
}