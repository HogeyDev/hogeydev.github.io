use crate::ir::VReg;

pub struct StackAllocator {
    pub offsets: Vec<usize>,
    pub frame_size: usize,
}

impl StackAllocator {
    pub fn new() -> Self {
        Self { offsets: vec![], frame_size: 0 }
    }

    pub fn allocate(&mut self, num_vregs: usize) {
        self.offsets = (0..num_vregs).map(|i| (i + 1) * 8).collect();
        self.frame_size = ((num_vregs * 8) + 15) & !15;
    }

    pub fn offset(&self, vreg: VReg) -> usize {
        self.offsets[vreg]
    }
}
