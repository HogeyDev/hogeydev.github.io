use crate::ir::*;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PhysReg {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

pub const ALL_REGS: &[PhysReg] = &[
    PhysReg::Rax,
    PhysReg::Rcx,
    PhysReg::Rdx,
    PhysReg::Rbx,
    PhysReg::Rsi,
    PhysReg::Rdi,
    PhysReg::R8,
    PhysReg::R9,
    PhysReg::R10,
    PhysReg::R11,
    PhysReg::R12,
    PhysReg::R13,
    PhysReg::R14,
    PhysReg::R15,
];

pub const CALLEE_SAVED: &[PhysReg] = &[
    PhysReg::Rbx,
    PhysReg::R12,
    PhysReg::R13,
    PhysReg::R14,
    PhysReg::R15,
];

pub const ARG_REGS: &[PhysReg] = &[
    PhysReg::Rdi,
    PhysReg::Rsi,
    PhysReg::Rdx,
    PhysReg::Rcx,
    PhysReg::R8,
    PhysReg::R9,
];

#[derive(Clone, Debug)]
pub struct LiveInterval {
    pub vreg: VReg,
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Debug)]
pub struct Allocation {
    pub reg_map: HashMap<VReg, PhysReg>,
    pub spill_slots: HashMap<VReg, usize>,
    pub stack_size: usize,
}

pub struct RegAllocator;

impl RegAllocator {
    pub fn new() -> Self {
        RegAllocator
    }

    pub fn allocate(&self, func: &IrFunction) -> Allocation {
        let intervals = self.compute_live_intervals(func);
        let mut sorted: Vec<LiveInterval> = intervals.clone();
        sorted.sort_by_key(|i| i.start);

        let mut reg_map: HashMap<VReg, PhysReg> = HashMap::new();
        let mut spill_slots: HashMap<VReg, usize> = HashMap::new();
        let mut next_spill_slot: usize = 0;

        let mut active: Vec<(usize, VReg, PhysReg)> = Vec::new();

        for interval in &sorted {
            self.expire_intervals(interval.start, &mut active, &mut reg_map);

            if active.len() < ALL_REGS.len() {
                let reg = ALL_REGS[active.len()];
                reg_map.insert(interval.vreg, reg);
                active.push((interval.end, interval.vreg, reg));
            } else {
                let spill_candidate = active
                    .iter()
                    .max_by_key(|(end, _, _)| *end)
                    .unwrap()
                    .1;
                let candidate_interval = intervals.iter().find(|i| i.vreg == spill_candidate).unwrap();

                if candidate_interval.end > interval.end {
                    // Spill candidate (farther end) instead
                    let reg = reg_map[&spill_candidate];
                    reg_map.remove(&spill_candidate);
                    let slot = self.get_spill_slot(&mut next_spill_slot);
                    spill_slots.insert(spill_candidate, slot);
                    reg_map.insert(interval.vreg, reg);
                    active.retain(|(_, v, _)| *v != spill_candidate);
                    active.push((interval.end, interval.vreg, reg));
                } else {
                    // Spill current interval
                    let slot = self.get_spill_slot(&mut next_spill_slot);
                    spill_slots.insert(interval.vreg, slot);
                }
            }
        }

        // Any vregs not assigned to a register get a spill slot
        for interval in &intervals {
            if !reg_map.contains_key(&interval.vreg) && !spill_slots.contains_key(&interval.vreg) {
                let slot = self.get_spill_slot(&mut next_spill_slot);
                spill_slots.insert(interval.vreg, slot);
            }
        }

        Allocation {
            reg_map,
            spill_slots,
            stack_size: next_spill_slot * 8,
        }
    }

    fn get_spill_slot(&self, counter: &mut usize) -> usize {
        let slot = *counter;
        *counter += 1;
        slot
    }

    fn expire_intervals(
        &self,
        current_start: usize,
        active: &mut Vec<(usize, VReg, PhysReg)>,
        reg_map: &mut HashMap<VReg, PhysReg>,
    ) {
        active.retain(|(end, vreg, reg)| {
            if *end <= current_start {
                reg_map.remove(vreg);
                false
            } else {
                true
            }
        });
    }

    fn compute_live_intervals(&self, func: &IrFunction) -> Vec<LiveInterval> {
        let mut defs: HashMap<VReg, usize> = HashMap::new();
        let mut uses: HashMap<VReg, Vec<usize>> = HashMap::new();
        let mut instr_counter: usize = 0;

        for block in &func.blocks {
            for instr in &block.instrs {
                for u in instr.uses() {
                    uses.entry(u).or_default().push(instr_counter);
                }
                if let Some(d) = instr.dest() {
                    defs.entry(d).or_insert(instr_counter);
                }
                instr_counter += 1;
            }
            match &block.terminator {
                IrTerminator::Ret(Some(v)) => {
                    uses.entry(*v).or_default().push(instr_counter);
                }
                IrTerminator::BrCond(v, _, _) => {
                    uses.entry(*v).or_default().push(instr_counter);
                }
                _ => {}
            }
            instr_counter += 1;
        }

        let mut intervals = Vec::new();
        for (&vreg, &def) in &defs {
            let last_use = uses.get(&vreg).and_then(|u| u.iter().max()).copied().unwrap_or(def);
            intervals.push(LiveInterval {
                vreg,
                start: def,
                end: last_use,
            });
        }

        for (&vreg, uses_list) in &uses {
            if !defs.contains_key(&vreg) {
                let last_use = uses_list.iter().max().copied().unwrap_or(0);
                intervals.push(LiveInterval {
                    vreg,
                    start: 0,
                    end: last_use,
                });
            }
        }

        intervals
    }
}
