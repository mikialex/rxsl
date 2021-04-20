use std::mem::MaybeUninit;

use crate::InstructionList;
use petgraph::{graph::NodeIndex, stable_graph::StableGraph};

pub struct ControlFlowGraph {
    graph: StableGraph<BasicBlock, ()>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            graph: StableGraph::new(),
        }
    }

    pub fn create_bb(&mut self) -> (&mut BasicBlock, BasicBlockHandle) {
        let bb = BasicBlock::new();
        let inner = self.graph.add_node(bb);
        let handle = BasicBlockHandle { inner };
        let bb = self.mutate_bb(handle).unwrap();
        bb.handle = handle;
        (bb, handle)
    }

    pub fn mutate_bb(&mut self, bb: BasicBlockHandle) -> Option<&mut BasicBlock> {
        self.graph.node_weight_mut(bb.inner)
    }

    pub fn delete_bb(&mut self, bb: BasicBlockHandle) {
        self.graph.remove_node(bb.inner);
    }
}

impl std::fmt::Display for ControlFlowGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "====CFG====")?;
        self.graph
            .node_indices()
            .map(|idx| self.graph.node_weight(idx).unwrap())
            .for_each(|bb| bb.fmt(f).unwrap());
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct BasicBlockHandle {
    inner: NodeIndex,
}

pub struct BasicBlock {
    handle: BasicBlockHandle,
    pub instructions: InstructionList,
}

impl BasicBlock {
    fn new() -> Self {
        Self {
            handle: unsafe { MaybeUninit::<BasicBlockHandle>::uninit().assume_init() },
            instructions: InstructionList::new(),
        }
    }
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{:?}:\n", self.handle.inner).unwrap();
        self.instructions.fmt(f)?;
        Ok(())
    }
}
