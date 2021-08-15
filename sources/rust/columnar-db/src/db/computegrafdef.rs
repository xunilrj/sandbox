use crate::catalog::Catalog;

use super::computegraph::ComputeGraph;

// use super::computegraph::*;
// use crate::catalog::*;
// use std::vec::Vec;

#[derive(Debug)]
pub enum ComputeTaskDefinitionSources {
    ReadColumn { column: String },
}

pub trait HasOutput {
    fn id(&self) -> usize;
}
pub trait HasInput {
    fn id(&self) -> usize;
}

pub struct ComputeTaskDefinitionSource(usize);
impl HasOutput for ComputeTaskDefinitionSource {
    fn id(&self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub enum ComputeTaskDefinitionFolds {
    Maximum,
}

pub struct ComputeTaskDefinitionFold(usize);
impl HasInput for ComputeTaskDefinitionFold {
    fn id(&self) -> usize {
        self.0
    }
}

// pub struct ComputeTaskDefinitionResult(usize);

#[derive(Debug)]

pub enum ComputeTaskDefinitionTypes {
    Source(ComputeTaskDefinitionSources),
    Fold(ComputeTaskDefinitionFolds),
    //     Result(String),
}

#[derive(Debug)]
pub struct ComputeTaskDefinition {
    id: usize,
    definition: ComputeTaskDefinitionTypes,
}

#[derive(Debug)]
pub struct ComputeGraphDefinition {
    tasks: Vec<ComputeTaskDefinition>,
    links: Vec<(usize, usize)>,
}

impl ComputeGraphDefinition {
    pub fn new() -> Self {
        Self {
            tasks: Vec::new(),
            links: Vec::new(),
        }
    }

    pub fn source(&mut self, source: ComputeTaskDefinitionSources) -> ComputeTaskDefinitionSource {
        let id = self.tasks.len();
        self.tasks.push(ComputeTaskDefinition {
            id: id,
            definition: ComputeTaskDefinitionTypes::Source(source),
        });
        ComputeTaskDefinitionSource(id)
    }

    //     pub fn from_columns(&mut self, column: &str) -> ComputeTaskDefinitionSource {
    //         self.source(ComputeTaskDefinitionSources::ReadColumn {
    //             column: column.to_string(),
    //         })
    //     }

    pub fn fold(&mut self, fold: ComputeTaskDefinitionFolds) -> ComputeTaskDefinitionFold {
        let id = self.tasks.len();
        self.tasks.push(ComputeTaskDefinition {
            id: id,
            definition: ComputeTaskDefinitionTypes::Fold(fold),
        });
        ComputeTaskDefinitionFold(id)
    }

    pub fn link<S: HasOutput, T: HasInput>(&mut self, from: S, to: T) {
        self.links.push((from.id(), to.id()))
    }

    //     pub fn result_from_fold(
    //         &mut self,
    //         fold: &ComputeTaskDefinitionFold,
    //         name: &str,
    //     ) -> ComputeTaskDefinitionResult {
    //         let id = self.tasks.len();
    //         self.tasks.push(ComputeTaskDefinition {
    //             id: id,
    //             definition: ComputeTaskDefinitionTypes::Result(name.to_string()),
    //         });
    //         self.links.push((fold.0, id));
    //         ComputeTaskDefinitionResult(id)
    //     }

    pub fn build<'a>(&self, catalog: &'a Catalog) -> ComputeGraph<'a> {
        let mut graph = ComputeGraph::new(catalog);

        for node in &self.tasks {
            graph.new_node(&node.definition);
        }

        //         for (s, d) in &self.links {
        //             graph.link(*s, 0, *d, 0);
        //         }

        graph
    }
}
