use crate::computegraph::*;
use crate::datacatalog::*;
use std::vec::Vec;

#[derive(Debug)]
pub enum ComputeTaskDefinitionSources {
    ReadColumn { column: String },
}

pub struct ComputeTaskDefinitionSource(usize);

#[derive(Debug)]
pub enum ComputeTaskDefinitionFolds {
    Maximum,
}

pub struct ComputeTaskDefinitionFold(usize);

pub struct ComputeTaskDefinitionResult(usize);

#[derive(Debug)]

pub enum ComputeTaskDefinitionTypes {
    Source(ComputeTaskDefinitionSources),
    Fold(ComputeTaskDefinitionFolds),
    Result(String),
}

#[derive(Debug)]
pub struct ComputeTaskDefinition {
    id: usize,
    definition: ComputeTaskDefinitionTypes,
}

impl ComputeTaskDefinition {}

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

    fn source(&mut self, source: ComputeTaskDefinitionSources) -> ComputeTaskDefinitionSource {
        let id = self.tasks.len();
        self.tasks.push(ComputeTaskDefinition {
            id: id,
            definition: ComputeTaskDefinitionTypes::Source(source),
        });
        ComputeTaskDefinitionSource(id)
    }

    pub fn from_columns(&mut self, column: &str) -> ComputeTaskDefinitionSource {
        self.source(ComputeTaskDefinitionSources::ReadColumn {
            column: column.to_string(),
        })
    }

    pub fn fold_source(
        &mut self,
        source: &ComputeTaskDefinitionSource,
        fold: ComputeTaskDefinitionFolds,
    ) -> ComputeTaskDefinitionFold {
        let id = self.tasks.len();
        self.tasks.push(ComputeTaskDefinition {
            id: id,
            definition: ComputeTaskDefinitionTypes::Fold(fold),
        });
        self.links.push((source.0, id));
        ComputeTaskDefinitionFold(id)
    }

    pub fn result_from_fold(
        &mut self,
        fold: &ComputeTaskDefinitionFold,
        name: &str,
    ) -> ComputeTaskDefinitionResult {
        let id = self.tasks.len();
        self.tasks.push(ComputeTaskDefinition {
            id: id,
            definition: ComputeTaskDefinitionTypes::Result(name.to_string()),
        });
        self.links.push((fold.0, id));
        ComputeTaskDefinitionResult(id)
    }

    pub fn build<'a>(&self, catalog: &'a DataCatalog) -> ComputeGraph<'a> {
        let mut graph = ComputeGraph::new(catalog);

        for node in &self.tasks {
            graph.new_node(&node.definition);
        }

        for (s, d) in &self.links {
            graph.link(*s, 0, *d, 0);
        }

        graph
    }
}
