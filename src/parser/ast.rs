pub trait Node {
    type OutputNode: Node;
    fn new() -> Self::OutputNode;
    fn get_type(&self) -> &NodeType;
}

pub struct Program {
    node_type: NodeType,
    body: [Statement],
}

pub struct Statement {}

impl Node for Statement {
    type OutputNode = Self;

    fn new() -> Self::OutputNode {
        Statement {}
    }

    fn get_type(&self) -> &NodeType {
        &NodeType::Statement
    }
}

pub struct Pattern {}

impl Node for Pattern {
    type OutputNode = Self;

    fn new() -> Self::OutputNode {
        Pattern {}
    }

    fn get_type(&self) -> &NodeType {
        &NodeType::Pattern
    }
}

pub struct SourceLocation {
    source: Option<String>,
    start: Position,
    end: Position,
}

pub struct Position {
    line: u16,
    col: u16,
}

pub struct Identifier {
    // name: String,
}

impl Node for Identifier {
    type OutputNode = Self;
    fn get_type(&self) -> &NodeType {
        &NodeType::Identifier
    }

    fn new() -> Self::OutputNode {
        Identifier {}
    }
}

pub enum NodeType {
    Program,
    Statement,
    Identifier,
    Pattern,
}
