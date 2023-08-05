pub trait Node {
    // fn new() -> Self where Self: Sized;
    fn get_type(&self) -> &NodeType;
    fn visit(&self, visitor: Box<dyn Visitor>);
}

pub trait Visitor {
    fn visit_identifier(&self);
}

pub trait Expression: Node {}

pub trait Pattern: Node {}

pub trait Statement: Node {}

pub struct Program {
    node_type: NodeType,
    body: Vec<Box<dyn Statement>>,
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
    name: String,
}

impl Node for Identifier {
    fn get_type(&self) -> &NodeType {
        &NodeType::Identifier
    }

    fn visit(&self, visitor: Box<dyn Visitor>) {
        visitor.visit_identifier();
    }
}

impl Identifier {
    fn new(name: String) -> Self {
        Identifier { name }
    }
}

impl Expression for Identifier {}

pub enum NodeType {
    Program,
    Statement,
    Identifier,
    Pattern,
}
