use std::collections::HashMap;
use crate::ast::expressions::*;
use crate::ast::statements::*;
use crate::error::LoxError;
use crate::token::Token;

#[derive(Debug,Default)]
struct NameGenerator(usize);
impl NameGenerator {
    fn generate(&mut self, prefix: &str) -> String {
        let name = format!("{}#{}", prefix, self.0);
        self.0 += 1;
        name
    }
}

#[derive(Debug, Default)]
struct Scope {
    /// Map of renamed symbol -> source name
    names: HashMap<String, String>,
    /// Map of source name -> new name
    source_names: HashMap<String, String>,
}

impl Scope {
    fn register_local(&mut self, source_name: &str, resolved_name: &str) {
        self.names.insert(resolved_name.to_string(), source_name.to_string());
        self.source_names.insert(source_name.to_string(), resolved_name.to_string());
    }
}

#[derive(Debug)]
pub struct Resolver {
    scopes: Vec<Scope>,
    name_generator: NameGenerator,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            // Starts out with the global scope
            scopes: vec![Scope::default()],
            name_generator: NameGenerator(1),
        }
    }

    fn gen_name(&mut self, name: &str) -> String {
        self.name_generator.generate(name)
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Resolve a local variable.
    /// Variables which shadow a variable declared in an outer scope are renamed.
    fn resolve_var(&self, name: &str) -> Option<(&str, usize)> {
        let scope_count = self.scopes.len();
        self.scopes.iter()
            .enumerate()
            .rfind(|(_index, scope)| scope.names.contains_key(name))
            .map(|(index, scope)| {
                let resolved_name = scope.names.get(name).unwrap();
                let ancestor = scope_count - index - 1;
                (resolved_name.as_str(), ancestor)
            })
    }

    fn declare_var(&mut self, name: &mut Token) -> Result<(), LoxError> {
        if self.scopes.len() <= 1 {
            // Anything goes in the global scope
            self.register(&name.lexeme, &name.lexeme);
            return Ok(())
        }

        if let Some((_name, ancestor)) = self.resolve_var(&name.lexeme) {
            // This variable shadows another variable
            if ancestor == 0 {
                // This variable shadows another variable declared in the current lexical scope.
                // This is an error.
                return Err(LoxError::new(
                    name.line,
                    "".to_string(),
                    format!("{} shadows another variable declared in the same scope", &name.lexeme))
                );
            } else {
                // This variable shadows another variable declared in an enclosing scope.
                // Rename it and all future reference
                let new_name = self.gen_name(&name.lexeme);
                self.register(&name.lexeme, &new_name);
                name.lexeme = new_name;
            }
        } else {
            // This variable doesn't shadow a variable in an enclosing scope, and doesn't need to
            // be renamed.
            self.register(&name.lexeme, &name.lexeme);
        }

        Ok(())
    }

    fn register(&mut self, source_name: &str, resolved_name: &str) {
        self.scopes.last_mut().unwrap().register_local(source_name, resolved_name);
    }

    pub fn resolve(&mut self, mut program: Vec<Statement>) -> Result<Vec<Statement>, LoxError> {
        for statement in program.iter_mut() {
            statement.resolve(self)?;
        }

        Ok(program)
    }
}

trait Resolve {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError>;
}

impl Resolve for Statement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        match self {
            Statement::Block(block) => block.resolve(resolver),
            Statement::Class(class) => class.resolve(resolver),
            Statement::Expression(expression) => expression.resolve(resolver),
            Statement::Function(function) => function.resolve(resolver),
            Statement::If(if_statement) => if_statement.resolve(resolver),
            Statement::Print(print_statement) => print_statement.resolve(resolver),
            Statement::Return(return_statement) => return_statement.resolve(resolver),
            Statement::Var(var_statement) => var_statement.resolve(resolver),
            Statement::While(while_statement) => while_statement.resolve(resolver),
        }
    }
}

impl Resolve for BlockStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        resolver.push_scope();

        for statement in &mut self.statements {
            statement.resolve(resolver)?;
        }

        resolver.pop_scope();
        Ok(())
    }
}

impl Resolve for ClassStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        resolver.declare_var(&mut self.name)?;

        if let Some(super_class) = self.super_class.as_mut() {
            if super_class.name.lexeme == self.name.lexeme {
                return Err(LoxError::new(
                    super_class.name.line,
                    "".to_string(),
                    "A class cannot inherit from itself".to_string()
                ));
            }
            super_class.resolve(resolver)?;
        }

        if self.super_class.is_some() {
            resolver.push_scope();
            resolver.register("super", "super");
        }

        resolver.push_scope();
        resolver.register("this", "this");

        for method in self.methods.iter_mut() {
            method.resolve(resolver)?;
        }

        if self.super_class.is_some() {
            resolver.pop_scope();
        }

        resolver.pop_scope();

        Ok(())
    }
}

impl Resolve for ExpressionStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.expression.resolve(resolver)
    }
}

impl Resolve for FunctionStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        resolver.declare_var(&mut self.name)?;

        resolver.push_scope();
        for param in &mut self.params {
            resolver.declare_var(param)?;
        }

        for statement in &mut self.body {
            statement.resolve(resolver)?;
        }
        resolver.pop_scope();

        Ok(())
    }
}

impl Resolve for IfStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.condition.resolve(resolver)?;
        self.then_branch.resolve(resolver)?;
        if let Some(else_branch) = self.else_branch.as_mut() {
            else_branch.resolve(resolver)?;
        }

        Ok(())
    }
}

impl Resolve for PrintStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.expression.resolve(resolver)
    }
}

impl Resolve for ReturnStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        if let Some(value) = self.value.as_mut() {
            value.resolve(resolver)?;
        }
        Ok(())
    }
}

impl Resolve for VarStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        resolver.declare_var(&mut self.name)?;

        if let Some(ref mut initializer) = self.initializer {
            initializer.resolve(resolver)
        } else {
            Ok(())
        }
    }
}

impl Resolve for WhileStatement {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.condition.resolve(resolver)?;
        self.body.resolve(resolver)
    }
}

impl Resolve for Expression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        match self {
            Expression::Assign(expr) => expr.resolve(resolver),
            Expression::Binary(expr) => expr.resolve(resolver),
            Expression::Call(expr) => expr.resolve(resolver),
            Expression::Get(expr) => expr.resolve(resolver),
            Expression::Grouping(expr) => expr.resolve(resolver),
            Expression::Literal(expr) => expr.resolve(resolver),
            Expression::Logical(expr) => expr.resolve(resolver),
            Expression::Set(expr) => expr.resolve(resolver),
            Expression::Super(expr) => expr.resolve(resolver),
            Expression::This(expr) => expr.resolve(resolver),
            Expression::Unary(expr) => expr.resolve(resolver),
            Expression::Variable(expr) => expr.resolve(resolver),
        }
    }
}

impl Resolve for AssignmentExpression{
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        if let Some((name, _ancestor)) = resolver.resolve_var(&self.name.lexeme) {
            self.name.lexeme = name.to_string();
            self.value.resolve(resolver)
        } else {
            Err(LoxError::new(
                self.name.line,
                "".to_string(),
                format!("Unknown variable '{}'", self.name.lexeme)
            ))
        }
    }
}

impl Resolve for BinaryExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.left.resolve(resolver)?;
        self.right.resolve(resolver)
    }
}

impl Resolve for CallExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        for arg in self.arguments.iter_mut() {
            arg.resolve(resolver)?;
        }

        self.callee.resolve(resolver)
    }
}

impl Resolve for GetExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.object.resolve(resolver)
    }
}

impl Resolve for SetExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.object.resolve(resolver)?;
        self.value.resolve(resolver)
    }
}

impl Resolve for SuperExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        if let Some((name, _)) = resolver.resolve_var("super") {
            self.keyword.lexeme = name.to_string();
            Ok(())
        } else {
            Err(LoxError::new(self.keyword.line, "".to_string(), "Cannot use 'super' outside of a class".to_string()))
        }
    }
}

impl Resolve for GroupingExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.expression.resolve(resolver)
    }    
}

impl Resolve for LiteralExpression {
    fn resolve(&mut self, _: &mut Resolver) -> Result<(), LoxError> {
        Ok(())
    }
}

impl Resolve for LogicalExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.left.resolve(resolver)?;
        self.right.resolve(resolver)
    }
}

impl Resolve for ThisExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        if let Some((name, _)) = resolver.resolve_var("this") {
            self.keyword.lexeme = name.to_string();
            Ok(())
        } else {
            Err(LoxError::new(self.keyword.line, "".to_string(), "Cannot use 'this' outside of a class".to_string()))
        }
    }
}

impl Resolve for UnaryExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        self.right.resolve(resolver)
    }
}

impl Resolve for VariableExpression {
    fn resolve(&mut self, resolver: &mut Resolver) -> Result<(), LoxError> {
        if let Some((name, _)) = resolver.resolve_var(&self.name.lexeme) {
            self.name.lexeme = name.to_string();
            Ok(())
        } else {
            Err(LoxError::new(self.name.line, "".to_string(), format!("Undefined variable {}", self.name.lexeme)))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::expressions::{Expression, VariableExpression};
    use crate::ast::statements::{BlockStatement, Statement, VarStatement};
    use crate::parser::Parser;
    use crate::resolver::Resolver;
    use crate::scanner::Scanner;

    #[test]
    fn dissallow_shadowing_variable_in_same_scope() {
         assert!(Resolver::new().resolve(
             ast(r#"
             {
                var x;
                var x;
             }
             "#)
         ).is_err())
    }

    #[test]
    fn renames_shadowed_variable() {
        let program = Resolver::new().resolve(
            ast(r#"
                var x = 1;
                {
                    var x = x;
                }
                "#)
        ).unwrap();

        assert_eq!(declaration(&program[0]).name.lexeme, "x");
        let dec2 = declaration(&block(&program[1]).statements[0]);
        assert_eq!(dec2.name.lexeme, "x#1");
        assert_eq!(var(&dec2.initializer.as_ref().unwrap()).name.lexeme, "x");
    }

    #[test]
    fn class_with_initializer() {
        Resolver::new().resolve(
            ast(r#"
                class Foo {
                    init(bar) {
                        this.bar = bar;
                    }
                }
                Foo(42);
                "#)
        ).unwrap();
    }

    #[test]
    fn allow_shadowing_in_the_global_scope() {
        let program = Resolver::new().resolve(
            ast(r#"
                var x = 1;
                var x = 2;
                "#)
        ).unwrap();

        assert_eq!(declaration(&program[0]).name.lexeme, "x");
        assert_eq!(declaration(&program[1]).name.lexeme, "x");
    }

    #[test]
    fn disallow_inheriting_from_self() {
        assert!(Resolver::new().resolve(
            ast(r#"
                class Foo < Foo {}
                "#)
        ).is_err());
    }

    fn ast(source: &str) -> Vec<Statement> {
        let tokens = Scanner::new(source.to_string()).scan_tokens().0;
        Parser::new(tokens).parse().unwrap()
    }

    fn declaration(stmt: &Statement) -> &VarStatement {
        match stmt {
            Statement::Var(v) => v,
            _ => panic!("Expected var statement, found {:?}", stmt)
        }
    }

    fn block(stmt: &Statement) -> &BlockStatement {
        match stmt {
            Statement::Block(block) => block,
            _ => panic!("Expected block statement found {:?}", stmt)
        }
    }

    fn var(expr: &Expression) -> &VariableExpression {
        match expr {
            Expression::Variable(var) => var,
            _ => panic!("Expected variable statement, found {:?}", expr)
        }
    }
}
