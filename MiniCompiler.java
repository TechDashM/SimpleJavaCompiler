import java.awt.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;

public class MiniCompiler {
    // Token types
    public enum TokenType {
        // Keywords
        INT, FLOAT, STRING, VOID, IF, ELSE, WHILE, FOR, RETURN, BREAK, CONTINUE, MESSAGE, DO,
        // Operators
        PLUS, MINUS, MULTIPLY, DIVIDE, MODULO, ASSIGN, EQUALS, NOT_EQUALS,
        LESS_THAN, GREATER_THAN, LESS_EQUAL, GREATER_EQUAL, AND, OR, NOT,
        INCREMENT, DECREMENT,
        // Symbols
        LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET, SEMICOLON, COMMA, DOT,
        // Literals
        INTEGER, FLOAT_NUM, STRING_LITERAL, IDENTIFIER,
        // End of file
        EOF
    }

    // Token class
    public static class Token {
        public final TokenType type;
        public final String value;
        public final int line;

        public Token(TokenType type, String value, int line) {
            this.type = type;
            this.value = value;
            this.line = line;
        }

        @Override
        public String toString() {
            return String.format("Token(%s, %s, line: %d)", type, value, line);
        }
    }

    // Lexical Analyzer
    public static class Lexer {
        private final String text;
        private int pos = 0;
        private int currentLine = 1;
        private char currentChar;
        private final Map<String, TokenType> keywords;

        public Lexer(String text) {
            this.text = text;
            this.currentChar = text.length() > 0 ? text.charAt(0) : '\0';
            
            // Initialize keywords
            keywords = new HashMap<>();
            keywords.put("INT", TokenType.INT);
            keywords.put("float", TokenType.FLOAT);
            keywords.put("STRING", TokenType.STRING);
            keywords.put("void", TokenType.VOID);
            keywords.put("if", TokenType.IF);
            keywords.put("else", TokenType.ELSE);
            keywords.put("while", TokenType.WHILE);
            keywords.put("for", TokenType.FOR);
            keywords.put("return", TokenType.RETURN);
            keywords.put("break", TokenType.BREAK);
            keywords.put("continue", TokenType.CONTINUE);
            keywords.put("Message", TokenType.MESSAGE);
            keywords.put("do", TokenType.DO);
        }

        public Token peekNextToken() {
            return peekNextToken(1);
        }

        public Token peekNextToken(int n) {
            int originalPos = pos;
            int originalLine = currentLine;
            char originalChar = currentChar;
            
            Token token = null;
            for (int i = 0; i < n; i++) {
                token = getNextToken();
            }
            
            pos = originalPos;
            currentLine = originalLine;
            currentChar = originalChar;
            
            return token;
        }

        public Token getNextToken() {
            while (currentChar != '\0') {
                if (Character.isWhitespace(currentChar)) {
                    skipWhitespace();
                    continue;
                }

                if (Character.isDigit(currentChar)) {
                    String num = getInteger();
                    return new Token(TokenType.INTEGER, num, currentLine);
                }

                if (Character.isLetter(currentChar)) {
                    String id = getIdentifier();
                    TokenType type = keywords.getOrDefault(id, TokenType.IDENTIFIER);
                    return new Token(type, id, currentLine);
                }

                if (currentChar == '"') {
                    return new Token(TokenType.STRING_LITERAL, getString(), currentLine);
                }

                // Handle operators and symbols
                switch (currentChar) {
                    case '+':
                        advance();
                        if (currentChar == '+') {
                            advance();
                            return new Token(TokenType.INCREMENT, "++", currentLine);
                        }
                        return new Token(TokenType.PLUS, "+", currentLine);
                    case '-':
                        advance();
                        if (currentChar == '-') {
                            advance();
                            return new Token(TokenType.DECREMENT, "--", currentLine);
                        }
                        return new Token(TokenType.MINUS, "-", currentLine);
                    case '*':
                        advance();
                        return new Token(TokenType.MULTIPLY, "*", currentLine);
                    case '/':
                        advance();
                        return new Token(TokenType.DIVIDE, "/", currentLine);
                    case '%':
                        advance();
                        return new Token(TokenType.MODULO, "%", currentLine);
                    case '=':
                        advance();
                        if (currentChar == '=') {
                            advance();
                            return new Token(TokenType.EQUALS, "==", currentLine);
                        }
                        return new Token(TokenType.ASSIGN, "=", currentLine);
                    case '!':
                        advance();
                        if (currentChar == '=') {
                            advance();
                            return new Token(TokenType.NOT_EQUALS, "!=", currentLine);
                        }
                        return new Token(TokenType.NOT, "!", currentLine);
                    case '<':
                        advance();
                        if (currentChar == '=') {
                            advance();
                            return new Token(TokenType.LESS_EQUAL, "<=", currentLine);
                        }
                        return new Token(TokenType.LESS_THAN, "<", currentLine);
                    case '>':
                        advance();
                        if (currentChar == '=') {
                            advance();
                            return new Token(TokenType.GREATER_EQUAL, ">=", currentLine);
                        }
                        return new Token(TokenType.GREATER_THAN, ">", currentLine);
                    case '&':
                        advance();
                        if (currentChar == '&') {
                            advance();
                            return new Token(TokenType.AND, "&&", currentLine);
                        }
                    case '|':
                        advance();
                        if (currentChar == '|') {
                            advance();
                            return new Token(TokenType.OR, "||", currentLine);
                        }
                    case '(':
                        advance();
                        return new Token(TokenType.LPAREN, "(", currentLine);
                    case ')':
                        advance();
                        return new Token(TokenType.RPAREN, ")", currentLine);
                    case '{':
                        advance();
                        return new Token(TokenType.LBRACE, "{", currentLine);
                    case '}':
                        advance();
                        return new Token(TokenType.RBRACE, "}", currentLine);
                    case '[':
                        advance();
                        return new Token(TokenType.LBRACKET, "[", currentLine);
                    case ']':
                        advance();
                        return new Token(TokenType.RBRACKET, "]", currentLine);
                    case ';':
                        advance();
                        return new Token(TokenType.SEMICOLON, ";", currentLine);
                    case ',':
                        advance();
                        return new Token(TokenType.COMMA, ",", currentLine);
                    case '.':
                        advance();
                        return new Token(TokenType.DOT, ".", currentLine);
                }

                throw new RuntimeException("Invalid character: " + currentChar);
            }

            return new Token(TokenType.EOF, "", currentLine);
        }

        private void advance() {
            pos++;
            if (pos < text.length()) {
                currentChar = text.charAt(pos);
            } else {
                currentChar = '\0';
            }
        }

        private void skipWhitespace() {
            while (currentChar != '\0' && Character.isWhitespace(currentChar)) {
                if (currentChar == '\n') {
                    currentLine++;
                }
                advance();
            }
        }

        private String getInteger() {
            StringBuilder result = new StringBuilder();
            while (currentChar != '\0' && Character.isDigit(currentChar)) {
                result.append(currentChar);
                advance();
            }
            return result.toString();
        }

        private String getFloat() {
            StringBuilder result = new StringBuilder();
            boolean hasDecimal = false;
            
            while (currentChar != '\0' && (Character.isDigit(currentChar) || currentChar == '.')) {
                if (currentChar == '.') {
                    if (hasDecimal) break;
                    hasDecimal = true;
                }
                result.append(currentChar);
                advance();
            }
            return result.toString();
        }

        private String getString() {
            StringBuilder result = new StringBuilder();
            advance(); // Skip opening quote
            while (currentChar != '\0' && currentChar != '"') {
                if (currentChar == '\\') {
                    advance();
                    switch (currentChar) {
                        case 'n': result.append('\n'); break;
                        case 't': result.append('\t'); break;
                        default: result.append(currentChar);
                    }
                } else {
                    result.append(currentChar);
                }
                advance();
            }
            advance(); // Skip closing quote
            return result.toString();
        }

        private String getIdentifier() {
            StringBuilder result = new StringBuilder();
            while (currentChar != '\0' && (Character.isLetterOrDigit(currentChar) || currentChar == '_')) {
                result.append(currentChar);
                advance();
            }
            return result.toString();
        }
    }

    // Abstract Syntax Tree Nodes
    public static abstract class ASTNode {
        public abstract void accept(Visitor visitor);
    }

    public static class Program extends ASTNode {
        public final java.util.List<ASTNode> declarations;

        public Program(java.util.List<ASTNode> declarations) {
            this.declarations = declarations;
        }

        @Override
        public void accept(Visitor visitor) {
            visitor.visit(this);
        }
    }

    // Visitor interface for AST traversal
    public interface Visitor {
        void visit(Program node);
        void visit(Message node);
        void visit(BinaryOp node);
        void visit(Number node);
        void visit(Variable node);
        void visit(VarDecl node);
        void visit(StringLiteral node);
        void visit(FunctionDecl node);
        void visit(FunctionCall node);
        void visit(IfElse node);
        void visit(While node);
        void visit(DoWhile node);
        void visit(Assignment node);
        void visit(Block node);
    }

    // Update MessageOutput class
    public static class MessageOutput {
        private static final java.util.List<String> messages = new ArrayList<>();

        public static void add(String message) {
            messages.add(message);
            System.out.println("Adding message: " + message); // Debug output
        }

        public static java.util.List<String> getMessages() {
            return new ArrayList<>(messages);
        }

        public static void clear() {
            messages.clear();
        }
    }

    // Add Message AST node
    public static class Message extends ASTNode {
        private final java.util.List<ASTNode> args;

        public Message(java.util.List<ASTNode> args) {
            this.args = args;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public java.util.List<ASTNode> getArgs() {
            return args;
        }
    }

    // Add Expression AST nodes
    public static class BinaryOp extends ASTNode {
        private final ASTNode left;
        private final Token op;
        private final ASTNode right;

        public BinaryOp(ASTNode left, Token op, ASTNode right) {
            this.left = left;
            this.op = op;
            this.right = right;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public ASTNode getLeft() { return left; }
        public Token getOp() { return op; }
        public ASTNode getRight() { return right; }
    }

    public static class Number extends ASTNode {
        private final Token token;

        public Number(Token token) {
            this.token = token;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public Token getToken() { return token; }
    }

    public static class Variable extends ASTNode {
        private final Token token;

        public Variable(Token token) {
            this.token = token;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public Token getToken() { return token; }
    }

    // Add Variable Declaration AST node
    public static class VarDecl extends ASTNode {
        private final Token type;
        private final Token name;
        private final ASTNode value;

        public VarDecl(Token type, Token name, ASTNode value) {
            this.type = type;
            this.name = name;
            this.value = value;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public Token getType() { return type; }
        public Token getName() { return name; }
        public ASTNode getValue() { return value; }
    }

    // Add StringLiteral AST node
    public static class StringLiteral extends ASTNode {
        private final Token token;

        public StringLiteral(Token token) {
            this.token = token;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public Token getToken() { return token; }
    }

    // Add Function AST nodes
    public static class FunctionDecl extends ASTNode {
        private final Token returnType;
        private final Token name;
        private final java.util.List<VarDecl> parameters;
        private final java.util.List<ASTNode> body;

        public FunctionDecl(Token returnType, Token name, java.util.List<VarDecl> parameters, java.util.List<ASTNode> body) {
            this.returnType = returnType;
            this.name = name;
            this.parameters = parameters;
            this.body = body;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public Token getReturnType() { return returnType; }
        public Token getName() { return name; }
        public java.util.List<VarDecl> getParameters() { return parameters; }
        public java.util.List<ASTNode> getBody() { return body; }
    }

    public static class FunctionCall extends ASTNode {
        private final Token name;
        private final java.util.List<ASTNode> arguments;

        public FunctionCall(Token name, java.util.List<ASTNode> arguments) {
            this.name = name;
            this.arguments = arguments;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public Token getName() { return name; }
        public java.util.List<ASTNode> getArguments() { return arguments; }
    }

    // Update AST nodes
    public static class IfElse extends ASTNode {
        private final ASTNode condition;
        private final ASTNode ifBody;
        private final ASTNode elseBody;

        public IfElse(ASTNode condition, ASTNode ifBody, ASTNode elseBody) {
            this.condition = condition;
            this.ifBody = ifBody;
            this.elseBody = elseBody;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public ASTNode getCondition() { return condition; }
        public ASTNode getIfBody() { return ifBody; }
        public ASTNode getElseBody() { return elseBody; }
    }

    public static class While extends ASTNode {
        private final ASTNode condition;
        private final ASTNode body;

        public While(ASTNode condition, ASTNode body) {
            this.condition = condition;
            this.body = body;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public ASTNode getCondition() { return condition; }
        public ASTNode getBody() { return body; }
    }

    public static class DoWhile extends ASTNode {
        private final ASTNode condition;
        private final ASTNode body;

        public DoWhile(ASTNode condition, ASTNode body) {
            this.condition = condition;
            this.body = body;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public ASTNode getCondition() { return condition; }
        public ASTNode getBody() { return body; }
    }

    // Update MessageVisitor
    public static class MessageVisitor implements Visitor {
        private final Map<String, Object> variables = new HashMap<>();
        private final Map<String, FunctionDecl> functions = new HashMap<>();

        @Override
        public void visit(Program node) {
            System.out.println("Visiting Program with " + node.declarations.size() + " declarations");
            // First pass: collect function declarations
            for (ASTNode decl : node.declarations) {
                if (decl instanceof FunctionDecl) {
                    decl.accept(this);
                }
            }
            // Second pass: execute other declarations
            for (ASTNode decl : node.declarations) {
                if (!(decl instanceof FunctionDecl)) {
                    decl.accept(this);
                }
            }
        }

        @Override
        public void visit(FunctionDecl node) {
            System.out.println("Registering function: " + node.getName().value);
            functions.put(node.getName().value, node);
        }

        @Override
        public void visit(FunctionCall node) {
            System.out.println("Calling function: " + node.getName().value);
            FunctionDecl func = functions.get(node.getName().value);
            if (func == null) {
                System.out.println("Error: Function " + node.getName().value + " not found");
                return;
            }

            // Create new scope for function call
            Map<String, Object> oldVariables = new HashMap<>(variables);
            
            // Set parameter values
            for (int i = 0; i < func.getParameters().size(); i++) {
                VarDecl param = func.getParameters().get(i);
                ASTNode arg = node.getArguments().get(i);
                Object value = evaluate(arg);
                System.out.println("Setting parameter " + param.getName().value + " = " + value);
                variables.put(param.getName().value, value);
            }

            // Execute function body
            for (ASTNode stmt : func.getBody()) {
                if (stmt instanceof VarDecl) {
                    // Handle local variable declarations
                    VarDecl varDecl = (VarDecl) stmt;
                    if (varDecl.getValue() != null) {
                        Object value = evaluate(varDecl.getValue());
                        variables.put(varDecl.getName().value, value);
                        System.out.println("Set local variable " + varDecl.getName().value + " = " + value);
                    }
                } else {
                    // Handle other statements (Message, etc.)
                    stmt.accept(this);
                }
            }

            // Restore old scope
            variables.clear();
            variables.putAll(oldVariables);
        }

        @Override
        public void visit(VarDecl node) {
            System.out.println("Visiting VarDecl: " + node.getName().value);
            if (node.getValue() != null) {
                Object value = evaluate(node.getValue());
                variables.put(node.getName().value, value);
                System.out.println("Set variable " + node.getName().value + " = " + value);
            }
        }

        @Override
        public void visit(Message node) {
            System.out.println("Visiting Message with " + node.getArgs().size() + " args");
            StringBuilder output = new StringBuilder();
            for (ASTNode arg : node.getArgs()) {
                Object value = evaluate(arg);
                if (value instanceof Double) {
                    output.append(value);
                } else if (value instanceof StringLiteral) {
                    output.append(((StringLiteral) value).getToken().value);
                } else if (value instanceof String) {
                    output.append(value);
                }
                output.append(" ");
            }
            String message = output.toString().trim();
            System.out.println("Adding message: " + message);
            MessageOutput.add(message);
        }

        @Override
        public void visit(StringLiteral node) {
            // String literals are handled directly in evaluate
        }

        @Override
        public void visit(Number node) {
            // Numbers are handled directly in evaluate
        }

        @Override
        public void visit(Variable node) {
            // Variables are handled directly in evaluate
        }

        @Override
        public void visit(BinaryOp node) {
            // Binary operations are handled directly in evaluate
        }

        @Override
        public void visit(IfElse node) {
            System.out.println("Visiting IfElse");
            Object conditionValue = evaluate(node.getCondition());
            System.out.println("Condition value: " + conditionValue);
            
            if (conditionValue instanceof Double && ((Double) conditionValue) > 0) {
                System.out.println("Executing if body");
                node.getIfBody().accept(this);
            } else if (node.getElseBody() != null) {
                System.out.println("Executing else body");
                node.getElseBody().accept(this);
            }
        }

        @Override
        public void visit(While node) {
            System.out.println("Visiting While loop");
            while (evaluate(node.getCondition()) instanceof Double && ((Double) evaluate(node.getCondition())) > 0) {
                System.out.println("While loop iteration");
                node.getBody().accept(this);
            }
        }

        @Override
        public void visit(DoWhile node) {
            System.out.println("Visiting DoWhile loop");
            do {
                System.out.println("DoWhile loop iteration");
                node.getBody().accept(this);
            } while (evaluate(node.getCondition()) instanceof Double && ((Double) evaluate(node.getCondition())) > 0);
        }

        @Override
        public void visit(Assignment node) {
            System.out.println("Visiting Assignment: " + node.getIdentifier().value);
            Object value = evaluate(node.getValue());
            variables.put(node.getIdentifier().value, value);
            System.out.println("Set variable " + node.getIdentifier().value + " = " + value);
        }

        @Override
        public void visit(Block node) {
            System.out.println("Visiting Block with " + node.getStatements().size() + " statements");
            for (ASTNode stmt : node.getStatements()) {
                stmt.accept(this);
            }
        }

        private Double evaluateToDouble(ASTNode node) {
            if (node instanceof Number) {
                return Double.parseDouble(((Number) node).getToken().value);
            } else if (node instanceof Variable) {
                String varName = ((Variable) node).getToken().value;
                Object value = variables.get(varName);
                if (value == null) {
                    System.out.println("Warning: Variable " + varName + " not found, defaulting to 0");
                    return 0.0;
                }
                return (Double) value;
            } else if (node instanceof BinaryOp) {
                BinaryOp op = (BinaryOp) node;
                Double left = evaluateToDouble(op.getLeft());
                Double right = evaluateToDouble(op.getRight());
                
                System.out.println("Evaluating binary op: " + left + " " + op.getOp().type + " " + right);
                
                switch (op.getOp().type) {
                    case PLUS: return left + right;
                    case MINUS: return left - right;
                    case MULTIPLY: return left * right;
                    case DIVIDE: return left / right;
                    case GREATER_THAN: return left > right ? 1.0 : 0.0;
                    case LESS_THAN: return left < right ? 1.0 : 0.0;
                    case EQUALS: return left.equals(right) ? 1.0 : 0.0;
                    case NOT_EQUALS: return !left.equals(right) ? 1.0 : 0.0;
                    default: return 0.0;
                }
            }
            return 0.0;
        }

        private Object evaluate(ASTNode node) {
            if (node instanceof Number) {
                return Double.parseDouble(((Number) node).getToken().value);
            } else if (node instanceof StringLiteral) {
                return ((StringLiteral) node).getToken().value;
            } else if (node instanceof Variable) {
                String varName = ((Variable) node).getToken().value;
                Object value = variables.get(varName);
                if (value == null) {
                    System.out.println("Warning: Variable " + varName + " not found, defaulting to empty string");
                    return "";
                }
                return value;
            } else if (node instanceof BinaryOp) {
                return evaluateToDouble(node);
            }
            return null;
        }
    }

    // Update Parser class
    public static class Parser {
        private final Lexer lexer;
        private Token currentToken;

        public Parser(Lexer lexer) {
            this.lexer = lexer;
            this.currentToken = lexer.getNextToken();
        }

        private void eat(TokenType type) {
            if (currentToken.type == type) {
                currentToken = lexer.getNextToken();
            } else {
                String errorMsg = String.format(
                    "Syntax Error at line %d: Expected '%s' but found '%s'",
                    currentToken.line,
                    type,
                    currentToken.type
                );
                throw new RuntimeException(errorMsg);
            }
        }

        private ASTNode parseStatement() {
            if (currentToken.type == TokenType.INT || currentToken.type == TokenType.STRING) {
                return parseVarDecl();
            } else if (currentToken.type == TokenType.MESSAGE) {
                return parseMessage();
            } else if (currentToken.type == TokenType.IF) {
                return parseIfElse();
            } else if (currentToken.type == TokenType.WHILE) {
                return parseWhile();
            } else if (currentToken.type == TokenType.DO) {
                return parseDoWhile();
            } else if (currentToken.type == TokenType.IDENTIFIER) {
                Token identifier = currentToken;
                eat(TokenType.IDENTIFIER);
                if (currentToken.type == TokenType.ASSIGN) {
                    eat(TokenType.ASSIGN);
                    ASTNode value = parseExpression();
                    eat(TokenType.SEMICOLON);
                    return new Assignment(identifier, value);
                } else if (currentToken.type == TokenType.LPAREN) {
                    return parseFunctionCall(identifier);
                } else {
                    throw new RuntimeException(String.format(
                        "Syntax Error at line %d: Unexpected token '%s' after identifier '%s'",
                        currentToken.line,
                        currentToken.type,
                        identifier.value
                    ));
                }
            } else {
                throw new RuntimeException(String.format(
                    "Syntax Error at line %d: Unexpected token '%s'",
                    currentToken.line,
                    currentToken.type
                ));
            }
        }

        private ASTNode parseBlock() {
            java.util.List<ASTNode> statements = new ArrayList<>();
            while (currentToken.type != TokenType.RBRACE) {
                statements.add(parseStatement());
            }
            return new Block(statements);
        }

        private IfElse parseIfElse() {
            eat(TokenType.IF);
            eat(TokenType.LPAREN);
            ASTNode condition = parseExpression();
            eat(TokenType.RPAREN);
            eat(TokenType.LBRACE);
            ASTNode ifBody = parseBlock();
            eat(TokenType.RBRACE);
            
            ASTNode elseBody = null;
            if (currentToken.type == TokenType.ELSE) {
                eat(TokenType.ELSE);
                eat(TokenType.LBRACE);
                elseBody = parseBlock();
                eat(TokenType.RBRACE);
            }
            
            return new IfElse(condition, ifBody, elseBody);
        }

        private While parseWhile() {
            eat(TokenType.WHILE);
            eat(TokenType.LPAREN);
            ASTNode condition = parseExpression();
            eat(TokenType.RPAREN);
            eat(TokenType.LBRACE);
            ASTNode body = parseBlock();
            eat(TokenType.RBRACE);
            
            return new While(condition, body);
        }

        private DoWhile parseDoWhile() {
            eat(TokenType.DO);
            eat(TokenType.LBRACE);
            ASTNode body = parseBlock();
            eat(TokenType.RBRACE);
            
            eat(TokenType.WHILE);
            eat(TokenType.LPAREN);
            ASTNode condition = parseExpression();
            eat(TokenType.RPAREN);
            eat(TokenType.SEMICOLON);
            
            return new DoWhile(condition, body);
        }

        private ASTNode parseExpression() {
            if (currentToken.type == TokenType.STRING_LITERAL) {
                Token token = currentToken;
                eat(TokenType.STRING_LITERAL);
                return new StringLiteral(token);
            }
            return parseTerm();
        }

        private ASTNode parseTerm() {
            ASTNode left = parseFactor();
            while (currentToken.type == TokenType.PLUS || 
                   currentToken.type == TokenType.MINUS ||
                   currentToken.type == TokenType.LESS_THAN ||
                   currentToken.type == TokenType.GREATER_THAN ||
                   currentToken.type == TokenType.EQUALS ||
                   currentToken.type == TokenType.NOT_EQUALS) {
                Token op = currentToken;
                eat(currentToken.type);
                ASTNode right = parseFactor();
                left = new BinaryOp(left, op, right);
            }
            return left;
        }

        private ASTNode parseFactor() {
            if (currentToken.type == TokenType.INTEGER) {
                Token token = currentToken;
                eat(TokenType.INTEGER);
                return new Number(token);
            } else if (currentToken.type == TokenType.STRING_LITERAL) {
                Token token = currentToken;
                eat(TokenType.STRING_LITERAL);
                return new StringLiteral(token);
            } else if (currentToken.type == TokenType.IDENTIFIER) {
                Token token = currentToken;
                eat(TokenType.IDENTIFIER);
                return new Variable(token);
            } else if (currentToken.type == TokenType.LPAREN) {
                eat(TokenType.LPAREN);
                ASTNode expr = parseExpression();
                eat(TokenType.RPAREN);
                return expr;
            } else {
                throw new RuntimeException(String.format(
                    "Syntax Error at line %d: Unexpected token '%s'",
                    currentToken.line,
                    currentToken.type
                ));
            }
        }

        private Message parseMessage() {
            eat(TokenType.MESSAGE);
            eat(TokenType.LPAREN);
            java.util.List<ASTNode> args = new ArrayList<>();
            if (currentToken.type != TokenType.RPAREN) {
                args.add(parseExpression());
                while (currentToken.type == TokenType.COMMA) {
                    eat(TokenType.COMMA);
                    args.add(parseExpression());
                }
            }
            eat(TokenType.RPAREN);
            eat(TokenType.SEMICOLON);
            return new Message(args);
        }

        private VarDecl parseVarDecl() {
            Token type = currentToken;
            eat(type.type);  // Eat the type token (INT or STRING)
            
            if (currentToken.type != TokenType.IDENTIFIER) {
                throw new RuntimeException(String.format(
                    "Syntax Error at line %d: Expected identifier after '%s'",
                    currentToken.line,
                    type.value
                ));
            }
            
            Token name = currentToken;
            eat(TokenType.IDENTIFIER);
            eat(TokenType.ASSIGN);
            
            ASTNode value;
            if (type.type == TokenType.INT) {
                if (currentToken.type == TokenType.STRING_LITERAL) {
                    throw new RuntimeException(String.format(
                        "Syntax Error at line %d: Cannot assign string literal to INT variable",
                        currentToken.line
                    ));
                }
                value = parseArithmeticExpression();
            } else if (type.type == TokenType.STRING) {
                if (currentToken.type != TokenType.STRING_LITERAL) {
                    throw new RuntimeException(String.format(
                        "Syntax Error at line %d: Expected string literal for STRING variable",
                        currentToken.line
                    ));
                }
                value = parseExpression();
            } else {
                value = parseExpression();
            }
            
            eat(TokenType.SEMICOLON);
            return new VarDecl(type, name, value);
        }

        private ASTNode parseArithmeticExpression() {
            ASTNode left = parseTerm();
            while (currentToken.type == TokenType.PLUS || 
                   currentToken.type == TokenType.MINUS) {
                Token op = currentToken;
                eat(currentToken.type);
                ASTNode right = parseTerm();
                left = new BinaryOp(left, op, right);
            }
            return left;
        }

        private ASTNode parseTerm() {
            ASTNode left = parseFactor();
            while (currentToken.type == TokenType.MULTIPLY || 
                   currentToken.type == TokenType.DIVIDE) {
                Token op = currentToken;
                eat(currentToken.type);
                ASTNode right = parseFactor();
                left = new BinaryOp(left, op, right);
            }
            return left;
        }

        private FunctionCall parseFunctionCall(Token identifier) {
            eat(TokenType.LPAREN);
            java.util.List<ASTNode> arguments = new ArrayList<>();
            if (currentToken.type != TokenType.RPAREN) {
                arguments.add(parseExpression());
                while (currentToken.type == TokenType.COMMA) {
                    eat(TokenType.COMMA);
                    arguments.add(parseExpression());
                }
            }
            eat(TokenType.RPAREN);
            eat(TokenType.SEMICOLON);
            return new FunctionCall(identifier, arguments);
        }

        public Program parse() {
            java.util.List<ASTNode> declarations = new ArrayList<>();
            while (currentToken.type != TokenType.EOF) {
                declarations.add(parseStatement());
            }
            return new Program(declarations);
        }
    }

    // Add new AST nodes
    public static class Assignment extends ASTNode {
        private final Token identifier;
        private final ASTNode value;

        public Assignment(Token identifier, ASTNode value) {
            this.identifier = identifier;
            this.value = value;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public Token getIdentifier() { return identifier; }
        public ASTNode getValue() { return value; }
    }

    public static class Block extends ASTNode {
        private final java.util.List<ASTNode> statements;

        public Block(java.util.List<ASTNode> statements) {
            this.statements = statements;
        }

        @Override
        public void accept(Visitor visitor) {
            if (visitor instanceof MessageVisitor) {
                ((MessageVisitor) visitor).visit(this);
            }
        }

        public java.util.List<ASTNode> getStatements() { return statements; }
    }

    // UI Class
    public static class CompilerUI extends JFrame {
        private JTextArea codeArea;
        private JTextArea outputArea;
        private JButton compileButton;
        private JButton clearButton;

        public CompilerUI() {
            setTitle("Mini Compiler");
            setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            setSize(1000, 800);
            setLocationRelativeTo(null);

            // Create main panel with split pane
            JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
            splitPane.setDividerLocation(400);
            splitPane.setResizeWeight(0.5);

            // Create code panel
            JPanel codePanel = new JPanel(new BorderLayout());
            codePanel.setBorder(new TitledBorder("Source Code"));
            
            codeArea = new JTextArea();
            codeArea.setFont(new Font("Monospaced", Font.PLAIN, 14));
            codeArea.setTabSize(4);
            codeArea.setLineWrap(false);
            JScrollPane codeScroll = new JScrollPane(codeArea);
            codeScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            codeScroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
            codePanel.add(codeScroll, BorderLayout.CENTER);

            // Create button panel
            JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            compileButton = new JButton("Compile");
            clearButton = new JButton("Clear");
            
            buttonPanel.add(compileButton);
            buttonPanel.add(clearButton);

            // Add button panel to code panel
            codePanel.add(buttonPanel, BorderLayout.NORTH);

            // Create output panel
            JPanel outputPanel = new JPanel(new BorderLayout());
            outputPanel.setBorder(new TitledBorder("Output"));
            
            outputArea = new JTextArea();
            outputArea.setFont(new Font("Monospaced", Font.PLAIN, 14));
            outputArea.setEditable(false);
            outputArea.setLineWrap(false);
            JScrollPane outputScroll = new JScrollPane(outputArea);
            outputScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            outputScroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
            outputPanel.add(outputScroll, BorderLayout.CENTER);

            // Add panels to split pane
            splitPane.setTopComponent(codePanel);
            splitPane.setBottomComponent(outputPanel);

            // Add split pane to frame
            add(splitPane);

            // Set up button actions
            compileButton.addActionListener(e -> compile());
            clearButton.addActionListener(e -> clear());

            // Add sample code
            codeArea.setText("STRING mess = \"Hello\";\n" +
                           "Message(mess);");
        }

        private void compile() {
            String code = codeArea.getText();
            outputArea.setText(""); // Clear previous output
            MessageOutput.clear(); // Clear previous messages
            
            try {
                // Lexical Analysis
                outputArea.append("=== Lexical Analysis ===\n");
                Lexer lexer = new Lexer(code);
                Token token;
                do {
                    token = lexer.getNextToken();
                    outputArea.append(token.toString() + "\n");
                } while (token.type != TokenType.EOF);
                
                // Parsing
                outputArea.append("\n=== Parsing ===\n");
                Parser parser = new Parser(new Lexer(code));
                Program program = parser.parse();
                outputArea.append("Parsing completed successfully\n");
                
                // Semantic Analysis and Code Generation
                outputArea.append("\n=== Message Outputs ===\n");
                MessageVisitor visitor = new MessageVisitor();
                program.accept(visitor);
                
                // Show Message outputs
                java.util.List<String> messages = MessageOutput.getMessages();
                if (messages.isEmpty()) {
                    outputArea.append("No messages to display\n");
                } else {
                    for (String msg : messages) {
                        outputArea.append(msg + "\n");
                    }
                }
                
                outputArea.append("\nCompilation completed successfully!\n");
            } catch (Exception e) {
                outputArea.append("\n=== Compilation Error ===\n");
                outputArea.append(e.getMessage() + "\n");
                
                // Show line numbers and code context
                outputArea.append("\n=== Code Context ===\n");
                String[] lines = code.split("\n");
                for (int i = 0; i < lines.length; i++) {
                    if (!lines[i].trim().isEmpty()) {
                        outputArea.append(String.format("%d: %s\n", i + 1, lines[i]));
                    }
                }
            }
        }

        private void clear() {
            codeArea.setText("");
            outputArea.setText("");
        }
    }

    public static void main(String[] args) {
        // Run the UI
        SwingUtilities.invokeLater(() -> {
            CompilerUI ui = new CompilerUI();
            ui.setVisible(true);
        });
    }
}

