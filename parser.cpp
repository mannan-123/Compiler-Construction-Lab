#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <stdexcept>
#include <cctype>
#include <sstream>
#include <algorithm>


using namespace std;



enum TokenType {
    T_INT, T_ID, T_NUM, T_FLOAT, T_STRING, T_IF, T_ELSE, T_RETURN,
    T_ASSIGN, T_PLUS, T_MINUS, T_MUL, T_DIV,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,
    T_SEMICOLON, T_GT, T_LT, T_EOF,
    T_FOR, T_WHILE, T_EQ, T_LE, T_AND
};


struct Token {
    TokenType type;
    string value;
    int lineNum;
    int colNum;
    Token(TokenType tt, string v) {
        type = tt;
        value = v;
    }
};


class Lexer {
private:
    string src;
    size_t pos;


public:
     Lexer(const string &src) {
        this->src = src;
        this->pos = 0;
    }
    
    vector<Token> tokenize() {
        vector<Token> tokens;
        while (pos < src.size()) {
            char current = src[pos];

            if (isspace(current)) {
                pos++;
                continue;
            }

            if (isdigit(current)) {
                tokens.push_back(Token(T_NUM, consumeNumber()));
                continue;
            }

            if (isalpha(current)) {
                string word = consumeWord();
                if (word == "int") {
                    tokens.push_back(Token(T_INT, word));
                } else if (word == "if") {
                    tokens.push_back(Token(T_IF, word));
                } else if (word == "else") {
                    tokens.push_back(Token(T_ELSE, word));
                } else if (word == "return") {
                    tokens.push_back(Token(T_RETURN, word));
                } else {
                    tokens.push_back(Token(T_ID, word));
                }
                continue;
            }

            switch (current) {
                case '=':
                    if (src[pos + 1] == '=') { // Handle "=="
                        tokens.push_back(Token(T_EQ, "=="));
                        pos++;
                    } else {
                        tokens.push_back(Token(T_ASSIGN, "="));
                    }
                    break;
                case '+':
                    tokens.push_back(Token(T_PLUS, "+"));
                    break;
                case '-':
                    tokens.push_back(Token(T_MINUS, "-"));
                    break;
                case '*':
                    tokens.push_back(Token(T_MUL, "*"));
                    break;
                case '/':
                    tokens.push_back(Token(T_DIV, "/"));
                    break;
                case '(':
                    tokens.push_back(Token(T_LPAREN, "("));
                    break;
                case ')':
                    tokens.push_back(Token(T_RPAREN, ")"));
                    break;
                case '{':
                    tokens.push_back(Token(T_LBRACE, "{"));
                    break;
                case '}':
                    tokens.push_back(Token(T_RBRACE, "}"));
                    break;
                case ';':
                    tokens.push_back(Token(T_SEMICOLON, ";"));
                    break;
                case '>':
                    tokens.push_back(Token(T_GT, ">"));
                    break;
                case '<':
                    if (src[pos + 1] == '=') { // Handle "<="
                        tokens.push_back(Token(T_LE, "<="));
                        pos++;
                    } else {
                        tokens.push_back(Token(T_LT, "<"));
                    }
                    break;
                case '&':
                    if (src[pos + 1] == '&') { // Handle "&&"
                        tokens.push_back(Token(T_AND, "&&"));
                        pos++;
                    } else {
                        cout << "Unexpected character: " << current << endl;
                        exit(1);
                    }
                    break;
                default:
                    cout << "Unexpected character: " << current << endl;
                    exit(1);
            }

            pos++;
        }

        tokens.push_back(Token(T_EOF, ""));
        return tokens;
    }

    string consumeNumber() {
        size_t start = pos;
        while (pos < src.size() && isdigit(src[pos])) pos++;
        return src.substr(start, pos - start);
    }


    string consumeWord() {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos])) pos++;
        return src.substr(start, pos - start);
    }
};




/*
    SymbolTable class:
    
    The SymbolTable class manages a collectin of variable names and their associated data types.
    It serves as a fundamental part of a compiler or interpreter, ensuring that variables are properly declared
    and accessed with their correct types during semantic analysis.




    Member Functions:
    1. declareVariable(const string &name, const string &type):
       - Purpose: Declares a new variable with a specified name and type.
       - This function checks if the variable is already declared. If it is, a runtime error is thrown indicating 
         that the variable has already been declared.
       - If the variable is not already declared, it adds the variable's name as a key and its type as the value 
         to the `symbolTable` map.
       - It will throw a runtime error if the variable already exists in the symbol table.
       - Example usage: Declare a new variable `x` with type `int`.
       
       if (symbolTable.find(name) != symbolTable.end()) {
           throw runtime_error("Semantic error: Variable '" + name + "' is already declared.");
       }
       symbolTable[name] = type;


    2. getVariableType(const string &name):
       - Purpose: Returns the type of a variable given its name.
       - This function checks if the variable exists in the `symbolTable`. If the variable is not found, it throws 
         a runtime error indicating that the variable has not been declared yet.
       - If the variable is declared, it retrieves and returns its associated type from the symbol table.
       - Throws a runtime error if the variable is not found.
       - Example usage: Get the type of variable `x`.
       
       if (symbolTable.find(name) == symbolTable.end()) {
           throw runtime_error("Semantic error: Variable '" + name + "' is not declared.");
       }
       return symbolTable[name];


    3. isDeclared(const string &name) const:
       - Purpose: Checks whether a variable has been declared.
       - This function returns a boolean value indicating whether the variable exists in the `symbolTable`.
       - It is a quick way to check the existence of a variable without retrieving its type.
       - Example usage: Check if variable `x` is declared.
       
       return symbolTable.find(name) != symbolTable.end();


    Private Data Members:
    
    - map<string, string> symbolTable:
      - A `map` that stores variable names (strings) as keys and their associated types (strings) as values.
      - This map allows efficient lookups to check if a variable is declared and to retrieve its type.
      
    Usage in a Compiler or Interpreter:
    - The `SymbolTable` is crucial for ensuring that variables are used consistently and correctly in a program.
    - When a variable is declared, it is added to the symbol table, and its type is stored.
    - When a variable is referenced, the symbol table is consulted to ensure that it has been declared and to retrieve its type.
    - The symbol table helps detect semantic errors such as undeclared variables or redeclared variables.
*/
class SymbolTable {
public:


    void declareVariable(const string &name, const string &type) {
        if (symbolTable.find(name) != symbolTable.end()) {
            throw runtime_error("Semantic error: Variable '" + name + "' is already declared.");
        }
        symbolTable[name] = type;
    }


    string getVariableType(const string &name) {
        if (symbolTable.find(name) == symbolTable.end()) {
            throw runtime_error("Semantic error: Variable '" + name + "' is not declared.");
        }
        return symbolTable[name];
    }


    bool isDeclared(const string &name) const {
        return symbolTable.find(name) != symbolTable.end();
    }


private:
    map<string, string> symbolTable;
};


class IntermediateCodeGnerator {
public:
    vector<string> instructions;
    int tempCount = 0;


    string newTemp() {
        return "t" + to_string(tempCount++);
    }


    void addInstruction(const string &instr) {
        instructions.push_back(instr);
    }


    void printInstructions() {
        for (const auto &instr : instructions) {
            cout << instr << endl;
        }
    }
};


class Parser {
    
public:
    //Constructor
    Parser(const vector<Token> &tokens, SymbolTable &symTable, IntermediateCodeGnerator &icg)
        : tokens(tokens), pos(0), symTable(symTable), icg(icg) {}
        //here the private member of this class are being initalized with the arguments passed to this constructor


    void parseProgram() {
        while (tokens[pos].type != T_EOF) {
            parseStatement();
        }
    }


private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable &symTable;
    IntermediateCodeGnerator &icg;


    void parseStatement() {
        if (tokens[pos].type == T_INT) {
            parseDeclaration();
        } else if (tokens[pos].type == T_ID) {
            parseAssignment();
        } else if (tokens[pos].type == T_IF) {
            parseIfStatement();
        } else if (tokens[pos].type == T_RETURN) {
            parseReturnStatement();
        } else if (tokens[pos].type == T_LBRACE) {
            parseBlock();
        } else {
            cout << "Syntax error: unexpected token '" << tokens[pos].value << "' at line " << tokens[pos].lineNum << endl;
            exit(1);
        }
    }


    /*
     parseDeclaration handles the parsing of variable declarations.
     It expects the token type to be `T_INT` (for declaring an integer type variable),
     followed by an identifier (variable name), and a semicolon to terminate the statement.
     It also registers the declared variable in the symbol table with type "int".
     Example:
     int x;   // This will be parsed and the symbol table will store x with type "int".
    */
    void parseDeclaration() {
        expect(T_INT);  // Expect and consume the int keyword.
        string varName = expectAndReturnValue(T_ID); // Expect and return the variable name (identifier).
        symTable.declareVariable(varName, "int"); // Register the variable in the symbol table with type "int".
        expect(T_SEMICOLON); // Expect the semicolon to end the statement.
    }


    /*
     parseAssignment handles the parsing of variable assignments.
     It expects an identifier (variable name), an assignment operator `=`, followed by an expression,
     and a semicolon at the end of the statement.
     It checks if the variable is declared in the symbol table, parses the expression, and generates intermediate code
     for the assignment.
     Example:
     x = 10;   -->  This will be parsed, checking if x is declared, then generating intermediate code like `x = 10`.
    */
    void parseAssignment() {
        string varName = expectAndReturnValue(T_ID);
        symTable.getVariableType(varName);    // Ensure the variable is declared in the symbol table.
        expect(T_ASSIGN);
        string expr = parseExpression();
        icg.addInstruction(varName + " = " + expr);  // Generate intermediate code for the assignment.
        expect(T_SEMICOLON);
    }
    /*
         parseIfStatement handles the parsing of `if` statements.
         It expects the keyword `if`, followed by an expression in parentheses that serves as the condition.
         If the condition evaluates to true, it executes the statement inside the block. If an `else` part is present,
         it executes the corresponding statement after the `else` keyword.
         Intermediate code for the `if` statement is generated, including labels for conditional jumps.
         Example:
         if(5 > 3) { x = 20; }  --> This will generate intermediate code for the condition check and jump instructions.
    */
    void parseIfStatement() {
        expect(T_IF);
        expect(T_LPAREN);               // Expect and consume the opening parenthesis for the condition.
        string cond = parseExpression(); // Parse the condition expression inside the parentheses.
        expect(T_RPAREN);


        string temp = icg.newTemp();    // Generate a new temporary variable for the condition result.
        icg.addInstruction(temp + " = " + cond);        // Generate intermediate code for storing the condition result.


        icg.addInstruction("if " + temp + " goto L1");   // Jump to label L1 if condition is true.
        icg.addInstruction("goto L2");                  // Otherwise, jump to label L2.
        icg.addInstruction("L1:");                      // Otherwise, jump to label L2.


        parseStatement();


        if (tokens[pos].type == T_ELSE) {            // If an `else` part exists, handle it.
            icg.addInstruction("goto L3");
            icg.addInstruction("L2:");
            expect(T_ELSE);
            parseStatement();       // Parse the statement inside the else block.
            icg.addInstruction("L3:");
        } else {
            icg.addInstruction("L2:");
        }
    }
    /*
        parseReturnStatement handles the parsing of `return` statements.
        It expects the keyword `return`, followed by an expression to return, and a semicolon to terminate the statement.
        It generates intermediate code to represent the return of the expression.
        Example:
        return x + 5;   -->  This will generate intermediate code like `return x + 5`.
    */
    void parseReturnStatement() {
        expect(T_RETURN);
        string expr = parseExpression();
        icg.addInstruction("return " + expr);  // Generate intermediate code for the return statement.
        expect(T_SEMICOLON);
    }
    /*
        parseBlock handles the parsing of block statements, which are enclosed in curly braces `{ }`.
        It parses the statements inside the block recursively until it reaches the closing brace.
        Example:
        { x = 10; y = 20; }   -->  This will parse each statement inside the block.
    */
    void parseBlock() {
        expect(T_LBRACE);           // Expect and consume the opening brace `{`.
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF) {
            parseStatement();       // Parse the statements inside the block.
        }
        expect(T_RBRACE);
    }
    /*
        parseExpression handles the parsing of expressions involving addition, subtraction, or comparison operations.
        It first parses a term, then processes addition (`+`) or subtraction (`-`) operators if present, generating
        intermediate code for the operations.
        Example:
        5 + 3 - 2;  -->  This will generate intermediate code like `t0 = 5 + 3` and `t1 = t0 - 2`.
    */
    string parseExpression() {
        string term = parseTerm();
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS) {
            TokenType op = tokens[pos++].type;
            string nextTerm = parseTerm();    // Parse the next term in the expression.
            string temp = icg.newTemp();     // Generate a temporary variable for the result
            icg.addInstruction(temp + " = " + term + (op == T_PLUS ? " + " : " - ") + nextTerm); // Intermediate code for operation
            term = temp;
        }
        if (tokens[pos].type == T_GT) {
            pos++;
            string nextExpr = parseExpression();    // Parse the next expression for the comparison.
            string temp = icg.newTemp();             // Generate a temporary variable for the result.
            icg.addInstruction(temp + " = " + term + " > " + nextExpr); // Intermediate code for the comparison.
            term = temp; 
        }
        return term;
    }
    /*
        parseTerm handles the parsing of terms involving multiplication or division operations.
        It first parses a factor, then processes multiplication (`*`) or division (`/`) operators if present,
        generating intermediate code for the operations.
        Example:
        5 * 3 / 2;   This will generate intermediate code like `t0 = 5 * 3` and `t1 = t0 / 2`.
    */
    string parseTerm() {
        string factor = parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV) {
            TokenType op = tokens[pos++].type;
            string nextFactor = parseFactor();
            string temp = icg.newTemp(); // Generate a temporary variable for the result.
            icg.addInstruction(temp + " = " + factor + (op == T_MUL ? " * " : " / ") + nextFactor);  // Intermediate code for operation.
            factor = temp;  // Update the factor to be the temporary result.
        }
        return factor;
    }
    /*
        parseFactor handles the parsing of factors in expressions, which can be either numeric literals, identifiers
        (variables), or expressions inside parentheses (for sub-expressions).
        Example:
        5;          -->  This will return the number "5".
        x;          -->  This will return the identifier "x".
        (5 + 3);    --> This will return the sub-expression "5 + 3".
    */
    string parseFactor() {
        if (tokens[pos].type == T_NUM) {
            return tokens[pos++].value;
        } else if (tokens[pos].type == T_ID) {
            return tokens[pos++].value;
        } else if (tokens[pos].type == T_LPAREN) {
            expect(T_LPAREN);
            string expr = parseExpression();
            expect(T_RPAREN);
            return expr;
        } else {
            cout << "Syntax error: unexpected token '" << tokens[pos].value << "' at line " << tokens[pos].lineNum << endl;
            exit(1);
        }
    }
    /*
        expect function:
        This functin is used to check whether the current token matches the expected type.
        If the token type does not match the expected type, an error message is displayed
        and the program exits. If the token type matches, it advances the position to the next token.
    */
    void expect(TokenType type) {
    if (tokens[pos].type != type) {
        cout << "Syntax error: expected '" << type << "' at line " << tokens[pos].lineNum << endl;
        exit(1);
    }
    pos++;
}
        /* 
        Explanation:
        - The `expect` function ensures that the parser encounters the correct tokens in the expected order. 
        - It's mainly used for non-value-based tokens, such as keywords, operators, and delimiters (e.g., semicolons).
        - If the parser encounters an unexpected token, it halts the process by printing an error message, indicating where the error occurred (line number) and what was expected.
        - The `pos++` advances to the next token after confirming the expected token is present.


        Use Case:
        - This function is helpful when checking for the correct syntax or structure in a language's grammar, ensuring the parser processes the tokens in the correct order.
        */
    string expectAndReturnValue(TokenType type) {
        string value = tokens[pos].value;
        expect(type);
        return value;
    }


/*
    Why both functions are needed:
    - The `expect` function is useful when you are only concerned with ensuring the correct token type without needing its value.
    - For example, ensuring a semicolon `;` or a keyword `if` is present in the source code.
    - The `expectAndReturnValue` function is needed when the parser not only needs to check for a specific token but also needs to use the value of that token in the next stages of compilation or interpretation.
    - For example, extracting the name of a variable (`T_ID`) or the value of a constant (`T_NUMBER`) to process it in a symbol table or during expression evaluation.
*/
};


class AssemblyCodeGenerator {
private:
    vector<string> intermediateCode;
    vector<string> AssemblyCode;
    map<string, string> registerMap;
    int registerCounter = 0;

    string getNextRegister() {
        string reg = "%" + string(1, 'a' + (registerCounter % 4));
        registerCounter++;
        return reg;
    }

    string extractOperands(const string& instruction) {
        size_t equalPos = instruction.find('=');
        if (equalPos != string::npos) {
            return instruction.substr(equalPos + 1);
        }
        return instruction;
    }

public:
    AssemblyCodeGenerator(const vector<string>& intermediateCode)
        : intermediateCode(intermediateCode) {}

    void generateAssemblyCode() {
        AssemblyCode.push_back(".text");
        AssemblyCode.push_back(".globl main");
        AssemblyCode.push_back("main:");
        
        for (const auto& instr : intermediateCode) {
            if (instr.find("=") != string::npos) {
                generateAssignmentCode(instr);
            } else if (instr.find("return") != string::npos) {
                generateReturnCode(instr);
            } else if (instr.find("if") != string::npos) {
                generateConditionalCode(instr);
            }
        }

        // Default exit for x86 assembly
        AssemblyCode.push_back("    movl $0, %eax");
        AssemblyCode.push_back("    ret");
    }

    void generateAssignmentCode(const string& instruction) {
        istringstream iss(instruction);
        string dest, equalSign, source;
        iss >> dest >> equalSign;

        // Check for simple numeric assignment
        if (all_of(dest.begin(), dest.end(), ::isalpha)) {
            string operands = extractOperands(instruction);
            istringstream operandStream(operands);
            string op1, op2, operation;

            if (operandStream >> op1 >> operation >> op2) {
                // Handle arithmetic operations
                string reg1 = getNextRegister();
                string reg2 = getNextRegister();

                // Load first operand
                AssemblyCode.push_back("    movl $" + op1 + ", " + reg1);
                
                // Load second operand
                AssemblyCode.push_back("    movl $" + op2 + ", " + reg2);

                // Perform operation
                if (operation == "+") {
                    AssemblyCode.push_back("    addl " + reg2 + ", " + reg1);
                } else if (operation == "-") {
                    AssemblyCode.push_back("    subl " + reg2 + ", " + reg1);
                } else if (operation == "*") {
                    AssemblyCode.push_back("    imull " + reg2 + ", " + reg1);
                } else if (operation == "/") {
                    AssemblyCode.push_back("    movl " + reg1 + ", %eax");
                    AssemblyCode.push_back("    cdq");  // Sign extend
                    AssemblyCode.push_back("    idivl " + reg2);
                }

                // Store result
                registerMap[dest] = reg1;
            } else {
                // Simple numeric or variable assignment
                AssemblyCode.push_back("    movl $" + operands + ", %eax");
                AssemblyCode.push_back("    movl %eax, " + dest);
            }
        }
    }

    void generateReturnCode(const string& instruction) {
        string returnValue = instruction.substr(7);  // Remove "return "
        AssemblyCode.push_back("    movl $" + returnValue + ", %eax");
    }

    void generateConditionalCode(const string& instruction) {
        // Very basic conditional handling
        if (instruction.find("goto") != string::npos) {
            AssemblyCode.push_back("    # Conditional jump: " + instruction);
        }
    }

    void printAssemblyCode() {
        cout << "Generated Assembly Code (x86 Assembly):\n";
        for (const auto& line : AssemblyCode) {
            cout << line << endl;
        }
    }

    vector<string> getAssemblyCode() const {
        return AssemblyCode;
    }
};




int main() {
    string src = R"(
    int x;
    x = 10;
    int y;
    y = 20;
    int sum;
    sum = x + y * 3;
    if(5 > 3){
        x = 20;
    }
    )";
    Lexer lexer(src);
    vector<Token> tokens = lexer.tokenize();

    SymbolTable symTable;
    IntermediateCodeGnerator icg;
    Parser parser(tokens, symTable, icg);

    parser.parseProgram();
    
    // Print intermediate code
    cout << "Intermediate Code:\n";
    icg.printInstructions();
    
    // Generate Assembly code
    AssemblyCodeGenerator AssemblyCodeGen(icg.instructions);
    AssemblyCodeGen.generateAssemblyCode();
    
    // Print Assembly code
    AssemblyCodeGen.printAssemblyCode();

    return 0;
}