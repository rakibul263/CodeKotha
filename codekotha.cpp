#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <map>
#include <fstream>
#include <sstream>
#include <locale>
#include <stdexcept>
#include <cmath>
#include <iomanip>

// ============================================================================
// TOKEN DEFINITIONS
// ============================================================================

enum class TokenType
{
  // Keywords (Bengali)
  KW_PURNO_SONGKHA = 0, // পূর্ণসংখ্যা (int)
  KW_JODI = 1,          // যদি (if)
  KW_NOILE = 2,         // নইলে (else)
  KW_DEKHAO = 3,        // দেখাও (print)

  // Operators
  OP_ASSIGN = 4,
  OP_GT = 5,
  OP_LT = 6,
  OP_EQ = 7,
  OP_NE = 8,
  OP_PLUS = 9,
  OP_MINUS = 10,
  OP_MULT = 11,
  OP_DIV = 12,

  // Delimiters
  SEMICOLON = 13,
  LBRACE = 14,
  RBRACE = 15,
  LPAREN = 16,
  RPAREN = 17,
  COMMA = 18,

  // Literals and identifiers
  IDENTIFIER = 19,
  INTEGER_LITERAL = 20,
  STRING_LITERAL = 21,
  FLOAT_LITERAL = 22,
  CHAR_LITERAL = 23,

  // Special tokens
  END_OF_FILE = 24,
  ERROR = 25,
  UNKNOWN = 26
};

struct Token
{
  TokenType type;
  std::wstring lexeme;
  int line, column;

  Token(TokenType t, std::wstring lex, int l = 0, int c = 0)
      : type(t), lexeme(std::move(lex)), line(l), column(c) {}
};

// ============================================================================
// LEXER
// ============================================================================

class Lexer
{
private:
  std::wstring source;
  size_t currentPos = 0;
  int currentLine = 1, currentColumn = 1;

  wchar_t peek() const { return currentPos >= source.length() ? L'\0' : source[currentPos]; }
  wchar_t advance() { return currentPos >= source.length() ? L'\0' : source[currentPos++]; }
  bool isAtEnd() const { return currentPos >= source.length(); }

  void skipWhitespace()
  {
    while (currentPos < source.length())
    {
      wchar_t ch = source[currentPos];
      if (!iswspace(ch))
        break;

      if (ch == L'\n')
      {
        currentLine++;
        currentColumn = 1;
      }
      else
      {
        currentColumn++;
      }
      currentPos++;
    }
  }

  Token readNumber()
  {
    int startColumn = currentColumn;
    std::wstring number;
    number.reserve(32); // Reserve space for typical numbers

    bool seenDot = false;
    while (currentPos < source.length())
    {
      wchar_t ch = source[currentPos];
      bool isAsciiDigit = (ch >= L'0' && ch <= L'9');
      bool isBengaliDigit = (ch >= L'০' && ch <= L'৯');
      if (ch == L'.')
      {
        if (seenDot)
          break; // stop at second dot
        seenDot = true;
        number.push_back(L'.');
        currentPos++;
        currentColumn++;
        continue;
      }
      if (!isAsciiDigit && !isBengaliDigit)
        break;

      if (isBengaliDigit)
      {
        number.push_back(L'0' + (ch - L'০'));
      }
      else
      {
        number.push_back(ch);
      }
      currentPos++;
      currentColumn++;
    }

    return Token(seenDot ? TokenType::FLOAT_LITERAL : TokenType::INTEGER_LITERAL, std::move(number), currentLine, startColumn);
  }

  Token readIdentifier()
  {
    int startColumn = currentColumn;
    std::wstring identifier;
    identifier.reserve(32); // Reserve space for typical identifiers

    while (currentPos < source.length())
    {
      wchar_t ch = source[currentPos];
      if (!iswalpha(ch) && !iswdigit(ch) && ch != L'_' && (ch < 0x0980 || ch > 0x09FF))
      {
        break;
      }
      identifier.push_back(ch);
      currentPos++;
      currentColumn++;
    }

    // Fast keyword lookup using switch on first character
    if (!identifier.empty())
    {
      switch (identifier[0])
      {
      case L'প':
        if (identifier == L"পূর্ণসংখ্যা")
          return Token(TokenType::KW_PURNO_SONGKHA, std::move(identifier), currentLine, startColumn);
        break;
      case L'য':
        if (identifier == L"যদি")
          return Token(TokenType::KW_JODI, std::move(identifier), currentLine, startColumn);
        break;
      case L'ন':
        if (identifier == L"নইলে")
          return Token(TokenType::KW_NOILE, std::move(identifier), currentLine, startColumn);
        break;
      case L'দ':
        if (identifier == L"দেখাও")
          return Token(TokenType::KW_DEKHAO, std::move(identifier), currentLine, startColumn);
        break;
      }
    }

    return Token(TokenType::IDENTIFIER, std::move(identifier), currentLine, startColumn);
  }

  Token readOperator()
  {
    int startColumn = currentColumn;
    wchar_t ch = source[currentPos++];
    currentColumn++;

    // Use static strings to avoid allocations
    static const std::wstring op_assign = L"=";
    static const std::wstring op_gt = L">";
    static const std::wstring op_lt = L"<";
    static const std::wstring op_plus = L"+";
    static const std::wstring op_minus = L"-";
    static const std::wstring op_mult = L"*";
    static const std::wstring op_div = L"/";
    static const std::wstring semicolon = L";";
    static const std::wstring lbrace = L"{";
    static const std::wstring rbrace = L"}";
    static const std::wstring lparen = L"(";
    static const std::wstring rparen = L")";
    static const std::wstring comma = L",";
    static const std::wstring unknown = L"";

    switch (ch)
    {
    case L'=':
      return Token(TokenType::OP_ASSIGN, op_assign, currentLine, startColumn);
    case L'>':
      return Token(TokenType::OP_GT, op_gt, currentLine, startColumn);
    case L'<':
      return Token(TokenType::OP_LT, op_lt, currentLine, startColumn);
    case L'+':
      return Token(TokenType::OP_PLUS, op_plus, currentLine, startColumn);
    case L'-':
      return Token(TokenType::OP_MINUS, op_minus, currentLine, startColumn);
    case L'*':
      return Token(TokenType::OP_MULT, op_mult, currentLine, startColumn);
    case L'/':
      return Token(TokenType::OP_DIV, op_div, currentLine, startColumn);
    case L';':
      return Token(TokenType::SEMICOLON, semicolon, currentLine, startColumn);
    case L'{':
      return Token(TokenType::LBRACE, lbrace, currentLine, startColumn);
    case L'}':
      return Token(TokenType::RBRACE, rbrace, currentLine, startColumn);
    case L'(':
      return Token(TokenType::LPAREN, lparen, currentLine, startColumn);
    case L')':
      return Token(TokenType::RPAREN, rparen, currentLine, startColumn);
    case L',':
      return Token(TokenType::COMMA, comma, currentLine, startColumn);
    }

    currentPos--;
    currentColumn--;
    return Token(TokenType::UNKNOWN, unknown, currentLine, startColumn);
  }

  Token readStringLiteral()
  {
    int startColumn = currentColumn;
    // consume opening quote
    advance();
    currentColumn++;
    std::wstring value;
    while (!isAtEnd())
    {
      wchar_t ch = advance();
      currentColumn++;
      if (ch == L'"')
      {
        return Token(TokenType::STRING_LITERAL, std::move(value), currentLine, startColumn);
      }
      if (ch == L'\\')
      {
        if (isAtEnd())
          break;
        wchar_t esc = advance();
        currentColumn++;
        switch (esc)
        {
        case L'\\':
          value.push_back(L'\\');
          break;
        case L'"':
          value.push_back(L'"');
          break;
        case L'n':
          value.push_back(L'\n');
          break;
        case L't':
          value.push_back(L'\t');
          break;
        case L'r':
          value.push_back(L'\r');
          break;
        default:
          value.push_back(esc);
          break;
        }
      }
      else
      {
        value.push_back(ch);
      }
    }
    return Token(TokenType::ERROR, L"Unterminated string literal", currentLine, startColumn);
  }

  Token readCharLiteral()
  {
    int startColumn = currentColumn;
    // consume opening quote
    advance();
    currentColumn++;
    if (isAtEnd())
      return Token(TokenType::ERROR, L"Unterminated char literal", currentLine, startColumn);
    wchar_t ch = advance();
    currentColumn++;
    if (ch == L'\\' && !isAtEnd())
    {
      wchar_t esc = advance();
      currentColumn++;
      switch (esc)
      {
      case L'\\':
        ch = L'\\';
        break;
      case L'\'':
        ch = L'\'';
        break;
      case L'"':
        ch = L'"';
        break;
      case L'n':
        ch = L'\n';
        break;
      case L't':
        ch = L'\t';
        break;
      case L'r':
        ch = L'\r';
        break;
      default:
        ch = esc;
        break;
      }
    }
    if (isAtEnd() || advance() != L'\'')
    {
      return Token(TokenType::ERROR, L"Unterminated char literal", currentLine, startColumn);
    }
    currentColumn++;
    std::wstring w(1, ch);
    return Token(TokenType::CHAR_LITERAL, std::move(w), currentLine, startColumn);
  }

public:
  Lexer(const std::wstring &sourceCode) : source(sourceCode) {}

  std::vector<Token> tokenize()
  {
    std::vector<Token> tokens;
    tokens.reserve(source.length() / 4); // Estimate token count

    currentPos = 0;
    currentLine = 1;
    currentColumn = 1;

    while (currentPos < source.length())
    {
      skipWhitespace();
      if (currentPos >= source.length())
        break;

      wchar_t ch = source[currentPos];

      if (ch == L'"')
      {
        tokens.emplace_back(readStringLiteral());
      }
      else if (ch == L'\'')
      {
        tokens.emplace_back(readCharLiteral());
      }
      else if (iswdigit(ch) || (ch >= L'০' && ch <= L'৯'))
      {
        tokens.emplace_back(readNumber());
      }
      else if (iswalpha(ch) || ch == L'_' || (ch >= 0x0980 && ch <= 0x09FF))
      {
        tokens.emplace_back(readIdentifier());
      }
      else
      {
        Token token = readOperator();
        if (token.type != TokenType::UNKNOWN)
        {
          tokens.emplace_back(std::move(token));
        }
        else
        {
          std::wcerr << L"Unexpected character: " << ch << L" at line " << currentLine << L", column " << currentColumn << std::endl;
          currentPos++;
          currentColumn++;
        }
      }
    }

    static const std::wstring eof_lexeme = L"";
    tokens.emplace_back(TokenType::END_OF_FILE, eof_lexeme, currentLine, currentColumn);
    return tokens;
  }
};

// ============================================================================
// AST NODES
// ============================================================================

struct Expression
{
  virtual ~Expression() = default;
};

struct IntegerLiteral : Expression
{
  int value;
  IntegerLiteral(int v) : value(v) {}
};

struct FloatLiteral : Expression
{
  double value;
  explicit FloatLiteral(double v) : value(v) {}
};

struct StringLiteral : Expression
{
  std::wstring value;
  explicit StringLiteral(std::wstring v) : value(std::move(v)) {}
};

struct CharLiteral : Expression
{
  wchar_t value;
  explicit CharLiteral(wchar_t v) : value(v) {}
};

struct Identifier : Expression
{
  std::wstring name;
  Identifier(const std::wstring &n) : name(n) {}
};

struct BinaryExpression : Expression
{
  std::unique_ptr<Expression> left, right;
  TokenType op;
  BinaryExpression(std::unique_ptr<Expression> l, std::unique_ptr<Expression> r, TokenType o)
      : left(std::move(l)), right(std::move(r)), op(o) {}
};

struct Statement
{
  virtual ~Statement() = default;
};

struct VariableDeclaration : Statement
{
  std::wstring name;
  std::unique_ptr<Expression> initializer;
  VariableDeclaration(const std::wstring &n, std::unique_ptr<Expression> init)
      : name(n), initializer(std::move(init)) {}
};

struct Assignment : Expression
{
  std::wstring name;
  std::unique_ptr<Expression> value;
  Assignment(const std::wstring &n, std::unique_ptr<Expression> v)
      : name(n), value(std::move(v)) {}
};

struct PrintStatement : Statement
{
  std::vector<std::unique_ptr<Expression>> expressions;
  PrintStatement(std::vector<std::unique_ptr<Expression>> exprs) : expressions(std::move(exprs)) {}
};

struct IfStatement : Statement
{
  std::unique_ptr<Expression> condition;
  std::vector<std::unique_ptr<Statement>> thenBlock, elseBlock;
  IfStatement(std::unique_ptr<Expression> cond) : condition(std::move(cond)) {}
};

struct Program
{
  std::vector<std::unique_ptr<Statement>> statements;
};

// ============================================================================
// PARSER
// ============================================================================

class Parser
{
private:
  std::vector<Token> tokens;
  size_t current = 0;
  int errorCount = 0;

  Token peek() const { return current >= tokens.size() ? tokens.back() : tokens[current]; }
  Token advance() { return current >= tokens.size() ? tokens.back() : tokens[current++]; }
  bool isAtEnd() const { return current >= tokens.size(); }
  bool match(TokenType type)
  {
    if (peek().type == type)
    {
      advance();
      return true;
    }
    return false;
  }

  std::unique_ptr<Expression> parseExpression()
  {
    return parseAssignment();
  }

  std::unique_ptr<Expression> parseAssignment()
  {
    auto expr = parseEquality();

    if (match(TokenType::OP_ASSIGN))
    {
      auto value = parseAssignment();
      if (auto id = dynamic_cast<Identifier *>(expr.get()))
      {
        return std::make_unique<Assignment>(id->name, std::move(value));
      }
    }

    return expr;
  }

  std::unique_ptr<Expression> parseEquality()
  {
    auto expr = parseComparison();

    while (match(TokenType::OP_EQ) || match(TokenType::OP_NE))
    {
      TokenType op = tokens[current - 1].type;
      auto right = parseComparison();
      expr = std::make_unique<BinaryExpression>(std::move(expr), std::move(right), op);
    }

    return expr;
  }

  std::unique_ptr<Expression> parseComparison()
  {
    auto expr = parseTerm();

    while (match(TokenType::OP_GT) || match(TokenType::OP_LT))
    {
      TokenType op = tokens[current - 1].type;
      auto right = parseTerm();
      expr = std::make_unique<BinaryExpression>(std::move(expr), std::move(right), op);
    }

    return expr;
  }

  std::unique_ptr<Expression> parseTerm()
  {
    auto expr = parseFactor();

    while (match(TokenType::OP_PLUS) || match(TokenType::OP_MINUS))
    {
      TokenType op = tokens[current - 1].type;
      // Debug output removed for cleaner execution
      auto right = parseFactor();
      expr = std::make_unique<BinaryExpression>(std::move(expr), std::move(right), op);
    }

    return expr;
  }

  std::unique_ptr<Expression> parseFactor()
  {
    auto expr = parsePrimary();

    while (match(TokenType::OP_MULT) || match(TokenType::OP_DIV))
    {
      TokenType op = tokens[current - 1].type;
      auto right = parsePrimary();
      expr = std::make_unique<BinaryExpression>(std::move(expr), std::move(right), op);
    }

    return expr;
  }

  std::unique_ptr<Expression> parsePrimary()
  {
    if (match(TokenType::FLOAT_LITERAL))
    {
      return std::make_unique<FloatLiteral>(std::stod(tokens[current - 1].lexeme));
    }
    if (match(TokenType::INTEGER_LITERAL))
    {
      return std::make_unique<IntegerLiteral>(std::stoi(tokens[current - 1].lexeme));
    }
    if (match(TokenType::STRING_LITERAL))
    {
      return std::make_unique<StringLiteral>(tokens[current - 1].lexeme);
    }
    if (match(TokenType::CHAR_LITERAL))
    {
      const std::wstring &w = tokens[current - 1].lexeme;
      return std::make_unique<CharLiteral>(w.empty() ? L'\0' : w[0]);
    }

    if (match(TokenType::IDENTIFIER))
    {
      return std::make_unique<Identifier>(tokens[current - 1].lexeme);
    }

    if (match(TokenType::LPAREN))
    {
      auto expr = parseExpression();
      if (!match(TokenType::RPAREN))
      {
        std::wcerr << L"Expected ')'" << std::endl;
      }
      return expr;
    }

    std::wcerr << L"Unexpected token: " << peek().lexeme << std::endl;
    return std::make_unique<IntegerLiteral>(0);
  }

  std::unique_ptr<Statement> parseStatement()
  {
    if (match(TokenType::KW_PURNO_SONGKHA))
    {
      if (peek().type != TokenType::IDENTIFIER)
      {
        std::wcerr << L"Expected identifier after পূর্ণসংখ্যা" << std::endl;
        errorCount++;
        return nullptr;
      }

      std::wstring name = advance().lexeme;

      if (!match(TokenType::OP_ASSIGN))
      {
        std::wcerr << L"Expected '=' after variable name" << std::endl;
        errorCount++;
        return nullptr;
      }

      auto initializer = parseExpression();

      if (!match(TokenType::SEMICOLON))
      {
        std::wcerr << L"Expected ';' after variable declaration" << std::endl;
        errorCount++;
      }

      return std::make_unique<VariableDeclaration>(name, std::move(initializer));
    }

    if (match(TokenType::KW_DEKHAO))
    {
      if (!match(TokenType::LPAREN))
      {
        std::wcerr << L"Expected '(' after দেখাও" << std::endl;
        errorCount++;
        return nullptr;
      }

      std::vector<std::unique_ptr<Expression>> expressions;
      expressions.push_back(parseExpression());

      // Parse additional expressions separated by commas
      while (peek().type == TokenType::COMMA)
      {
        advance(); // consume comma
        expressions.push_back(parseExpression());
      }

      if (!match(TokenType::RPAREN))
      {
        std::wcerr << L"Expected ')' after expression" << std::endl;
        errorCount++;
      }

      if (!match(TokenType::SEMICOLON))
      {
        std::wcerr << L"Expected ';' after print statement" << std::endl;
        errorCount++;
      }

      return std::make_unique<PrintStatement>(std::move(expressions));
    }

    if (match(TokenType::KW_JODI))
    {
      if (!match(TokenType::LPAREN))
      {
        std::wcerr << L"Expected '(' after যদি" << std::endl;
        errorCount++;
        return nullptr;
      }

      auto condition = parseExpression();

      if (!match(TokenType::RPAREN))
      {
        std::wcerr << L"Expected ')' after condition" << std::endl;
        errorCount++;
      }

      if (!match(TokenType::LBRACE))
      {
        std::wcerr << L"Expected '{' after condition" << std::endl;
        errorCount++;
      }

      auto ifStmt = std::make_unique<IfStatement>(std::move(condition));

      while (peek().type != TokenType::RBRACE && !isAtEnd())
      {
        ifStmt->thenBlock.push_back(parseStatement());
      }

      if (!match(TokenType::RBRACE))
      {
        std::wcerr << L"Expected '}' after if block" << std::endl;
        errorCount++;
      }

      if (match(TokenType::KW_NOILE))
      {
        if (!match(TokenType::LBRACE))
        {
          std::wcerr << L"Expected '{' after নইলে" << std::endl;
          errorCount++;
        }

        while (peek().type != TokenType::RBRACE && !isAtEnd())
        {
          ifStmt->elseBlock.push_back(parseStatement());
        }

        if (!match(TokenType::RBRACE))
        {
          std::wcerr << L"Expected '}' after else block" << std::endl;
          errorCount++;
        }
      }

      return ifStmt;
    }

    std::wcerr << L"Unexpected statement: " << peek().lexeme << std::endl;
    errorCount++;
    return nullptr;
  }

public:
  Parser(std::vector<Token> t) : tokens(std::move(t)) {}

  void synchronize()
  {
    if (!isAtEnd())
      advance();
    while (!isAtEnd())
    {
      TokenType t = peek().type;
      if (t == TokenType::SEMICOLON || t == TokenType::RBRACE)
      {
        advance();
        break;
      }
      if (t == TokenType::KW_PURNO_SONGKHA || t == TokenType::KW_DEKHAO || t == TokenType::KW_JODI || t == TokenType::KW_NOILE)
      {
        break;
      }
      advance();
    }
  }

  std::unique_ptr<Program> parse()
  {
    auto program = std::make_unique<Program>();

    while (!isAtEnd() && peek().type != TokenType::END_OF_FILE)
    {
      size_t before = current;
      auto stmt = parseStatement();
      if (stmt)
      {
        program->statements.push_back(std::move(stmt));
      }
      else
      {
        synchronize();
      }
      if (current == before)
      {
        std::wcerr << L"Parser made no progress, aborting to avoid infinite loop." << std::endl;
        break;
      }
    }

    return program;
  }
};

// ============================================================================
// CODE GENERATOR
// ============================================================================

class CodeGenerator
{
private:
  std::string cCode;
  int tempCounter = 0;
  int labelCounter = 0;
  std::map<std::wstring, int> variables; // Variable storage
  std::vector<std::string> outputLines;  // Store actual program output

  std::string newTemp() { return "t" + std::to_string(tempCounter++); }
  std::string newLabel() { return "L" + std::to_string(labelCounter++); }

  static void appendUtf8ForCodepoint(uint32_t cp, std::string &out)
  {
    if (cp <= 0x7F)
    {
      out.push_back(static_cast<char>(cp));
    }
    else if (cp <= 0x7FF)
    {
      out.push_back(static_cast<char>(0xC0 | ((cp >> 6) & 0x1F)));
      out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    }
    else if (cp <= 0xFFFF)
    {
      out.push_back(static_cast<char>(0xE0 | ((cp >> 12) & 0x0F)));
      out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
      out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    }
    else
    {
      out.push_back(static_cast<char>(0xF0 | ((cp >> 18) & 0x07)));
      out.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3F)));
      out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
      out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    }
  }

  static std::string toUtf8(const std::wstring &w)
  {
    std::string out;
    out.reserve(w.size() * 3);
    for (wchar_t wc : w)
    {
      appendUtf8ForCodepoint(static_cast<uint32_t>(wc), out);
    }
    return out;
  }

  static std::string toUtf8(wchar_t wc)
  {
    std::string out;
    appendUtf8ForCodepoint(static_cast<uint32_t>(wc), out);
    return out;
  }

  int evaluateExpression(Expression *expr)
  {
    if (auto lit = dynamic_cast<IntegerLiteral *>(expr))
    {
      return lit->value;
    }
    if (auto fl = dynamic_cast<FloatLiteral *>(expr))
    {
      return static_cast<int>(std::llround(fl->value));
    }
    if (auto ch = dynamic_cast<CharLiteral *>(expr))
    {
      return static_cast<int>(ch->value);
    }

    if (auto id = dynamic_cast<Identifier *>(expr))
    {
      // Look up variable value in the variable table
      auto it = variables.find(id->name);
      if (it != variables.end())
      {
        return it->second;
      }
      else
      {
        std::wcerr << L"Warning: Variable '" << id->name << L"' not found, using 0" << std::endl;
        return 0;
      }
    }

    if (auto bin = dynamic_cast<BinaryExpression *>(expr))
    {
      int left = evaluateExpression(bin->left.get());
      int right = evaluateExpression(bin->right.get());

      // Debug output removed for cleaner execution

      int result = 0;
      switch (bin->op)
      {
      case TokenType::OP_PLUS:
        result = left + right;
        break;
      case TokenType::OP_MINUS:
        result = left - right;
        break;
      case TokenType::OP_MULT:
        result = left * right;
        break;
      case TokenType::OP_DIV:
        result = right != 0 ? left / right : 0;
        break;
      case TokenType::OP_GT:
        result = left > right ? 1 : 0;
        break;
      case TokenType::OP_LT:
        result = left < right ? 1 : 0;
        break;
      case TokenType::OP_EQ:
        result = left == right ? 1 : 0;
        break;
      case TokenType::OP_NE:
        result = left != right ? 1 : 0;
        break;
      default:
        result = 0;
        break;
      }

      return result;
    }

    return 0;
  }

  void executeStatement(Statement *stmt)
  {
    if (auto decl = dynamic_cast<VariableDeclaration *>(stmt))
    {
      int value = evaluateExpression(decl->initializer.get());
      variables[decl->name] = value; // Store variable value
      // Remove terminal output for variable declaration
    }

    if (auto print = dynamic_cast<PrintStatement *>(stmt))
    {
      std::string outputLine = "";
      for (size_t i = 0; i < print->expressions.size(); ++i)
      {
        Expression *e = print->expressions[i].get();
        if (i > 0)
        {
          outputLine += " ";
        }
        if (auto s = dynamic_cast<StringLiteral *>(e))
        {
          outputLine += toUtf8(s->value);
        }
        else if (auto ch = dynamic_cast<CharLiteral *>(e))
        {
          outputLine += toUtf8(ch->value);
        }
        else if (auto fl = dynamic_cast<FloatLiteral *>(e))
        {
          std::ostringstream oss;
          oss.setf(std::ios::fixed);
          oss << std::setprecision(6) << fl->value; // default precision
          outputLine += oss.str();
        }
        else
        {
          int value = evaluateExpression(e);
          outputLine += std::to_string(value);
        }
      }
      outputLines.push_back(outputLine); // Store the output
    }

    if (auto ifStmt = dynamic_cast<IfStatement *>(stmt))
    {
      int condition = evaluateExpression(ifStmt->condition.get());

      if (condition)
      {
        for (auto &stmt : ifStmt->thenBlock)
        {
          executeStatement(stmt.get());
        }
      }
      else
      {
        for (auto &stmt : ifStmt->elseBlock)
        {
          executeStatement(stmt.get());
        }
      }
    }
  }

public:
  void generate(Program *program)
  {
    cCode.clear();

    for (auto &stmt : program->statements)
    {
      executeStatement(stmt.get());
    }
  }

  void generateC(Program *program)
  {
    cCode = "#include <stdio.h>\n#include <stdlib.h>\n\nint main() {\n";

    for (auto &stmt : program->statements)
    {
      if (auto decl = dynamic_cast<VariableDeclaration *>(stmt.get()))
      {
        std::string name(decl->name.begin(), decl->name.end());
        int value = evaluateExpression(decl->initializer.get());
        cCode += "    int " + name + " = " + std::to_string(value) + ";\n";
      }

      if (auto print = dynamic_cast<PrintStatement *>(stmt.get()))
      {
        for (size_t i = 0; i < print->expressions.size(); ++i)
        {
          int value = evaluateExpression(print->expressions[i].get());
          cCode += "    printf(\"%d\\n\", " + std::to_string(value) + ");\n";
        }
      }

      if (auto ifStmt = dynamic_cast<IfStatement *>(stmt.get()))
      {
        int condition = evaluateExpression(ifStmt->condition.get());
        std::string trueLabel = newLabel();
        std::string endLabel = newLabel();

        cCode += "    if (" + std::to_string(condition) + ") goto " + trueLabel + ";\n";

        // Generate else block
        for (auto &stmt : ifStmt->elseBlock)
        {
          if (auto print = dynamic_cast<PrintStatement *>(stmt.get()))
          {
            for (size_t i = 0; i < print->expressions.size(); ++i)
            {
              int value = evaluateExpression(print->expressions[i].get());
              cCode += "    printf(\"%d\\n\", " + std::to_string(value) + ");\n";
            }
          }
        }

        cCode += "    goto " + endLabel + ";\n";
        cCode += "    " + trueLabel + ":\n";

        // Generate then block
        for (auto &stmt : ifStmt->thenBlock)
        {
          if (auto print = dynamic_cast<PrintStatement *>(stmt.get()))
          {
            for (size_t i = 0; i < print->expressions.size(); ++i)
            {
              int value = evaluateExpression(print->expressions[i].get());
              cCode += "    printf(\"%d\\n\", " + std::to_string(value) + ");\n";
            }
          }
        }

        cCode += "    " + endLabel + ":\n";
      }
    }

    cCode += "    return 0;\n}\n";
  }

  void printC()
  {
    std::cout << "Generated C Code:\n================\n"
              << cCode;
  }

  void writeToFile(const std::string &filename, bool generateC)
  {
    std::ofstream file(filename);
    if (generateC)
    {
      file << "// Generated C Code from BangLang\n";
      file << "// ==============================\n\n";
      file << cCode;
    }
    else
    {
      // Show actual program output
      for (const auto &line : outputLines)
      {
        file << line << "\n";
      }
    }
  }
};

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

std::wstring readFileAsWString(const std::string &filename)
{
  std::ifstream file(filename, std::ios::binary);
  if (!file.is_open())
  {
    throw std::runtime_error("Could not open file: " + filename);
  }

  file.seekg(0, std::ios::end);
  size_t size = file.tellg();
  file.seekg(0, std::ios::beg);

  std::string content(size, '\0');
  file.read(&content[0], size);

  std::wstring result;
  result.reserve(size);

  const unsigned char *data = reinterpret_cast<const unsigned char *>(content.data());
  const size_t len = content.size();

  for (size_t i = 0; i < len;)
  {
    unsigned char byte = data[i];

    if (byte < 0x80)
    {
      result.push_back(static_cast<wchar_t>(byte));
      ++i;
    }
    else if ((byte & 0xE0) == 0xC0 && i + 1 < len)
    {
      unsigned char byte2 = data[i + 1];
      if ((byte2 & 0xC0) == 0x80)
      {
        result.push_back(((byte & 0x1F) << 6) | (byte2 & 0x3F));
        i += 2;
      }
      else
      {
        result.push_back(L'?');
        ++i;
      }
    }
    else if ((byte & 0xF0) == 0xE0 && i + 2 < len)
    {
      unsigned char byte2 = data[i + 1];
      unsigned char byte3 = data[i + 2];
      if ((byte2 & 0xC0) == 0x80 && (byte3 & 0xC0) == 0x80)
      {
        result.push_back(((byte & 0x0F) << 12) | ((byte2 & 0x3F) << 6) | (byte3 & 0x3F));
        i += 3;
      }
      else
      {
        result.push_back(L'?');
        ++i;
      }
    }
    else
    {
      result.push_back(L'?');
      ++i;
    }
  }

  return result;
}

// ============================================================================
// MAIN FUNCTION
// ============================================================================

int main(int argc, char *argv[])
{
  std::locale::global(std::locale(""));

  std::string inputFile = "input.txt";   // Default input file
  std::string outputFile = "output.txt"; // Default output file
  bool generateC = false;

  // Parse command line arguments
  for (int i = 1; i < argc; ++i)
  {
    std::string arg = argv[i];
    if (arg == "-c")
    {
      generateC = true;
    }
    else if (arg == "-o" && i + 1 < argc)
    {
      outputFile = argv[++i];
    }
    else if (arg == "-i" && i + 1 < argc)
    {
      inputFile = argv[++i];
    }
    else if (arg == "-h" || arg == "--help")
    {
      std::cout << "BangLang Compiler (Compact Version)\n";
      std::cout << "Usage: " << argv[0] << " [-i input_file] [-o output_file] [-c]\n";
      std::cout << "  -i: Input file (default: input.txt)\n";
      std::cout << "  -o: Output file (default: output.txt)\n";
      std::cout << "  -c: Generate C code instead of 3-address code\n";
      std::cout << "  -h: Show this help message\n";
      return 0;
    }
  }

  try
  {
    // Lexical Analysis
    std::wstring sourceCode = readFileAsWString(inputFile);
    Lexer lexer(sourceCode);
    auto tokens = lexer.tokenize();

    // Syntax Analysis with error guard
    Parser parser(tokens);
    auto program = parser.parse();

    // If program has no statements and tokens were non-empty, fail gracefully
    if (program->statements.empty() && tokens.size() > 1)
    {
      std::cerr << "Compilation error: No valid statements parsed. Aborting to avoid infinite loop." << std::endl;
      return 1;
    }

    // Code Generation
    CodeGenerator codeGen;
    codeGen.generate(program.get());

    if (generateC)
    {
      codeGen.generateC(program.get());
    }

    codeGen.writeToFile(outputFile, generateC);
  }
  catch (const std::exception &e)
  {
    std::cerr << "✗ Compilation failed: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}
