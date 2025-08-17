#include "./scanner.h"

#include <stdio.h>
#include <string.h>

#include "./common.h"

void initScanner(Scanner *scanner, const char *source)
{
    scanner->start = source;
    scanner->current = source;
    scanner->line = 1;
}

static bool isAlpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool isDigit(char c)
{
    return c >= '0' && c <= '9';
}

static bool isAtEnd(Scanner *scanner)
{
    return *scanner->current == '\0';
}

static char advance(Scanner *scanner)
{
    scanner->current++;
    return scanner->current[-1];
}

static char peek(Scanner *scanner)
{
    return *scanner->current;
}

static char peekNext(Scanner *scanner)
{
    if (isAtEnd(scanner)) return '\0';
    return scanner->current[1];
}

static bool match(Scanner *scanner, char expected)
{
    if (isAtEnd(scanner)) return false;
    if (*scanner->current != expected) return false;
    scanner->current++;
    return true;
}

static Token makeToken(Scanner *scanner, TokenType type)
{
    Token token;
    token.type = type;
    token.start = scanner->start;
    token.length = (int) (scanner->current - scanner->start);
    token.line = scanner->line;
    return token;
}

static Token errorToken(Scanner *scanner, const char *message)
{
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int) strlen(message);
    token.line = scanner->line;
    return token;
}

static void skipWhitespace(Scanner *scanner)
{
    for (;;)
    {
        char c = peek(scanner);
        switch (c)
        {
            case ' ':
            case '\r':
            case '\t':
                advance(scanner);
                break;
            case '/':
                if (peekNext(scanner) == '/') {
                    // A comment goes until the end of the line.
                    while (peek(scanner) != '\n' && !isAtEnd(scanner))
                    {
                        advance(scanner);
                    }
                } else if (peekNext(scanner) == '*') {
                    // A multi-line comment goes until "*/"
                    advance(scanner); // Consume the '/'
                    advance(scanner); // Consume the '*'

                    while (!isAtEnd(scanner))
                    {
                        if (peek(scanner) == '*' && peekNext(scanner) == '/')
                        {
                            advance(scanner); // Consume the '*'
                            advance(scanner); // Consume the '/'
                            break;
                        }

                        if (peek(scanner) == '\n')
                        {
                            scanner->line++;
                        }

                        advance(scanner);
                    }

                    if (isAtEnd(scanner))
                    {
                        // We reached the end of the file without finding the closing "*/"
                        // This is technically an error, but we'll just ignore it and continue
                    }
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

static TokenType checkKeyword(Scanner *scanner, int start, int length, const char *rest, TokenType type)
{
    if (scanner->current - scanner->start == start + length && memcmp(scanner->start + start, rest, length) == 0) return type;
    return TOKEN_IDENTIFIER;
}

static TokenType identifierType(Scanner *scanner)
{
    TokenType type = TOKEN_IDENTIFIER;

    switch (scanner->start[0])
    {
        case 'a': type = checkKeyword(scanner, 1, 2, "nd", TOKEN_AND); break;
        case 'b': type = checkKeyword(scanner, 1, 4, "reak", TOKEN_BREAK); break;
        case 'c':
            if (scanner->current - scanner->start > 1)
            {
                switch (scanner->start[1])
                {
                    case 'l': type = checkKeyword(scanner, 2, 3, "ass", TOKEN_CLASS); break;
                    case 'o': type = checkKeyword(scanner, 2, 6, "ntinue", TOKEN_CONTINUE); break;
                }
            }
            break;
        case 'd': type = checkKeyword(scanner, 1, 1, "o", TOKEN_DO); break;
        case 'e': type = checkKeyword(scanner, 1, 3, "lse", TOKEN_ELSE); break;
        case 'f':
            if (scanner->current - scanner->start > 1)
            {
                switch (scanner->start[1])
                {
                    case 'a': type = checkKeyword(scanner, 2, 3, "lse", TOKEN_FALSE); break;
                    case 'o': type = checkKeyword(scanner, 2, 1, "r", TOKEN_FOR); break;
                    case 'u': type = checkKeyword(scanner, 2, 2, "nc", TOKEN_FUNC); break;
                }
            }
            break;
        case 'g': type = checkKeyword(scanner, 1, 3, "oto", TOKEN_GOTO); break;
        case 'i': type = checkKeyword(scanner, 1, 1, "f", TOKEN_IF); break;
        case 'j': type = checkKeyword(scanner, 1, 2, "mp", TOKEN_JMP); break;
        case 'm': type = checkKeyword(scanner, 1, 7, "icroasm", TOKEN_MICROASM); break;
        case 'n': type = checkKeyword(scanner, 1, 3, "ull", TOKEN_NULL); break;
        case 'o': type = checkKeyword(scanner, 1, 1, "r", TOKEN_OR); break;
        case 'p': type = checkKeyword(scanner, 1, 4, "rint", TOKEN_PRINT); break;
        case 'r': type = checkKeyword(scanner, 1, 5, "eturn", TOKEN_RETURN); break;
        case 's': type = checkKeyword(scanner, 1, 4, "uper", TOKEN_SUPER); break;
        case 't':
            if (scanner->current - scanner->start > 1)
            {
                switch (scanner->start[1])
                {
                    case 'h': type = checkKeyword(scanner, 2, 2, "is", TOKEN_THIS); break;
                    case 'r': type = checkKeyword(scanner, 2, 2, "ue", TOKEN_TRUE); break;
                }
            }
            break;
        case 'v': type = checkKeyword(scanner, 1, 2, "ar", TOKEN_VAR); break;
        case 'w': type = checkKeyword(scanner, 1, 4, "hile", TOKEN_WHILE); break;
    }

    return type;
}

static Token identifier(Scanner *scanner)
{
    while (isAlpha(peek(scanner)) || isDigit(peek(scanner)))
    {
        advance(scanner);
    }
    return makeToken(scanner, identifierType(scanner));
}

static bool isBinaryDigit(char c) {
    return c == '0' || c == '1';
}

static bool isHexDigit(char c) {
    return (c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

static Token number(Scanner* scanner) {
    // Support for hex (0x) and binary (0b)
    if (scanner->current[-1] == '0') {
        if (*scanner->current == 'x' || *scanner->current == 'X') {
            advance(scanner); // consume 'x' or 'X'
            if (!isHexDigit(peek(scanner)) && peek(scanner) != '_') {
                return errorToken(scanner, "Malformed hex literal.");
            }
            while (isHexDigit(peek(scanner)) || peek(scanner) == '_') {
                if (peek(scanner) == '_') { advance(scanner); continue; }
                advance(scanner);
            }
            return makeToken(scanner, TOKEN_NUMBER);
        }
        if (*scanner->current == 'b' || *scanner->current == 'B') {
            advance(scanner); // consume 'b' or 'B'
            if (!isBinaryDigit(peek(scanner)) && peek(scanner) != '_') {
                return errorToken(scanner, "Malformed binary literal.");
            }
            while (isBinaryDigit(peek(scanner)) || peek(scanner) == '_') {
                if (peek(scanner) == '_') { advance(scanner); continue; }
                advance(scanner);
            }
            return makeToken(scanner, TOKEN_NUMBER);
        }
    }

    // Decimal integer part (support _)
    while (isDigit(peek(scanner)) || peek(scanner) == '_') {
        if (peek(scanner) == '_') { advance(scanner); continue; }
        advance(scanner);
    }

    // Fractional part (for floating-point, support _)
    if (peek(scanner) == '.' && (isDigit(peekNext(scanner)) || peekNext(scanner) == '_')) {
        advance(scanner); // consume '.'
        while (isDigit(peek(scanner)) || peek(scanner) == '_') {
            if (peek(scanner) == '_') { advance(scanner); continue; }
            advance(scanner);
        }
    }

    return makeToken(scanner, TOKEN_NUMBER);
}

static Token string(Scanner *scanner)
{
    while (peek(scanner) != '"' && !isAtEnd(scanner))
    {
        if (peek(scanner) == '\n') scanner->line++;
        advance(scanner);
    }

    if (isAtEnd(scanner)) return errorToken(scanner, "Unterminated string.");

    // The closing quote.
    advance(scanner);
    return makeToken(scanner, TOKEN_STRING);
}

Token scanToken(Scanner *scanner)
{
    skipWhitespace(scanner);
    scanner->start = scanner->current;

    if (isAtEnd(scanner)) return makeToken(scanner, TOKEN_EOF);

    char c = advance(scanner);
    if (isAlpha(c)) return identifier(scanner);
    if (isDigit(c)) return number(scanner);

    switch (c)
    {
        case '(': return makeToken(scanner, TOKEN_LEFT_PAREN);
        case ')': return makeToken(scanner, TOKEN_RIGHT_PAREN);
        case '{': return makeToken(scanner, TOKEN_LEFT_BRACE);
        case '}': return makeToken(scanner, TOKEN_RIGHT_BRACE);
        case '[': return makeToken(scanner, TOKEN_LEFT_SQUARE);
        case ']': return makeToken(scanner, TOKEN_RIGHT_SQUARE);
        case '?': return makeToken(scanner, TOKEN_QUESTION);
        case ';': return makeToken(scanner, TOKEN_SEMICOLON);
        case ':': return makeToken(scanner, TOKEN_COLON);
        case ',': return makeToken(scanner, TOKEN_COMMA);
        case '.': return makeToken(scanner, TOKEN_DOT);
        case '~': return makeToken(scanner, TOKEN_TILDE);
        case '-': return makeToken(scanner, match(scanner, '=') ? TOKEN_MINUS_EQUAL : (match(scanner, '-') ? TOKEN_MINUS_MINUS : TOKEN_MINUS));
        case '+': return makeToken(scanner, match(scanner, '=') ? TOKEN_PLUS_EQUAL : (match(scanner, '+') ? TOKEN_PLUS_PLUS : TOKEN_PLUS));
        case '/': return makeToken(scanner, match(scanner, '=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);
        case '*': return makeToken(scanner, match(scanner, '=') ? TOKEN_STAR_EQUAL : TOKEN_STAR);
        case '%': return makeToken(scanner, match(scanner, '=') ? TOKEN_PERCENT_EQUAL : TOKEN_PERCENT);
        case '!': return makeToken(scanner, match(scanner, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '=': return makeToken(scanner, match(scanner, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '&': return makeToken(scanner, match(scanner, '=') ? TOKEN_AMPERSAND_EQUAL : TOKEN_AMPERSAND);
        case '|': return makeToken(scanner, match(scanner, '=') ? TOKEN_PIPE_EQUAL : TOKEN_PIPE);
        case '^': return makeToken(scanner, match(scanner, '=') ? TOKEN_CARET_EQUAL : TOKEN_CARET);
        case '<':
            if (match(scanner, '<')) {
                return makeToken(scanner, match(scanner, '=') ? TOKEN_LESS_LESS_EQUAL : TOKEN_LESS_LESS);
            }
            return makeToken(scanner, match(scanner, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>':
            if (match(scanner, '>')) {
                return makeToken(scanner, match(scanner, '=') ? TOKEN_GREATER_GREATER_EQUAL : TOKEN_GREATER_GREATER);
            }
            return makeToken(scanner, match(scanner, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);

        case '"': return string(scanner);

        case '\n':
            scanner->line++;
            return makeToken(scanner, TOKEN_NEWLINE);
    }

    return errorToken(scanner, "Unexpected character.");
}
