/* Please feel free to modify any content */

/* Definition section */
%{
    #include "compiler_common.h"
    #include <string.h>
    // #define YYDEBUG 1
    // int yydebug = 1;

    #define MAX_SYMBOLS 256
    #define MAX_SCOPE 10

    typedef struct {
        char name[64];
        char type[16];
        int addr;
        int lineno;
        int mut;
        char func_sig[32];
    } Symbol;

    typedef struct {
        Symbol symbols[MAX_SYMBOLS];
        int count;
        int level;
    } Scope;

    static Scope scope_stack[MAX_SCOPE];
    static int scope_top = -1;
    static int addr_counter = 0;

    extern int yylineno; /*extern means it's declared in another file*/
    extern int yylex();
    extern FILE *yyin;

    int yylex_destroy ();
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new functions if needed. */
    /* parameters and return type can be changed */
    static void create_symbol();
    static int insert_symbol(const char *name, const char *type, int addr, int lineno, const char *sig);
    static int lookup_symbol(const char *name);
    static void dump_symbol();
    static int next_addr();
    static int get_scope_level();
    static const char* get_symbol_type(const char *name);

    /* Global variables */
    bool HAS_ERROR = false;

    int is_mutable(const char* name) {
    for (int i = scope_top; i >= 0; --i) {
        for (int j = 0; j < scope_stack[i].count; ++j) {
            if (strcmp(scope_stack[i].symbols[j].name, name) == 0) {
                return scope_stack[i].symbols[j].mut;
            }
        }
    }
    return 0; 
}

%}

/* %error-verbose */
%define parse.error verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 *  - you can add new fields if needed.
*/

%union {
    int i_val;
    float f_val;
    char *s_val;
    char* type; /* i32, f32, str, bool */
}

/* Token without return */
%token LET MUT NEWLINE
%token INT FLOAT BOOL STR
%token TRUE FALSE
%token GEQ LEQ EQL NEQ LOR LAND
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN
%token IF ELSE FOR WHILE LOOP
%token PRINT PRINTLN
%token FUNC RETURN BREAK
%token ARROW AS IN DOTDOT RSHIFT LSHIFT
%token '"'

/* Token with return, which need to specify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <s_val> IDENT
%token <s_val> ID

/* Nonterminal with return, which need to specify type */
%type <s_val> Type
%type <type> VarDeclStmt PrintStmt PrintlnStmt
%type <type> Expression OrExpr AndExpr RelExpr AddExpr MulExpr AsExpr UnaryExpr Primary
%type <type> ExpressionList ArrayIndexExpr

/* Precedence */
/* 讓Bison知道 = 和 += 之類的是左結合運算子 */
%left '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN
%left LOWER_THAN_ASSIGN
%nonassoc LOWER_THAN_ELSE /* 優先把 else 配對給最接近的 if */
%nonassoc ELSE

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : GlobalStatementList
;

GlobalStatementList 
    : GlobalStatementList GlobalStatement
    | GlobalStatement
;

GlobalStatement
    : FunctionDeclStmt
    | NEWLINE
;

FunctionDeclStmt
    : FUNC ID '(' ')' {
        create_symbol();
        printf("func: %s\n", $2);
        insert_symbol($2, "func", -1, yylineno, "(V)V");
    } Block
;

StatementList
    : /* empty */
    | StatementList Statement
;

Statement
    : VarDeclStmt
    | AssignmentStmt
    | IfStmt
    | WhileStmt
    | PrintStmt
    | PrintlnStmt
    | Block
    | ExpressionStmt
;

Type
    : INT     { $$ = "i32"; }
    | FLOAT   { $$ = "f32"; }
    | STR     { $$ = "str"; }
    | '&' STR { $$ = "str"; }
    | BOOL    { $$ = "bool"; }
    | '[' Type ';' INT_LIT ']' { printf("INT_LIT %d\n", $4); $$ = "array"; }
;

VarDeclStmt
    : LET ID '=' Expression ';' {
        insert_symbol($2, $4, next_addr(), yylineno, "-");
        free($2);
    }
    | LET ID ':' Type '=' Expression ';' {
        insert_symbol($2, $4, next_addr(), yylineno, "-");
        free($2);
    }
    | LET ID ':' Type ';' {
        insert_symbol($2, $4, next_addr(), yylineno, "-");
        free($2);
    }    
    | LET MUT ID ':' Type '=' Expression ';' {
        int addr = next_addr();
        insert_symbol($3, $5, addr, yylineno, "-");
        scope_stack[scope_top].symbols[scope_stack[scope_top].count - 1].mut = 1;
        free($3);
    }
    | LET MUT ID ':' Type ';' {
        int addr = next_addr();
        insert_symbol($3, $5, addr, yylineno, "-");
        scope_stack[scope_top].symbols[scope_stack[scope_top].count - 1].mut = 1;
        free($3);
    }
    | LET MUT ID '=' Expression ';' {
        insert_symbol($3, $5, next_addr(), yylineno, "-");
        scope_stack[scope_top].symbols[scope_stack[scope_top].count - 1].mut = 1;
        free($3);
    }
;

AssignmentStmt
    : ID '=' Expression ';' {
        int ref = lookup_symbol($1);
        if (ref == -1) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
        }else {
            if (!HAS_ERROR) 
                printf("ASSIGN\n");
            if (!is_mutable($1)) {
                printf("error:%d: cannot borrow immutable borrowed content `%s` as mutable\n", yylineno, $1);
            }
        }
        free($1);
    }
    | ID ADD_ASSIGN Expression ';' {
        int ref = lookup_symbol($1);
        if (ref == -1) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
        }else {
            if (!HAS_ERROR) 
                printf("ADD_ASSIGN\n");
            if (!is_mutable($1)) {
                printf("error:%d: cannot borrow immutable borrowed content `%s` as mutable\n", yylineno, $1);
            }
        }
        free($1);
    }
    | ID SUB_ASSIGN Expression ';' {
        int ref = lookup_symbol($1);
        if (ref == -1) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
        }else {
            if (!HAS_ERROR) 
                printf("SUB_ASSIGN\n");
            if (!is_mutable($1)) {
                printf("error:%d: cannot borrow immutable borrowed content `%s` as mutable\n", yylineno, $1);
            }
        }
        free($1);
    }
    | ID MUL_ASSIGN Expression ';' {
        int ref = lookup_symbol($1);
        if (ref == -1) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
        }else {
            if (!HAS_ERROR) 
                printf("MUL_ASSIGN\n");
            if (!is_mutable($1)) {
                printf("error:%d: cannot borrow immutable borrowed content `%s` as mutable\n", yylineno, $1);
            }
        }
        free($1);
    }
    | ID DIV_ASSIGN Expression ';' {
        int ref = lookup_symbol($1);
        if (ref == -1) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
        }else {
            if (!HAS_ERROR) 
                printf("DIV_ASSIGN\n");
            if (!is_mutable($1)) {
                printf("error:%d: cannot borrow immutable borrowed content `%s` as mutable\n", yylineno, $1);
            }
        }
        free($1);
    }
    | ID REM_ASSIGN Expression ';' {
        int ref = lookup_symbol($1);
        if (ref == -1) {
            printf("error:%d: undefined: %s\n", yylineno, $1);
        }else {
            if (!HAS_ERROR) 
                printf("REM_ASSIGN\n");
            if (!is_mutable($1)) {
                printf("error:%d: cannot borrow immutable borrowed content `%s` as mutable\n", yylineno, $1);
            }
        }
        free($1);
    }
;

IfStmt
    : IF Expression Block %prec LOWER_THAN_ELSE
    | IF Expression Block ELSE Block
;

WhileStmt
    : WHILE Expression Block { ; }
;

ExpressionList
    : Expression { $$ = $1; }
    | ExpressionList ',' Expression { $$ = $1; }
;

PrintStmt
    : PRINT Expression ';' {
        if (strcmp($2, "array") == 0)
            printf("PRINT array\n");
        else
            printf("PRINT %s\n", $2);    
    }
    
;

PrintlnStmt
    : PRINTLN Expression ';' {
        if (strcmp($2, "array") == 0) {
            printf("PRINTLN array\n");
        }
        else {
            printf("PRINTLN %s\n", $2);
        }
    }
;

Block
    : '{' {
        create_symbol();    // 進入新scope時建立table
    } StatementList '}' {
        dump_symbol();      // 離開時丟出table
    }
;

ExpressionStmt
    : Expression ';'  %prec LOWER_THAN_ASSIGN
;

Expression
    : OrExpr { $$ = $1; }
;

OrExpr
    : OrExpr LOR AndExpr { printf("LOR\n"); $$ = "bool"; }
    | AndExpr { $$ = $1; }
;

AndExpr
    : AndExpr LAND RelExpr { printf("LAND\n"); $$ = "bool"; }
    | RelExpr { $$ = $1; }
;

RelExpr
    : AddExpr '>' AddExpr {
        if (strcmp($1, "undefined") == 0 || strcmp($3, "undefined") == 0) {
            printf("error:%d: invalid operation: GTR (mismatched types %s and %s)\n", yylineno, $1, $3);
        }
        printf("GTR\n");
        $$ = "bool";
    }
    | AddExpr '<' AddExpr { 
        if (strcmp($1, "undefined") == 0 || strcmp($3, "undefined") == 0) {
            printf("error:%d: invalid operation: LSS (mismatched types %s and %s)\n", yylineno, $1, $3);
        }
        printf("LSS\n"); 
        $$ = "bool"; 
    }
    | AddExpr LSHIFT AddExpr {
        if (!(strcmp($1, "i32") == 0 && strcmp($3, "i32") == 0)) {
            printf("error:%d: invalid operation: LSHIFT (mismatched types %s and %s)\n", yylineno, $1, $3);
        }
        printf("LSHIFT\n");
        $$ = "i32";
    }
    | AddExpr RSHIFT AddExpr { 
        if (!(strcmp($1, "i32") == 0 && strcmp($3, "i32") == 0)) {
            printf("error:%d: invalid operation: RSHIFT (mismatched types %s and %s)\n", yylineno, $1, $3);
        }
        printf("RSHIFT\n"); 
        $$ = "i32"; 
    }
    | AddExpr EQL AddExpr { 
        if (!(strcmp($1, "i32") == 0 && strcmp($3, "i32") == 0)) {
            printf("error:%d: invalid operation: EQL (mismatched types %s and %s)\n", yylineno, $1, $3);
        }
        printf("EQL\n");
        $$ = "bool"; 
    }
    | AddExpr NEQ AddExpr { 
        if (!(strcmp($1, "i32") == 0 && strcmp($3, "i32") == 0)) {
            printf("error:%d: invalid operation: NEQ (mismatched types %s and %s)\n", yylineno, $1, $3);
        }
        $$ = "bool"; 
        printf("NEQ\n");
    }
    | AddExpr { $$ = $1; }
;

AddExpr
    : AddExpr '+' MulExpr { printf("ADD\n"); $$ = $1; }
    | AddExpr '-' MulExpr { printf("SUB\n"); $$ = $1; }
    | MulExpr { $$ = $1; }
;

MulExpr
    : MulExpr '*' UnaryExpr { printf("MUL\n"); $$ = $1; }
    | MulExpr '/' UnaryExpr { printf("DIV\n"); $$ = $1; }
    | MulExpr '%' UnaryExpr { printf("REM\n"); $$ = $1; }
    | AsExpr
;

AsExpr
    : UnaryExpr AS Type {
        if (strcmp($1, "f32") == 0 && strcmp($3, "i32") == 0)
            printf("f2i\n");
        else if (strcmp($1, "i32") == 0 && strcmp($3, "f32") == 0)
            printf("i2f\n");
        $$ = $3;
    }
    | UnaryExpr { $$ = $1; }
;

UnaryExpr
    : '-' UnaryExpr { printf("NEG\n"); $$ = $2; }
    | '!' UnaryExpr { printf("NOT\n"); $$ = $2; }
    | Primary
;

Primary
    : '"' STRING_LIT '"' {
        $$ = "str";
        printf("STRING_LIT \"%s\"\n", $2);
        free($2);
    }
    | '"' '"' {
        $$ = "str";
        printf("STRING_LIT \"\"\n");
    }
    | INT_LIT       { printf("INT_LIT %d\n", $1); $$ = "i32"; }
    | FLOAT_LIT     { printf("FLOAT_LIT %f\n", $1); $$ = "f32"; }
    | TRUE          { printf("bool TRUE\n"); $$ = "bool"; }
    | FALSE         { printf("bool FALSE\n"); $$ = "bool"; }
    | ID {
        int ref = lookup_symbol($1);
        const char* type = get_symbol_type($1);
        if (ref == -1) {
            HAS_ERROR = true;
            printf("error:%d: undefined: %s\n", yylineno, $1);
            $$ = strdup("undefined");
        } else {
            printf("IDENT (name=%s, address=%d)\n", $1, ref);
            $$ = strdup(type);
        }
        free($1);
    }
    | ArrayIndexExpr { $$ = $1; }
    | '[' ExpressionList ']' {
        $$ = "array";
    }
    | '(' Expression ')' { $$ = $2; }
;

ArrayIndexExpr
    : ID '[' INT_LIT ']' {
        int ref = lookup_symbol($1);
        if (ref == -1) {
            HAS_ERROR = true;
            printf("error:%d: undefined variable %s\n", yylineno, $1);
        } else {
            printf("IDENT (name=%s, address=%d)\n", $1, ref);
            printf("INT_LIT %d\n", $3);
        }
        $$ = strdup("array");
        free($1);
    }
;

%%

/* C code section */
/* Symbol table and scope management (inline version) */

int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    yylineno = 1;
    yyparse();

    dump_symbol();
	printf("Total lines: %d\n", yylineno-1);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
    scope_top++;
    scope_stack[scope_top].count = 0;
    scope_stack[scope_top].level = scope_top;
    printf("> Create symbol table (scope level %d)\n", scope_top);
}

static int insert_symbol(const char *name, const char *type, int addr, int lineno, const char *sig) {
    Scope *current = &scope_stack[scope_top];
    Symbol *s = &current->symbols[current->count++];
    strcpy(s->name, name);
    strcpy(s->type, type);
    s->addr = addr;
    s->lineno = lineno;
    s->mut = 0;
    // 根據 type 判斷 mut 欄位值
    if (strcmp(type, "func") == 0) {
        s->mut = -1;
    } else {
        s->mut = 0;
    }
    strcpy(s->func_sig, sig);
    printf("> Insert `%s` (addr: %d) to scope level %d\n", name, addr, get_scope_level());
    return addr;
}

static int lookup_symbol(const char *name) {
    for (int i = scope_top; i >= 0; i--) {
        Scope *s = &scope_stack[i];
        for (int j = 0; j < s->count; j++) {
            if (strcmp(s->symbols[j].name, name) == 0) {
                return s->symbols[j].addr;
            }
        }
    }
    return -1;
}

static void dump_symbol() {
    Scope *current = &scope_stack[scope_top];
    printf("\n> Dump symbol table (scope level: %d)\n", current->level);
    printf("%-10s%-10s%-10s%-10s%-10s%-10s%-10s\n",
        "Index", "Name", "Mut", "Type", "Addr", "Lineno", "Func_sig");
    for (int i = 0; i < current->count; i++) {
        Symbol *s = &current->symbols[i];
        printf("%-10d%-10s%-10d%-10s%-10d%-10d%-10s\n",
            i, s->name, s->mut, s->type, s->addr, s->lineno, s->func_sig);
    }
    scope_top--;
}

static const char* get_symbol_type(const char *name) {
    for (int i = scope_top; i >= 0; i--) {
        Scope *s = &scope_stack[i];
        for (int j = 0; j < s->count; j++) {
            if (strcmp(s->symbols[j].name, name) == 0) {
                return s->symbols[j].type;
            }
        }
    }
    return NULL;
}

static int next_addr() {
    return addr_counter++;
}

static int get_scope_level() {
    return scope_top;
}