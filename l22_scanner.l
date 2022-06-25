%option c++ prefix="l22_scanner_" outfile="l22_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <cerrno>
#include <string>
#include <sstream>
#include <vector>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "l22_parser.tab.h"

int levelsToPop = 0;
bool closedBlock = false;
bool restart_indent = false;
int last_indent = 0;
std::vector<int> indents;

// output stream for building string literals
static std::ostringstream strlit;

bool seenIndent(int indent) {
    bool seen = false;
    int index = -1; 
    levelsToPop = index;
    for (size_t i = 0; i < indents.size(); i++) {
        if (indent == indents[i]) {
            levelsToPop = indents.size() - i - 1;
            index = i;
            seen = true;
        }
    }
    return seen;
}

// don't change this
#define yyerror LexerError
%}

SPACE [ ]

%x X_STRING X_COMMENT X_BACKSLASH X_NEWLINE X_NULL_BYTE

%%

  /*yydebug = 1;*/
  /*set_debug(1);*/

  {
    if (closedBlock) {
      closedBlock = false;
      return ';';
    }
    if (restart_indent) {
      last_indent = indents.back();
      indents.pop_back();
      closedBlock = true;
      if (indents.size() == 0) {
        restart_indent = false;
      }
      return '}';
    } else if (levelsToPop > 0) {
        levelsToPop--;
        indents.pop_back();
        closedBlock = true;
        return '}';
    }     
  }


{SPACE}*"..."\n         ;


";".*$                 ;  /* ignore comments */
"(*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"(*"        yy_push_state(X_COMMENT);
<X_COMMENT>"*)"        yy_pop_state();
<X_COMMENT>.|"\n"      ;  /* ignore inside comments */


\n                            yy_push_state(X_NEWLINE);
<X_NEWLINE>^{SPACE}+";".*$    ;  /* ignore comments */
<X_NEWLINE>^{SPACE}+"(*"      yy_push_state(X_COMMENT);

<X_NEWLINE>^{SPACE}+ {
  yy_pop_state();
  if (indents.size() == 0 || yyleng > indents.back()) {
    indents.push_back(yyleng);
    return '{';
  } else if (seenIndent(yyleng)) {
    return ';';
  } else {
    std::cerr << "bad indent at line " << yylineno;
    yyerror("");
  }
}
<X_NEWLINE>\n      ;
<X_NEWLINE>.       {
  yyless(0);
  yy_pop_state();
  if (indents.size() > 0) restart_indent = true;
  return ';';
}


"public"                 return tPUBLIC;
"use"                    return tUSE;
"foreign"                return tFOREIGN;


"if"                     return tIF;
"then:"                  return tTHEN;
"elif"                   return tELIF;
"else:"                  return tELSE;
"while"                  return tWHILE;
"do:"                    return tDO;  
"stop"                   return tSTOP;
"again"                  return tAGAIN;
"write"                  return tWRITE;
"writeln"                return tWRITELN;
"return"                 return tRETURN;


"begin"                  return tBEGIN;
"end"                    return tEND;
"sizeof"                 return tSIZEOF;
"null"                   return tNULL;


"int"                    return tTYPE_INT;
"double"                 return tTYPE_DOUBLE;
"void"                   return tTYPE_VOID;
"text"                   return tTYPE_STRING;
"var"                    return tTYPE_VAR;


"->"                   return tARROW;
"=="                   return tEQ;
"!="                   return tNE;
"<="                   return tLE;
">="                   return tGE;
"and"                  return tAND;
"not"                  return tNOT;
"or"                   return tOR;
"input"                return tINPUT;


[-+*/%=<>\[\]()@?,:]   return *yytext;


[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;


0[0-6]+                  { 
                            yylval.i = strtol(yytext, nullptr, 7); 
                            if (errno == ERANGE) 
                              yyerror("base 7 number out of range.");
                            return tINTEGER; 
                         }
0[0-9]+                  yyerror("invalid base 7 number.");
0|[1-9][0-9]*            {
                            yylval.i = strtoul(yytext, NULL, 10); 
                            if (errno == ERANGE) 
                              yyerror("base 10 number out of range.");
                            return tINTEGER; 
                         }


([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? { 
                                                    yylval.d = strtod(yytext, NULL); 
                                                    if (errno == ERANGE) 
                                                      yyerror("double out of range.");
                                                    return tDOUBLE; 
                                                  }
([0-9]+)([eE][-+]?[0-9]+)                         { 
                                                    yylval.d = strtod(yytext, NULL); 
                                                    if (errno == ERANGE) 
                                                      yyerror("double out of range.");
                                                    return tDOUBLE; 
                                                  }

"\""                           yy_push_state(X_STRING);
<X_STRING>\\                   yy_push_state(X_BACKSLASH);
<X_STRING>"\""                 {
                                 yylval.s = new std::string(strlit.str());
                                 strlit.str("");
                                 yy_pop_state();
                                 return tSTRING;
                               }
<X_STRING>.                    strlit << *yytext;

<X_NULL_BYTE>"\""              {
                                 yylval.s = new std::string(strlit.str());
                                 strlit.str("");
                                 yy_pop_state();
                                 yy_pop_state();
                                 return tSTRING;
                               }
<X_NULL_BYTE>.|\n              ;

                                  
<X_BACKSLASH>0                 yy_pop_state(); yy_push_state(X_NULL_BYTE);
<X_BACKSLASH>n                 strlit << '\n'; yy_pop_state();
<X_BACKSLASH>r                 strlit << '\r'; yy_pop_state();
<X_BACKSLASH>t                 strlit << '\t'; yy_pop_state();
<X_BACKSLASH>\"                strlit << '\"'; yy_pop_state();
<X_BACKSLASH>\\                strlit << '\\'; yy_pop_state();
<X_BACKSLASH>[0-6]{1,3}        {
                                 unsigned long num = strtoul(yytext, NULL, 7);
                                 if (errno == ERANGE) 
                                  yyerror("base 7 number out of range.");
                                 strlit << (char)(unsigned char) num;
                                 yy_pop_state();
                               }


<<EOF>>                {
  if (indents.size() > 0) {
    indents.pop_back();
    return '}';
  }
  return 0;
}


[ ]+               ; /* ignore whitespace */


.                      yyerror("Unknown character");

%%
