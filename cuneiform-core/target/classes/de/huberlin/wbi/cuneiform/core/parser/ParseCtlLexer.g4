lexer grammar ParseCtlLexer;

// LEXER RULES

RC        : '#' -> mode( COMMENT ) ;
OC        : '%' -> mode( COMMENT ) ;
CC        : '//' -> mode( COMMENT ) ;
LCCOMMENT : '/*' -> mode( CCOMMENT ) ;
LPAREN    : '(' -> mode( APPLY ) ;
LXCOMMENT : '<!--' -> mode( XCOMMENT ) ;
LMMECB    : '*{' -> mode( FOREIGN ) ;

LBRACE    : '{' ;
RBRACE    : '}' ;
SEMICOLON : ';' ;

WS        : [ \n\r\t|] -> skip ;
ANY       : . ;

mode COMMENT ;

NEWLINE   : [\n\r] -> mode( DEFAULT_MODE ) ;
ANYC      : . -> skip ;


mode CCOMMENT ;

RCCOMMENT : '*/' -> mode( DEFAULT_MODE ) ;
ANYCC     : . -> skip ;


mode XCOMMENT ;

RXCOMMENT : '-->' -> mode( DEFAULT_MODE ) ;
ANYXC     : . -> skip ;


mode APPLY ;

RPAREN    : ')' -> mode( DEFAULT_MODE ) ;
ANYA      : . -> skip ;

mode FOREIGN ;

RMMECB    : '}*' -> mode( DEFAULT_MODE ) ;
ANYF      : . -> skip ;