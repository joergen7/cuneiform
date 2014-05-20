grammar Dax;

adag           : LTAG ADAG adagProp* RTAG adagEl* LTAG SLASH ADAG RTAG ;

adagProp       : XMLNS EQ STRING
               | XSI EQ STRING
               | SCHEMALOCATION EQ STRING
               | VERSION EQ STRING
               | COUNT EQ STRING
               | INDEX EQ STRING
               | NAME EQ STRING
               ;

adagEl         : filename
               | job
               | child
               ;

filename       : LTAG FILENAME filenameProp* SLASH RTAG ;

filenameProp   : FILE EQ STRING                    # FilenamePropFile
               | LINK EQ( INPUT | OUTPUT | INOUT ) # FilenamePropLink
               ;

job            : LTAG JOB jobProp* RTAG jobEl* LTAG SLASH JOB RTAG ;

jobProp        : ID EQ STRING                      # JobPropId
               | NAME EQ STRING                    # JobPropName
               | VERSION EQ STRING                 # JobPropVersion
               | LEVEL EQ STRING                   # JobPropLevel
               | DVNAME EQ STRING                  # JobPropDvname
               | DVVERSION EQ STRING               # JobPropDvversion
               ;

jobEl          : argument
               | LTAG USES jobUsesProp* SLASH RTAG
               ;

argument       : LARGUMENT argumentEl* RARGUMENT ;

argumentEl     : ARG                               # ArgumentElPlain
               | filename                          # ArgumentElFilename
               ;

jobUsesProp    : FILE EQ STRING
               | LINK EQ( INPUT | OUTPUT )
               | REGISTER EQ( TRUE | FALSE )
               | TRANSFER EQ( TRUE | FALSE )
               | TYPE EQ EXECUTABLE
               | OPTIONAL EQ( TRUE | FALSE )
               ;

child          : LTAG CHILD REF EQ STRING RTAG parent+ LTAG SLASH CHILD RTAG ;

parent         : LTAG PARENT REF EQ STRING SLASH RTAG ;

ADAG           : 'adag' ;
ARGUMENT       : 'argument' ;
CHILD          : 'child' ;
COUNT          : 'count' ;
DVNAME         : 'dv-name' ;
DVVERSION      : 'dv-version' ;
EQ             : '=' ;
EXECUTABLE     : '"executable"' ;
FALSE          : '"false"' ;
FILE           : 'file' ;
FILENAME       : 'filename' ;
ID             : 'id' ;
INDEX          : 'index' ;
INOUT          : '"inout"' ;
INPUT          : '"input"' ;
JOB            : 'job' ;
LARGUMENT      : '<argument>' ;
LEVEL          : 'level' ;
LINK           : 'link' ;
LTAG           : '<' ;
NAME           : 'name' ;
OPTIONAL       : 'optional' ;
OUTPUT         : '"output"' ;
PARENT         : 'parent' ;
RARGUMENT      : '</argument>' ;
REF            : 'ref' ;
REGISTER       : 'register' ;
RTAG           : '>' ;
SCHEMALOCATION : 'xsi:schemaLocation' ;
SLASH          : '/' ;
TRANSFER       : 'transfer' ;
TRUE           : '"true"' ;
TYPE           : 'type' ;
USES           : 'uses' ;
VERSION        : 'version' ;
XMLNS          : 'xmlns' ;
XSI            : 'xmlns:xsi' ;

STRING         : '"' .*? '"' ;
ARG            : [a-zA-Z0-9\-\.]+ ;

METAINFO       : '<?' .*? '?>' -> skip ;
COMMENT        : '<!--' .*? '-->' -> skip ;
WS             : [ \t\r\n] -> skip ;


