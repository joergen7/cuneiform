grammar Dax;

adag           : LTAG ADAG adagProp* RTAG adagEl* LTAG SLASH ADAG RTAG ;

adagProp       : XMLNS EQ STRING                   # AdagPropXmlns
               | XSI EQ STRING                     # AdagPropXsi
               | SCHEMALOCATION EQ STRING          # AdagPropSchemaLocation
               | VERSION EQ STRING                 # AdagPropVersion
               | COUNT EQ STRING                   # AdagPropCount
               | INDEX EQ STRING                   # AdagPropIndex
               | NAME EQ STRING                    # AdagPropName
               ;

adagEl         : filename
               | job
               | child
               ;

filename       : LTAG FILENAME filenameProp* SLASH RTAG ;

filenameProp   : FILE EQ STRING                    # FilenamePropFile
               | LINK EQ INPUT                     # FilenamePropLinkInput
               | LINK EQ OUTPUT                    # FilenamePropLinkOutput
               | LINK EQ INOUT                     # FilenamePropLinkInout
               ;

job            : LTAG JOB jobProp* RTAG jobEl* LTAG SLASH JOB RTAG ;

jobProp        : ID EQ STRING                      # JobPropId
               | NAME EQ STRING                    # JobPropName
               | VERSION EQ STRING                 # JobPropVersion
               | LEVEL EQ STRING                   # JobPropLevel
               | DVNAME EQ STRING                  # JobPropDvName
               | DVVERSION EQ STRING               # JobPropDvVersion
               ;

jobEl          : LARGUMENT argumentEl* RARGUMENT   # JobElArgument
               | LTAG USES jobUsesProp* SLASH RTAG # JobElUses
               ;

argumentEl     : ARG                               # ArgumentElPlain
               | filename                          # ArgumentElFilename
               ;

jobUsesProp    : FILE EQ STRING                    # JobUsesPropFile
               | LINK EQ INPUT                     # JobUsesPropLinkInput
               | LINK EQ OUTPUT                    # JobUsesPropLinkOutput
               | REGISTER EQ TRUE                  # JobUsesPropRegisterTrue
               | REGISTER EQ FALSE                 # JobUsesPropRegisterFalse
               | TRANSFER EQ TRUE                  # JobUsesPropTransferTrue
               | TRANSFER EQ FALSE                 # JobUsesPropTransferFalse
               | TYPE EQ EXECUTABLE                # JobUsesPropExecutable
               | OPTIONAL EQ TRUE                  # JobUsesPropOptionalTrue
               | OPTIONAL EQ FALSE                 # JobUsesPropOptionalFalse
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


