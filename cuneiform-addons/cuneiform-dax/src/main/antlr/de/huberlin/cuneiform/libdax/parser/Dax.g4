/*******************************************************************************
 * In the Hi-WAY project we propose a novel approach of executing scientific
 * workflows processing Big Data, as found in NGS applications, on distributed
 * computational infrastructures. The Hi-WAY software stack comprises the func-
 * tional workflow language Cuneiform as well as the Hi-WAY ApplicationMaster
 * for Apache Hadoop 2.x (YARN).
 *
 * List of Contributors:
 *
 * Jörgen Brandt (HU Berlin)
 * Marc Bux (HU Berlin)
 * Ulf Leser (HU Berlin)
 *
 * Jörgen Brandt is funded by the European Commission through the BiobankCloud
 * project. Marc Bux is funded by the Deutsche Forschungsgemeinschaft through
 * research training group SOAMED (GRK 1651).
 *
 * Copyright 2014 Humboldt-Universität zu Berlin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

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

filename       : LTAG( FILE | FILENAME )filenameProp* SLASH RTAG ;

filenameProp   : ( FILE | NAME )EQ STRING          # FilenamePropFile
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
               | NAMESPACE EQ STRING               # JobPropNamespace
               ;

jobEl          : LARGUMENT argumentEl* RARGUMENT   # JobElArgument
               | LTAG USES jobUsesProp* SLASH RTAG # JobElUses
               ;

argumentEl     : ARG                               # ArgumentElPlain
               | filename                          # ArgumentElFilename
               ;

jobUsesProp    : ( FILE | NAME )EQ STRING          # JobUsesPropFile
               | LINK EQ INPUT                     # JobUsesPropLinkInput
               | LINK EQ OUTPUT                    # JobUsesPropLinkOutput
               | REGISTER EQ( TRUE | FALSE )       # JobUsesPropRegister
               | TRANSFER EQ( TRUE | FALSE )       # JobUsesPropTransfer
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
NAMESPACE      : 'namespace' ;
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


