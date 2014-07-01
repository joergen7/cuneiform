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

package de.huberlin.wbi.cuneiform.starlinger;

public class StarlingerNode extends JsonMap {

	public static final String ATT_TYPE = "type";
	public static final String ATT_ID = "id";
	public static final String ATT_NAME = "name";
	public static final String ATT_FOREIGN_SRC = "foreign-src";
	public static final String ATT_STRING_CONTENT = "string-content";
	
	public static final String LABEL_NIL = "nil";
	public static final String LABEL_APPEND = "append";
	public static final String LABEL_NATIVE_LAMBDA = "\\\\{}";
	public static final String LABEL_CURRY = "curry";
	public static final String LABEL_FOREIGN_LAMBDA = "\\\\*{}*";
	public static final String LABEL_COND = "cond";
	public static final String LABEL_APPLY = "apply";
	
	public static final String TYPE_APPEND = "Append";
	public static final String TYPE_NIL = "Nil";
	public static final String TYPE_STRING = "StringExpr";
	public static final String TYPE_APPLY = "ApplyExpr";
	public static final String TYPE_CURRY = "CurryExpr";
	public static final String TYPE_FOREIGN_LAMBDA = "ForeignLambdaExpr";
	public static final String TYPE_NATIVE_LAMBDA = "NativeLambdaExpr";
	public static final String TYPE_COND = "CondExpr";
	public static final String TYPE_NAME = "NameExpr";
	
	public static StarlingerNode createAppendNode( String id ) {
		return new StarlingerNode( id, LABEL_APPEND, TYPE_APPEND );
	}	

	public static StarlingerNode createNilNode( String id ) {
		return new StarlingerNode( id, LABEL_NIL, TYPE_NIL );
	}
	
	public static StarlingerNode createStringNode( String id, String label ) {
		
		StarlingerNode node;
		
		node = new StarlingerNode( id, label, TYPE_STRING );
		node.setStringContent( label );
		
		return node;
	}
	
	public static StarlingerNode createCurryNode( String id ) {
		return new StarlingerNode( id, LABEL_CURRY, TYPE_CURRY );
	}
	
	public static StarlingerNode createForeignLambdaNode( String id ) {
		return new StarlingerNode( id, LABEL_FOREIGN_LAMBDA, TYPE_FOREIGN_LAMBDA );
	}
	
	public static StarlingerNode createNativeLambdaNode( String id ) {
		return new StarlingerNode( id, LABEL_NATIVE_LAMBDA, TYPE_NATIVE_LAMBDA );
	}
	
	public static StarlingerNode createCondNode( String id ) {
		return new StarlingerNode( id, LABEL_COND, TYPE_COND );
	}
	
	public static StarlingerNode createApplyNode( String id ) {
		return new StarlingerNode( id, LABEL_APPLY, TYPE_APPLY );
	}
	
	public static StarlingerNode createApplyNode( String id, String label ) {
		return new StarlingerNode( id, label, TYPE_APPLY );
	}
	
	public static StarlingerNode createNameNode( String id, String label ) {
		return new StarlingerNode( id, label, TYPE_NAME );
	}
	
	
	

	public StarlingerNode( String id, String label, String type ) {
		setId( id );
		setLabel( label );
		setType( type );
	}
	
	public String getId() {
		
		String s;
		
		s = getAttribute( ATT_ID );
		
		return s.substring( 1, s.length()-1 );
	}
	
	public void setType( String type ) {		
		putAttribute( ATT_TYPE, "'"+type+"'" );
	}

	public void setLabel( String label ) {
		putAttribute( ATT_NAME, "'"+label+"'" );
	}
	
	public void setId( String id ) {
		putAttribute( ATT_ID, "'"+id+"'" );
	}
	
	public void setStringContent( String content ) {
		putAttribute( ATT_STRING_CONTENT, "'"+content+"'" );
	}
	
}
