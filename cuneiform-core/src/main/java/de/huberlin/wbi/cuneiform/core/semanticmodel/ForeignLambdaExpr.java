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

package de.huberlin.wbi.cuneiform.core.semanticmodel;

public class ForeignLambdaExpr extends LambdaExpr {

	public static final String LANGID_BASH = "bash";
	public static final String LANGID_LISP = "lisp";
	public static final String LANGID_OCTAVE = "octave";
	public static final String LANGID_R = "r";
	public static final String LANGID_PYTHON = "python";
	public static final String LANGID_PERL = "perl";
	public static final String LANGID_SCALA = "scala";

	public static final String[] LABEL_LANG = {
		LANGID_BASH, LANGID_LISP, LANGID_OCTAVE, LANGID_R, LANGID_PYTHON, LANGID_PERL, LANGID_SCALA
	};
	
	private final int lang;
	private final String body;
	private String name;
	
	public ForeignLambdaExpr( Prototype prototype, String langLabel, String body ) {
		this( prototype, labelToInt( langLabel ), body );
	}
	
	public ForeignLambdaExpr( Prototype prototype, int lang, String body ) {

		super( prototype );
		
		if( body == null )
			throw new NullPointerException( "Body must not be null." );
		
		if( body.isEmpty() )
			throw new RuntimeException( "Body must not be empty." );
		
		if( lang < 0 || lang > 6 )
			throw new RuntimeException( "Language id "+lang+" not recognized. Must be a number in [0,6]." );
		
		this.body = body;		
		this.lang = lang;
	}
	
	public String getBody() {
		return body;
	}
	
	public int getLang() {
		return lang;
	}
	
	public String getLangLabel() {
		return LABEL_LANG[ lang ];
	}

	public long getLambdaId() {
		
		long h;
		
		h = Hash.add( getPrototype().getPrototypeId(), lang );
		h = Hash.add( h, body );
		
		return h;
	}

	public String getTaskName() {
		return name;
	}
	
	public boolean hasTaskName() {
		return name != null;
	}	
	
	public void setTaskName( String name ) {
		
		if( name == null )
			throw new NullPointerException( "Task name must not be null." );
		
		if( name.isEmpty() )
			throw new RuntimeException( "Task name must not be empty." );
		
		this.name = name;
	}
	
	@Override
	public String toString() {
		return super.toString()+"in "+intToLabel( lang )+" *{"+body+"}*";
	}
	
	public static String intToLabel( int i ) {
		return LABEL_LANG[ i ];
	}
	
	public static int labelToInt( String label ) {
		
		int i, n;
		
		n = LABEL_LANG.length;
		for( i = 0; i < n; i++ )
			if( LABEL_LANG[ i ].equals( label ) )
				return i;
		
		throw new SemanticModelException( label, "Language '"+label+"' not recognized." );
	}
	
	@Override
	public <T> T visit( NodeVisitor<? extends T> visitor ) {
		return visitor.accept( this );
	}

}
