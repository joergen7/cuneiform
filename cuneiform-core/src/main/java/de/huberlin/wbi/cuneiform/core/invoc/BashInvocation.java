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

package de.huberlin.wbi.cuneiform.core.invoc;

import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class BashInvocation extends Invocation {

	private static final String BASH_SHEBANG = "#!/usr/bin/env bash\n";

	public BashInvocation( Ticket ticket ) {
		super( ticket );
	}

	@Override
	protected String getShebang() {
		return BASH_SHEBANG;
	}
	
	@Override
	protected String getCheckPost() {
		return bashIfNotEquals(
			"$?",
			"0",
			"echo Task invocation returned non-zero exit value. >&2\nexit -1" );		
	}
	
	@Override
	protected String varDef( String varname, CompoundExpr list ) throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( varname ).append( "=( " );
		for( String s : list.normalize() )			
			buf.append( '"' ).append( s ).append( "\" " );
		buf.append( ")\n" );
		
		return buf.toString();

	}
	

	@Override
	protected String varDef( String varname, String value ) {
		return varname+"="+value+"\n";
	}

	private static String bashArraySize( String varName ) {
		return "${#"+varName+"[@]}";
	}
	
	private static String bashArrayGet( String varName, String index ) {
		
		String v, i;
		
		if( varName == null )
			throw new NullPointerException( "Varname must not be null." );
		
		v = varName.trim();

		if( v.isEmpty() )
			throw new RuntimeException( "Varname must not be empty." );
		
		if( v.startsWith( "$" ) )
			throw new RuntimeException( "Varname '"+varName+"' already dereferenced." );
		
		if( v.contains( "\n" ) )
			throw new RuntimeException(
				"Varname cannot be a multi-line expression." );
		
		if( index == null )
			throw new NullPointerException( "Index must not be null." );
		
		i = index.trim();
		
		if( i.isEmpty() )
			throw new RuntimeException( "Index must not be empty." );
		
		if( i.contains( "\n" ) )
			throw new RuntimeException(
				"Index cannot be a multi-line expression." );
		
		
		
		return "${"+v+"["+index+"]}";
	}
	
	private static String bashIfNotEquals( String a, String b, String thenBlock ) {
		
		if( a == null )
			throw new NullPointerException( "Operand a must not be null." );
		
		if( a.isEmpty() )
			throw new RuntimeException( "Operand a must not be empty." );
		
		if( b == null )
			throw new NullPointerException( "Operand b must not be null." );
		
		if( b.isEmpty() )
			throw new RuntimeException( "Operand b must not be empty." );
		
		if( thenBlock == null )
			throw new NullPointerException( "Then block must not be null." );
		
		if( thenBlock.isEmpty() )
			throw new RuntimeException( "Then block must not be empty." );
		
		
		String ret;
		ret = "if [ \""+a+"\" -ne \""+b+"\" ]\nthen\n"+thenBlock+"\n";
		
		ret += "fi\n";
		
		return ret;
	}
	
	private static String bashAllFrom( String varname ) {
		
		if( varname == null )
			throw new NullPointerException( "Varname must not be null." );
		
		if( varname.isEmpty() )
			throw new RuntimeException( "Varname must not be empty." );
		
		return bashArrayGet( varname, "@" );
	}
	
	@Override
	protected String ifNotFileExists( String filename, String body ) {
		
		String ret;
		
		if( filename == null )
			throw new NullPointerException( "Filename must not be null." );
		
		if( filename.isEmpty() )
			throw new RuntimeException( "Filename must not be empty." );
		
		if( body == null )
			throw new NullPointerException( "Then block must not be null." );
		
		if( body.isEmpty() )
			throw new RuntimeException( "Then block must not be empty." );
		
		
		ret = "if [ ! -e "+filename+" ]\nthen\n"+body+"\n";
		
		ret += "fi\n";
		
		return ret;
	}

	private String defFunction( String funName, String outputName, String[] inputNameList, String body ) {
		
		int i;
		
		if( funName == null )
			throw new NullPointerException( "Function name must not be null." );
		
		if( funName.isEmpty() )
			throw new RuntimeException( "Function name must not be empty." );
		
		if( body == null )
			throw new NullPointerException( "Function body must not be null." );
		
		if( body.isEmpty() )
			throw new RuntimeException( "Function body must not be empty." );
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( funName ).append( "() {\n" );
		
		i = 0;
		for( String inputName : inputNameList )
			buf.append(
				varDef( inputName, dereference( String.valueOf( ++i ) ) ) );
		
		buf.append( body );
		
		if( outputName != null )
			buf.append( "echo " ).append( dereference( outputName ) );
		
		buf.append( "\n}\n" );
		
		return buf.toString();
	}
	
	@Override
	protected String callFunction( String name, String... argValue ) {
		
		StringBuffer buf;
		
		if( name == null )
			throw new NullPointerException( "Function name must not be null." );
		
		if( name.isEmpty() )
			throw new RuntimeException( "Function name must not be empty." );
		
		if( argValue == null )
			throw new NullPointerException( "Parameter bindings must not be null." );
		
		buf = new StringBuffer();
		
		buf.append( '`' ).append( name );
		
		for( String value : argValue )
			
			if( value.trim().startsWith( "\"" ) && value.trim().endsWith( "\"" ) )
				buf.append( ' ' ).append( value );
			else
				buf.append( " \"" ).append( value ).append( '"' );
		
		
		buf.append( '`' );
		
		return buf.toString();
	}
	
	@Override
	protected String callProcedure( String name, String... argValue ) {
		
		StringBuffer buf;
		
		if( name == null )
			throw new NullPointerException( "Function name must not be null." );
		
		if( name.isEmpty() )
			throw new RuntimeException( "Function name must not be empty." );
		
		if( argValue == null )
			throw new NullPointerException( "Parameter bindings must not be null." );
		
		buf = new StringBuffer();
		
		buf.append( name );
		
		for( String value : argValue )
			
			if( value.trim().startsWith( "\"" ) && value.trim().endsWith( "\"" ) )
				buf.append( ' ' ).append( value );
			else
				buf.append( " \"" ).append( value ).append( '"' );
		
		
		buf.append( '\n' );
		
		return buf.toString();
	}
	
	@Override
	protected String defFunctionLog() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "echo -e \"{" ).append( JsonReportEntry.ATT_TIMESTAMP )
		.append( ':' ).append( System.currentTimeMillis() ).append( ',' )
		.append( JsonReportEntry.ATT_RUNID ).append( ":\\\"" )
		.append( getRunId() ).append( "\\\"," )
		.append( JsonReportEntry.ATT_TASKID ).append( ':' )
		.append( getTaskId() ).append( ',' );
		
		if( hasTaskName() )
			buf.append( JsonReportEntry.ATT_TASKNAME ).append( ":\\\"" )
			.append( getTaskName() ).append( "\\\"," );
		
		buf.append( JsonReportEntry.ATT_LANG ).append( ":\\\"" )
		.append( getLangLabel() ).append( "\\\"," )
		.append( JsonReportEntry.ATT_INVOCID ).append( ':' )
		.append( getTicketId() ).append( ',' )
		.append( JsonReportEntry.ATT_KEY ).append( ":\\\"$1\\\"," )
		.append( JsonReportEntry.ATT_VALUE ).append( ":$2}\" >> " )
		.append( REPORT_FILENAME );
		
		return defFunction( FUN_LOG, null, new String[] { "key", "payload" }, buf.toString() );
	}
	
	@Override
	protected String defFunctionLogFile() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "echo -e \"{" ).append( JsonReportEntry.ATT_TIMESTAMP )
		.append( ':' ).append( System.currentTimeMillis() ).append( ',' )
		.append( JsonReportEntry.ATT_RUNID ).append( ":\\\"" )
		.append( getRunId() ).append( "\\\"," )
		.append( JsonReportEntry.ATT_TASKID ).append( ':' )
		.append( getTaskId() ).append( ',' );
		
		if( hasTaskName() )
			buf.append( JsonReportEntry.ATT_TASKNAME ).append( ":\\\"" )
			.append( getTaskName() ).append( "\\\"," );
		
		buf.append( JsonReportEntry.ATT_LANG ).append( ":\\\"" )
		.append( getLangLabel() ).append( "\\\"," )
		.append( JsonReportEntry.ATT_INVOCID ).append( ':' )
		.append( getTicketId() ).append( ',' )
		.append( JsonReportEntry.ATT_FILE ).append( ":\\\"$1\\\"," )
		.append( JsonReportEntry.ATT_KEY ).append( ":\\\"$2\\\"," )
		.append( JsonReportEntry.ATT_VALUE ).append( ":$3}\" >> " )
		.append( REPORT_FILENAME );
		
		return defFunction( FUN_LOGFILE, null, new String[] { "key", "payload" }, buf.toString() );
	}
	
	@Override
	protected String newList( String listName ) {
		return listName+"=()\n";
	}

	@Override
	protected String listAppend( String listName, String element ) {
		return listName+"=("+bashAllFrom( listName )+" "+element+")\n";
	}

	@Override
	protected String dereference(String varName) {
		return "$"+varName;
	}

	@Override
	protected String forEach(String listName, String elementName, String body) {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "for " ).append( elementName ).append( " in "+bashAllFrom( listName )+"\ndo\n" );
		buf.append( body );
		buf.append( "\ndone\n" );
		
		return buf.toString();		
	}

	@Override
	protected String raise( String msg ) {
		return "echo "+msg+" >&2\nexit -1";
	}

	@Override
	protected String join( String... elementList ) {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		for( String element : elementList )
			buf.append( element );
		return buf.toString();
	}

	@Override
	protected String quote( String content ) {
		
		String ret;
		
		ret = "\""+content.replace( "\"", "\\\"" )+"\"";
		
		return ret;
	}

	@Override
	protected String fileSize(String filename) {
		// return "`du -b -L "+filename+" | awk '{print $1}'`";
		// return "`stat -c %s "+filename+"`";
		return "`du -k -L "+filename+" | awk '{print $1}'`";
	}

	@Override
	protected String ifListIsNotEmpty( String listName, String body ) {
		return bashIfNotEquals( bashArraySize( listName ), "0", body );
	}
	
	@Override
	protected String listToBraceCommaSeparatedString( String src, String dest, String open, String close ) {
		return dest+"=`printf \",%s\" ${"+src+"[@]}`\n"+dest+"="+open+"${"+dest+":1}"+close+"\n";
	}

	@Override
	protected String defFunctionNormalize() throws NotDerivableException {
		return defFunction(
			FUN_NORMALIZE,
			null,
			new String[] { "channel", "f" },
			"echo "+getTicketId()+"_"+"${channel}_${f##*/}\n" );
	}

	@Override
	protected String symlink( String src, String dest ) {
		// return "mv -f "+src+" "+dest+"\n";
		return "ln -s "+src+" "+dest+"\n";
	}

	@Override
	protected String getImport() {
		return "";
	}

	@Override
	protected String comment( String comment ) {
		return "# "+comment.replace( "\n", "\n# " )+"\n";
	}

	@Override
	protected String copyArray( String from, String to ) {
		return newList( to )+varDef( to, bashAllFrom( from ) );
	}

	@Override
	protected String clip(String varName) {
		return varName+"=${"+varName+":1}\n";
	}


}
