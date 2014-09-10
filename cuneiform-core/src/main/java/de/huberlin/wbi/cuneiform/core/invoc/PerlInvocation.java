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

import java.util.List;

import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class PerlInvocation extends Invocation {

	public PerlInvocation( Ticket ticket ) {
		super( ticket );
	}

	@Override
	protected String callFunction( String name, String... argValue ) {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( "&" + name ).append( "( " );
		comma = false;
		for( String arg : argValue ) {

			if( comma )
				buf.append( ", " );
			comma = true;
			
			buf.append( arg );
		}
		
		buf.append( " )" );
		
		return buf.toString();
	}

	@Override
	protected String callProcedure( String name, String... argValue ) {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( "&" + name ).append( "( " );
		comma = false;
		for( String arg : argValue ) {

			if( comma )
				buf.append( ", " );
			comma = true;
			
			buf.append( arg );
		}
		
		buf.append( " );\n" );
		
		return buf.toString();
	}

	@Override
	protected String clip( String varName ) {
		
		return "$"+varName+" = substr(\"$"+varName+"\",1);";
	}

	@Override
	protected String comment( String comment ) {
		return "# "+comment+"\n";
	}

	@Override
	protected String copyArray( String from, String to ) {
		return "@"+to+" = @"+from+";";
	}

	@Override
	protected String defFunctionLog() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "sub " ).append( FUN_LOG ).append( " {\n" )
		.append( "  my ( $key, $value ) = @_;\n" )
		
		.append( "  my $logfile = \"" ).append( REPORT_FILENAME ).append( "\";\n" ) 
		.append( "  open my $fh, \">>\", $logfile or die \"Can't open $logfile for appending log: $!\\n\";\n" )
		.append( "  flock($fh, 2) or die \"Can't obtain write lock on $logfile:$!\\n\";\n" ) 
		
		
		// .append( "  print $fh \"$key : $value\\\n\";\n" )
		
		.append( "  print $fh \"{" )
		.append( JsonReportEntry.ATT_TIMESTAMP )
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
		.append( JsonReportEntry.ATT_KEY ).append( ":\\\"$key\\\"," )
		.append( JsonReportEntry.ATT_VALUE ).append( ":$value}\";\n" )
		
		
		.append( "  close $fh or die \"Can't close $logfile after appending: $!\\n\";\n" ) 
		.append( "}\n" );
		
		return buf.toString();
	}

	@Override
	protected String defFunctionLogFile() throws NotDerivableException {
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "sub " ).append( FUN_LOGFILE ).append( " {\n" )
		.append( "  my ( $file, $key, $value ) = @_;\n" )

		.append( "  my $logfile = \"" ).append( REPORT_FILENAME ).append( "\";\n" ) 
		.append( "  open my $fh, \">>\", $logfile or die \"Can't open $logfile for appending log: $!\\\n\";\n" )
		.append( "  flock($fh, 2) or die \"Can't obtain write lock on $logfile:$!\\n\";\n" )
		
		// .append( "  print $fh \"$file : $key : $value\\\n\";\n" )
		
		.append( "  print $fh \"{" )
		.append( JsonReportEntry.ATT_TIMESTAMP )
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
		.append( JsonReportEntry.ATT_FILE ).append( ":\\\"$file\\\"," )
		.append( JsonReportEntry.ATT_KEY ).append( ":\\\"$key\\\"," )
		.append( JsonReportEntry.ATT_VALUE ).append( ":$value}\";\n" )
		
		.append( "  close $fh or die \"Can't close $logfile after appending: $!\\n\";\n" ) 
		.append( "}\n" );
		
		return buf.toString();
	}

	@Override
	protected String defFunctionNormalize() throws NotDerivableException {
		StringBuffer buf = new StringBuffer(); 
		
		long id = getTicketId(); 
		buf.append( "sub " ).append( FUN_NORMALIZE ).append( " {\n" )
		.append( "  my ( $channel, $f ) = @_;\n" )
		.append( "  $f =~ m!([^/])+$!;\n" )
		.append("  my $fn = $1;\n")
		.append("  die \"Can't determine filename from $f\\n\" unless $fn;\n")
		.append("  return \"" ).append( id ).append( "_${channel}_$fn\";\n")
		.append("  }\n" );
		
		return buf.toString();
	}

	@Override
	protected String dereference(String varName) {
		return "$"+varName;
	}

	@Override
	protected String fileSize( String filename ) {
		return "-s "+filename;
	}

	@Override
	protected String forEach(String listName, String elementName, String body) {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		
		buf.append( "for my $" ).append( elementName ).append( "(@" )
			.append( listName ).append( ") {\n" )
			.append( body )
			.append( "\n}\n" );
		
		return buf.toString();
	}

	@Override
	protected String getShebang() {
		return "#!/usr/bin/env perl";
	}

	@Override
	protected String ifListIsNotEmpty(String listName, String body) {
		return "if (scalar "+listName+") {\n"+body+"\n}\n";
		
	}

	@Override
	protected String ifNotFileExists(String fileName, String body) {
		return "unless (-e "+fileName+") {\n"+body+"\n}\n";
	}

	@Override
	protected String join( String... elementList ) {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		buf.append("join('',(");
		for (int i = 0; i < elementList.length; i++) {
			buf.append( elementList[i] ); 
			if (i < elementList.length - 1) buf.append(","); 
		}
		buf.append("))");
		return buf.toString(); 
	}

	@Override
	protected String listAppend(String listName, String element) {
		return "push @"+listName+", "+element+";\n";
	}

	@Override
	protected String listToBraceCommaSeparatedString(String listName,
			String stringName, String open, String close) {
		return "$"+stringName + " = " + open + ".join(',', @"+listName+")."+ close +";\n";
	}

	@Override
	protected String newList(String listName) {
		return "@"+listName+" = ();\n";
	}

	@Override
	protected String quote( String content ) {
		
		String ret;
		
		ret = "\""+content.replace( "\"", "\\\"" )+"\"";
		
		return ret;
	}

	@Override
	protected String raise( String msg ) {
		return "die "+msg+";\n";
	}

	@Override
	protected String symlink(String src, String dest) {
		return "`ln -s "+src+" "+dest+"`;\n";
	}

	@Override
	protected String varDef(String varname, String value) {
		StringBuffer buf;
		buf = new StringBuffer();

		buf.append( "$"+ varname ).append( " = " ).append( value ).append(";\n");
		return buf.toString();
	}

	@Override
	protected String varDef( String varname, CompoundExpr ce ) throws NotDerivableException {
		StringBuffer buf;
		int i;
		boolean comma;
		List<String> list;
		
		list = ce.normalize();
		
		buf = new StringBuffer();
		
		comma = false;
		buf.append( "@"+ varname ).append( " = ( " );
		for( i = 0; i < list.size(); i++ ) {
			
			if( comma )
				buf.append( ", " );
			comma = true;
			
			buf.append( "\"" ).append( list.get( i ) ).append( "\"" );
		}
		
		buf.append( " );\n" );
		
		return buf.toString();
	}

}
