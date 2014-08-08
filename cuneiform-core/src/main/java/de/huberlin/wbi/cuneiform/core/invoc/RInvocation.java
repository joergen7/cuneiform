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

public class RInvocation extends Invocation {

	protected RInvocation( Ticket ticket ) {
		super( ticket );
	}

	@Override
	protected String defFunctionLog() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( defFunction( FUN_LOG, null, new String[] { "key", "value" },
			"if( mode( key ) != \"character\" )\n"
			+"stop( \"Expected key to be of type 'character'.\" )\n"
			+"if( mode( value ) != \"character\" )\n"
			+"stop( \"Expected key to be of type 'value'.\" )\n"
			+"write( paste( \"{"
			+JsonReportEntry.ATT_TIMESTAMP+":"+System.currentTimeMillis()+","
			+JsonReportEntry.ATT_RUNID+":\\\""+getRunId()+"\\\","
			+JsonReportEntry.ATT_TASKID+":"+getTaskId()+","
			+JsonReportEntry.ATT_TASKNAME+":"+getTaskName()+","
			+JsonReportEntry.ATT_LANG+":"+getLangLabel()+","
			+JsonReportEntry.ATT_INVOCID+":"+getTicketId()+","
			+JsonReportEntry.ATT_KEY+":\\\"\", key, \"\\\","
			+JsonReportEntry.ATT_VALUE+":\", value, \"}\\n\", sep=\"\" ), "
			+"file=\""+REPORT_FILENAME+"\" )" ) );
		
		return buf.toString();
	}

	@Override
	protected String getShebang() {
		return "#!/usr/bin/env Rscript\n";
	}

	@Override
	protected String varDef( String varname, CompoundExpr ce )
			throws NotDerivableException {
		
		StringBuffer buf;
		int i;
		boolean comma;
		List<String> list;
		
		list = ce.normalize();
		
		buf = new StringBuffer();
		
		comma = false;
		buf.append( varname ).append( " <- c( " );
		for( i = 0; i < list.size(); i++ ) {
			
			if( comma )
				buf.append( ',' );
			comma = true;
			
			buf.append( "\"" ).append( list.get( i ) ).append( "\"" );
		}
		
		buf.append( ")\n" );
		
		return buf.toString();
	}

	@Override
	protected String varDef( String varname, String value ) {
		return varname+" <- "+value+"\n";
	}
	
	/**
	 * @param outputName  
	 */
	@SuppressWarnings( "static-method" )
	private String defFunction( String funName, String outputName, String[] inputNameList, String body ) {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( funName ).append( " <- function(" );
		
		comma = false;
		for( String arg : inputNameList ) {
			
			if( comma )
				buf.append( ',' );
			
			comma = true;
			
			buf.append( ' ' ).append( arg );
		}
		
		buf.append( " ) {\n" ).append( body ).append( "\n}\n" );
		
		return buf.toString();
	}

	@Override
	protected String callFunction( String name, String... argValue ) {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( name ).append( "( " );
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
		
		buf.append( name ).append( "( " );
		comma = false;
		for( String arg : argValue ) {

			if( comma )
				buf.append( ", " );
			comma = true;
			
			buf.append( arg );
		}
		
		buf.append( " )\n" );
		
		return buf.toString();
	}

	@Override
	protected String newList( String listName ) {
		return varDef( listName, "NULL" );
	}

	@Override
	protected String listAppend( String listName, String element ) {
		return listName+" <- append( "+listName+", "+element+" )\n";
	}

	@Override
	protected String dereference( String varName ) {
		return varName;
	}

	@Override
	protected String forEach( String listName, String elementName, String body ) {
		return "for( "+elementName+" in "+listName+" ) {\n"+body+"\n}\n";
	}

	@Override
	protected String ifNotFileExists( String fileName, String body ) {
		return
			"if( !"
			+callFunction( "file.exists", dereference( fileName ) )
			+" ) {\n"
			+body
			+"\n}\n";
	}

	@Override
	protected String raise( String msg ) {
		return callProcedure( "stop", msg );
	}

	@Override
	protected String join( String... elementList ) {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( "paste( " );
		
		comma = false;
		for( String element : elementList ) {
			
			if( comma )
				buf.append( ", " );
			comma = true;
			
			buf.append( element );
		}
		
		buf.append( ", sep=\"\" )" );
		
		return buf.toString();
	}

	@Override
	protected String quote( String content ) {
		return "\""+content.replace( "\"", "\\\"" )+"\"";
	}

	@Override
	protected String fileSize( String filename ) {
		return "file.info( "+quote( filename )+" )$size/1024";
	}

	@Override
	protected String ifListIsNotEmpty( String listName, String body ) {
		return "if( length( "+listName+" ) != 0 ) {\n"+body+"\n}\n";
	}

	@Override
	protected String listToBraceCommaSeparatedString( String listName,
		String stringName, String open, String close ) {
		return stringName+" = paste( "+listName+", sep=\"\", collapse=\",\" )\n";
	}

	@Override
	protected String defFunctionNormalize() throws NotDerivableException {
		return defFunction(
				FUN_NORMALIZE,
				null,
				new String[] { "channel", "f" },
				"sprintf( \""+getTicketId()
				+"_%d_%s\", channel, f )\n" );
	}

	@Override
	protected String symlink( String src, String dest ) {
		return callProcedure( "file.symlink", src, dest )+"\n";
	}

	@Override
	protected String comment( String comment ) {
		return "# "+comment.replace( "\n", "\n# " )+"\n";
	}

	@Override
	protected String copyArray( String from, String to ) {
		return to+"="+from+"\n";
	}

	@Override
	protected String clip( String varName ) {
		return varName+" <- substring( "+varName+", 2 )\n";
	}

	@Override
	protected String defFunctionLogFile() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( defFunction( FUN_LOGFILE, null, new String[] { "file", "key", "value" },
			"if( mode( key ) != \"character\" )\n"
			+"stop( \"Expected key to be of type 'character'.\" )\n"
			+"if( mode( value ) != \"character\" )\n"
			+"stop( \"Expected key to be of type 'value'.\" )\n"
			+"write( paste( \"{"
			+JsonReportEntry.ATT_TIMESTAMP+":"+System.currentTimeMillis()+","
			+JsonReportEntry.ATT_RUNID+":\\\""+getRunId()+"\\\","
			+JsonReportEntry.ATT_TASKID+":"+getTaskId()+","
			+JsonReportEntry.ATT_TASKNAME+":"+getTaskName()+","
			+JsonReportEntry.ATT_LANG+":"+getLangLabel()+","
			+JsonReportEntry.ATT_INVOCID+":"+getTicketId()+","
			+JsonReportEntry.ATT_FILE+":\\\"\", file, \"\\\","
			+JsonReportEntry.ATT_KEY+":\\\"\", key, \"\\\","
			+JsonReportEntry.ATT_VALUE+":\", value, \"}\\n\", sep=\"\" ), "
			+"file=\""+REPORT_FILENAME+"\" )" ) );
		
		return buf.toString();
	}

}
