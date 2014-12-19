package de.huberlin.wbi.cuneiform.core.invoc;

import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class PythonInvocation extends Invocation {

	private static String defFunction( String funname, String[] inputNameList, String body ) {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( "def " ).append( funname ).append( "( " );
		
		comma = false;
		for( String inputName : inputNameList ) {
			
			if( comma )
				buf.append( ", " );
			comma = true;
			
			buf.append( inputName );
		}
		
		buf.append( " )\n    " );
		
		buf.append( body.replace( "\n", "\n    " ) );
		buf.append( "\n" );

		return buf.toString();
	}
	
	public PythonInvocation( Ticket ticket, String libPath ) {
		super( ticket, libPath );
	}

	@Override
	protected String callFunction( String name, String... argValue ) {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( name ).append( '(' );
		
		comma = false;
		for( String arg : argValue ) {
			
			if( comma )
				buf.append( ',' );
			
			comma = true;
			
			buf.append( arg );
		}
		
		buf.append( ")" );
		
		return buf.toString();
	}

	@Override
	protected String callProcedure( String name, String... argValue ) {
		return callFunction( name, argValue )+"\n";
	}

	@Override
	protected String clip( String varName ) {
		return varName+"[1:]\n";
	}

	@Override
	protected String comment( String comment ) {
		return "# "+comment.replace( "\n", "\n# " )+"\n";
	}

	@Override
	protected String copyArray( String from, String to ) {
		return to+" = copy.copy( "+from+" )\n";
	}

	@Override
	protected String defFunctionLog() throws NotDerivableException {
		return	defFunction( FUN_LOG, new String[] { "key", "value" },
					"f = file(\""+REPORT_FILENAME+"\",\"a\")\nf.write("+"\"{"
						+JsonReportEntry.ATT_TIMESTAMP+":"+System.currentTimeMillis()+","
						+JsonReportEntry.ATT_RUNID+":\\\""+getRunId()+"\\\","
						+JsonReportEntry.ATT_TASKID+":"+getTaskId()+","
						+JsonReportEntry.ATT_TASKNAME+":\\\""+getTaskName()+"\\\","
						+JsonReportEntry.ATT_LANG+":\\\""+getLangLabel()+"\\\","
						+JsonReportEntry.ATT_INVOCID+":"+getTicketId()+","
						+JsonReportEntry.ATT_KEY+":\\\"%s\\\","
						+JsonReportEntry.ATT_VALUE+":%s}\\n\"%(key,value))\nf.close()\n" );
	}

	@Override
	protected String defFunctionLogFile() throws NotDerivableException {
		return defFunction( FUN_LOG, new String[] { "fx", "key", "value" },
				"f = file(\""+REPORT_FILENAME+"\",\"a\")\nf.write("+"\"{"
					+JsonReportEntry.ATT_TIMESTAMP+":"+System.currentTimeMillis()+","
					+JsonReportEntry.ATT_RUNID+":\\\""+getRunId()+"\\\","
					+JsonReportEntry.ATT_TASKID+":"+getTaskId()+","
					+JsonReportEntry.ATT_TASKNAME+":\\\""+getTaskName()+"\\\","
					+JsonReportEntry.ATT_LANG+":\\\""+getLangLabel()+"\\\","
					+JsonReportEntry.ATT_INVOCID+":"+getTicketId()+","
					+JsonReportEntry.ATT_FILE+":\\\"%s\\\","
					+JsonReportEntry.ATT_KEY+":\\\"%s\\\","
					+JsonReportEntry.ATT_VALUE+":%s}\\n\"%(fx,key,value))\nf.close()\n" );
	}

	@Override
	protected String defFunctionNormalize() throws NotDerivableException {
		return defFunction(
				FUN_NORMALIZE,
				new String[] { "channel", "f" },
				"\""+getTicketId()+"_%d_%s\"%(channel,f)" );
	}

	@Override
	protected String dereference( String varName ) {
		return varName;
	}

	@Override
	protected String fileSize( String filename ) {
		return "os.stat("+filename+").st_size";
	}

	@Override
	protected String forEach( String listName, String elementName, String body ) {
		return
			"for "+elementName+" in "+listName+":\n    "
			+body.replace( "\n", "\n    " )+"\n";
	}

	@Override
	protected String getImport() {
		return "import copy, os\n";
	}

	@Override
	protected String getShebang() {
		return "#!/usr/bin/env python\n";
	}

	@Override
	protected String ifListIsNotEmpty( String listName, String body ) {
		return "if not("+listName+"):\n    "+body.replace( "\n", "\n    " )+"\n";
	}

	@Override
	protected String ifNotFileExists( String fileName, String body ) {
		return
			"if not(os.path.exists("+fileName+")):\n    "
			+body.replace( "\n", "\n    " )+"\n";
	}

	@Override
	protected String join( String... elementList ) {

		boolean comma;
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		comma = false;
		for( String element : elementList ) {
			
			if( comma )
				buf.append( '+' );
			
			comma = true;
			
			buf.append( element );
		}
		
		return buf.toString();
	}

	@Override
	protected String listAppend( String listName, String element ) {
		return listName+".append("+element+")\n";
	}

	@Override
	protected String listToBraceCommaSeparatedString( String listName,
			String stringName, String open, String close ) {
		return
			stringName+"='"+open+"'+','.join( i for i in "+listName
			+" )+'"+close+"'\n";
	}

	@Override
	protected String newList( String listName ) {
		return listName+"=[]\n";
	}

	@Override
	protected String quote( String content ) {
		return "\""+content.replace( "\\", "\\\\" ).replace( "\"", "\\\"")+"\"";
	}

	@Override
	protected String raise( String msg ) {
		return "raise Exception("+quote( msg )+")";
	}

	@Override
	protected String symlink( String src, String dest ) {
		return "os.symlink("+src+","+"dest"+")\n";
	}

	@Override
	protected String varDef( String varname, CompoundExpr ce )
		throws NotDerivableException {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( varname ).append( "=[" );
		
		comma = false;
		for( String s : ce.normalize() ) {
			
			if( comma )
				buf.append( ',' );
			
			comma = true;
			
			buf.append( quote( s ) );
		}
		
		buf.append( "]\n" );
		
		return buf.toString();
	}
	
	@Override
	protected String varDef( String varname, String value ) {
		return varname+"="+value+"\n";
	}

	@Override
	protected String getLibPath() {
		throw new UnsupportedOperationException( "NYI" );
	}

}
