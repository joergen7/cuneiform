package de.huberlin.wbi.cuneiform.core.invoc;

import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class ScalaInvocation extends Invocation {

	public ScalaInvocation( Ticket ticket, String libPath ) {
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
		return varName+"="+varName+" substring(1)\n";
	}

	@Override
	protected String comment( String comment ) {
		return "// "+comment+"\n";
	}

	@Override
	protected String copyArray( String from, String to ) {
		return "val "+to+"="+from+"\n";
	}

	@Override
	protected String defFunctionLog() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "def "+FUN_LOG+"( key:String, value:String ) : Unit = {\n" );
		
		buf.append( "  val writer = new FileWriter(\""+REPORT_FILENAME+"\",true);\n" );
		buf.append( "  writer write(\"{" );
		
		buf.append( JsonReportEntry.ATT_TIMESTAMP )
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
		.append( JsonReportEntry.ATT_KEY ).append( ":\\\"\"+key+\"\\\"," )
		.append( JsonReportEntry.ATT_VALUE ).append( ":\"+value+\"}\\n\"" );
		
		buf.append( ")\n  writer close\n" );
		
		buf.append( "}\n" );
		
		return buf.toString();
	}

	@Override
	protected String defFunctionLogFile() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "def "+FUN_LOGFILE+"( file:String, key:String, value:String ) : Unit = {\n" );
		
		buf.append( "  val writer = new FileWriter(\""+REPORT_FILENAME+"\",true);\n" );
		buf.append( "  writer write(\"{" );
		
		buf.append( JsonReportEntry.ATT_TIMESTAMP )
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
		.append( JsonReportEntry.ATT_FILE ).append( ":\\\"\"+file+\"\\\"," )
		.append( JsonReportEntry.ATT_KEY ).append( ":\\\"\"+key+\"\\\"," )
		.append( JsonReportEntry.ATT_VALUE ).append( ":\"+value+\"}\\n\"" );
		
		buf.append( ")\n  writer close\n" );
		
		buf.append( "}\n" );
		
		return buf.toString();
	}

	@Override
	protected String defFunctionNormalize() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "def "+FUN_NORMALIZE+"(channel:Int,filename:String) : String = {" );
		buf.append( "val p = Paths.get(filename)\n");
		buf.append( "return \""+getTicketId()+"_\"+channel+\"_\"+p.getName(p.getNameCount-1)\n" );
		buf.append( "}\n" );
		
		return buf.toString();
	}

	@Override
	protected String dereference( String varName ) {
		return varName;
	}

	@Override
	protected String fileSize( String filename ) {
		return "Files.size(Paths.get("+filename+")).toString";
	}

	@Override
	protected String forEach( String listName, String elementName, String body ) {
		return "for(String "+elementName+":"+listName+") {\n"+body+"\n}\n";
	}

	@Override
	protected String getShebang() {
		return "#!/bin/sh\nexec scala \"$0\" \"$@\"\n!#";
	}

	@Override
	protected String getLibPath() {
		throw new UnsupportedOperationException( "NYI" );
	}
	
	@Override
	protected String getImport() {
		return "import java.nio.file.Paths\nimport java.io.FileWriter\nimport java.nio.file.Files";
	}

	@Override
	protected String ifListIsNotEmpty(String listName, String body) {
		return( "if(!"+listName+".isEmpty) {\n"+body+"\n}\n" );
	}

	@Override
	protected String ifNotFileExists(String fileName, String body) {
		return( "if(!Files.exists(Paths.get("+fileName+"))) {\n"+body+"\n}\n" );
	}
		
	@Override
	protected String declareString( String outputName ) {
		return "var "+outputName+"=\"\"\n";
	}
	
	@Override
	protected String declareList( String paramName ) {
		return "var "+paramName+"=List()\n";
	}

	@Override
	protected String join( String... elementList ) {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		comma = false;
		for( String element : elementList ) {
			
			if( comma )
				buf.append( "+" );
			comma = true;
			
			buf.append( element );
		}
		
		return buf.toString();
	}

	@Override
	protected String listAppend(String listName, String element) {
		return listName+"="+listName+":+"+element+"\n";
	}

	@Override
	protected String listToBraceCommaSeparatedString(String listName, String stringName, String open, String close) {
		return stringName+"=\""+open+"\"+"+listName+".mkString(\",\")+\""+close+"\"\n";
	}

	@Override
	protected String newList( String listName ) {
		return "var "+listName+"=List()\n";
	}

	@Override
	protected String quote( String content ) {
		return "\""+content.replace( "\"", "\\\"" )+"\"";
	}

	@Override
	protected String raise( String msg ) {
		return "throw new RuntimeException("+msg+")\n";
	}

	@Override
	protected String symlink( String src, String dest ) {
		return "Files.createSymbolicLink(Paths.get("+dest+"),Paths.get("+src+"))\n";
	}

	@Override
	protected String varDef( String varname, String value ) {
		return varname+"="+value+"\n";
	}

	@Override
	protected String varDef( String varname, CompoundExpr ce ) throws NotDerivableException {
		
		StringBuffer buf;
		boolean comma;
		int i, n;
		
		buf = new StringBuffer();
		
		buf.append( "val "+varname+"=List(" );
		
		comma = false;
		n = ce.getNumAtom();
		for( i = 0; i < n; i++ ) {
			
			if( comma )
				buf.append( ',' );
			comma = true;
			
			buf.append( ce.getStringExprValue( i ) );
		}
		buf.append( ")\n" );
		
		return buf.toString();
	}

}
