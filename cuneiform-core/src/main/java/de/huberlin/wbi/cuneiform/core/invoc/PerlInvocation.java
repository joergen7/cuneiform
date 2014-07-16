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
	protected String clip( String varName ) {
		
		return "$"+varName+" = substr(\"$"+varName+"\",1);";
	}

	@Override
	protected String comment( String comment ) {
		return "# "+comment;
	}

	@Override
	protected String copyArray( String from, String to ) {
		return "@"+to+" = @"+from+";";
	}

	@Override
	public String defFunctionLog() throws NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		

		
		return buf.toString();
	}

	@Override
	protected String defFunctionLogFile() throws NotDerivableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String defFunctionNormalize() throws NotDerivableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String dereference(String varName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String fileSize(String filename) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String forEach(String listName, String elementName, String body) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String getShebang() {
		return "#!/usr/bin/env perl -w";
	}

	@Override
	protected String getCheckPost() { return ""; }

	@Override
	protected String getImport() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String ifListIsNotEmpty(String listName, String body) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String ifNotFileExists(String fileName, String body) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String join(String... elementList) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String listAppend(String listName, String element) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String listToBraceCommaSeparatedString(String listName,
			String stringName, String open, String close) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String newList(String listName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String quote(String content) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String raise(String msg) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String symlink(String src, String dest) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String varDef(String varname, String value) {
		// TODO Auto-generated method stub
		return null;
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
		buf.append( "my @"+ varname ).append( " = ( " );
		for( i = 0; i < list.size(); i++ ) {
			
			if( comma )
				buf.append( ',' );
			comma = true;
			
			buf.append( "\"" ).append( list.get( i ) ).append( "\"" );
		}
		
		buf.append( ")\n" );
		
		return buf.toString();
	}

}
