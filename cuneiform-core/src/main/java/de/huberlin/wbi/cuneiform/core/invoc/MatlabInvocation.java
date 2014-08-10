package de.huberlin.wbi.cuneiform.core.invoc;

import java.nio.file.Path;

import de.huberlin.wbi.cuneiform.core.semanticmodel.CfSemanticModelVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class MatlabInvocation extends OctaveInvocation {
	
	public MatlabInvocation( Ticket ticket ) {
		super( ticket );
	}

	@Override
	protected String getShebang() {
		return "";
	}
	
	@Override
	public Path getExecutablePath( Path location ) {
		return location.resolve( SCRIPT_NAME+".m" );
	}
	
	@Override
	public String[] getCmd() {
		
		return new String[] {
			"matlab",
			"-nodisplay",
			"-nosplash",
			"-r",
			SCRIPT_NAME };
	}
	
	public String getScriptHead() {
		return "function "+SCRIPT_NAME+"\ntry\n";
	}
	
	public String getScriptFoot() {
		return "catch ex\nexit( -1 )\nend\nexit\nend\n";
	}
	
	public String toScript() throws NotBoundException, NotDerivableException {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		// get script header
		buf.append( comment( "script header" ) );
		buf.append( getScriptHead() );

		// bind single output variables to default values
		buf.append( comment( "bind single output variables to default values" ) );
		for( String outputName : getSingleOutputNameSet() )
			buf.append(
				varDef(
					outputName,	
					quote( outputName ) ) );
		buf.append( '\n' );
		
		// bind input parameters
		buf.append( comment( "bind input parameters" ) );
		for( String paramName : getSingleParamNameSet() ) {
			
			if( paramName.equals( CfSemanticModelVisitor.LABEL_TASK ) )
				continue;
			
			buf.append( varDef( paramName, quote( getResolveableBoundToSingleParam( paramName ) ) ) );
		}
		for( String paramName : getReduceParamNameSet() )
			buf.append( varDef( paramName, getReduceParam( paramName ) ) );		
		buf.append( '\n' );
		
		// report stage in file sizes and report error when something is missing
		buf.append( comment( "report stage in file sizes and report error when something is missing" ) );
		buf.append( getStageInCollect() ).append( '\n' );
				
		// insert function body
		buf.append( comment( "insert function body" ) );
		buf.append( getTicket().getBody() ).append( '\n' );
		
		// rename output files
		buf.append( comment( "rename output files" ) );
		buf.append( getOutputRename() ).append( '\n' );
		
		// collect output variables
		buf.append( comment( "collect output variables" ) );
		buf.append( getOutputCollect() ).append( '\n' );
		
		// collect stage out information
		buf.append( comment( "collect stage out information" ) );
		buf.append( getStageOutCollect() ).append( '\n' );
		
		// script footer
		buf.append( comment( "script footer" ) );
		buf.append( getScriptFoot() );
		
		// define necessary functions
		buf.append( comment( "define necessary functions" ) );
		buf.append( getFunDef() ).append( '\n' );
		
		return buf.toString();
	}

}
