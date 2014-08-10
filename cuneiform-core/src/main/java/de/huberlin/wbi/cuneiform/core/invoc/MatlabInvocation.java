package de.huberlin.wbi.cuneiform.core.invoc;

import java.nio.file.Path;

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
			"\"try, "+SCRIPT_NAME+", catch ex, exit( -1 ), end, exit\"" };
	}
	
	public String getScriptHead() {
		return "function "+SCRIPT_NAME+"\n";
	}
	
	@Override
	public String getScriptFoot() {
		return "end\n";
	}
}
