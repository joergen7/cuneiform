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
		return location.resolve( "cfscript.m" );
	}
	
	@Override
	public String[] getCmd() {
		
		return new String[] {
			"matlab",
			"-nodisplay",
			"-nosplash",
			"-r",
			"\"cfscript\"" };
	}
	
	@Override
	public String getQuit() {
		return "exit;\n";
	}
}
