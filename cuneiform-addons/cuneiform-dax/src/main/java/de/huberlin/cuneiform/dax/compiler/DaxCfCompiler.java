package de.huberlin.cuneiform.dax.compiler;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.TerminalNode;

import de.huberlin.cuneiform.libdax.parser.DaxBaseListener;
import de.huberlin.cuneiform.libdax.parser.DaxParser;
import de.huberlin.cuneiform.libdax.parser.DaxParser.ArgumentElContext;

public class DaxCfCompiler extends DaxBaseListener implements CfConvertable {
	
	private static final int LINK_INPUT = 0;
	private static final int LINK_OUTPUT = 1;
	private static final int LINK_INOUT = 2;

	private Map<String,DaxJob> daxJobMap;
	private Map<String,String> file2VarMap;
	private Map<String,String> var2FileMap;
	private Set<String> inputFilenameSet;
	private DaxJob curDaxJob;
	private StringBuffer argBuf;
	private int nextVar;
	private String curFilename;
	private int curLink;
	
	@Override
	public void enterAdag( @NotNull DaxParser.AdagContext ctx ) {
				
		nextVar = 0;
		daxJobMap = new HashMap<>();
		file2VarMap = new HashMap<>();
		var2FileMap = new HashMap<>();
		inputFilenameSet = new HashSet<>();
	}
	
	@Override
	public void enterArgument( @NotNull DaxParser.ArgumentContext ctx ) {
		
		if( curDaxJob == null )
			throw new NullPointerException( "Current DAX job not set." );
		
		if( !curDaxJob.hasName() )
			throw new RuntimeException( "Current DAX job's name not set." );
		
		argBuf = new StringBuffer();
		argBuf.append( curDaxJob.getName() );
	}
	
	@Override
	public void enterArgumentElPlain( @NotNull DaxParser.ArgumentElPlainContext ctx ) {
		
		if( argBuf == null )
			throw new NullPointerException( "Argument buffer not initialized." );
		
		argBuf.append( ' ' ).append( ctx.ARG().getText() );
	}
		
	@Override
	public void enterChild( @NotNull DaxParser.ChildContext ctx ) {
		curDaxJob = getDaxJob( getStringContent( ctx.STRING() ) );
	}
	
	@Override
	public void enterFilenamePropFile( @NotNull DaxParser.FilenamePropFileContext ctx ) {
		curFilename = getStringContent( ctx.STRING() );
	}
	
	@Override
	public void enterFilenamePropLink( @NotNull DaxParser.FilenamePropLinkContext ctx ) {
		
		if( ctx.INPUT() != null ) {
			curLink = LINK_INPUT;
			return;
		}
		
		if( ctx.OUTPUT() != null ) {
			curLink = LINK_OUTPUT;
			return;
		}
		
		if( ctx.INOUT() != null ) {
			curLink = LINK_INOUT;
			return;
		}
		
		throw new RuntimeException( "Link type not recognized." );
	}
	
	@Override
	public void enterJobPropId( @NotNull DaxParser.JobPropIdContext ctx ) {
		curDaxJob = getDaxJob( getStringContent( ctx.STRING() ) );
	}
	
	@Override
	public void enterJobPropName( @NotNull DaxParser.JobPropNameContext ctx ) {
		
		if( curDaxJob == null )
			throw new NullPointerException( "Current DAX job not set." );
		
		curDaxJob.setName( getStringContent( ctx.STRING() ) );
	}
	
	@Override
	public void enterParent( @NotNull DaxParser.ParentContext ctx ) {
		
		DaxJob curParent;
		
		if( curDaxJob == null )
			throw new NullPointerException( "Current child not set." );
				
		// fetch parent job
		curParent = getDaxJob( getStringContent( ctx.STRING() ) );
		
		// register child
		curParent.addChild( curDaxJob );
		
		// register parent
		curDaxJob.addParent( curParent );
	}
	
	@Override
	public void exitArgument( @NotNull DaxParser.ArgumentContext ctx ) {
		
		if( curDaxJob == null )
			throw new NullPointerException( "Current DAX job not set." );
		
		if( argBuf == null )
			throw new NullPointerException( "Argument buffer not initialized." );
		
		curDaxJob.setBody( argBuf.toString() );
		argBuf = null;
	}
	
	@Override
	public void exitArgumentElFilename( @NotNull DaxParser.ArgumentElFilenameContext ctx ) {
		
		String varname;
		
		if( argBuf == null )
			throw new NullPointerException( "Argument buffer not initialized." );
		
		if( curDaxJob == null )
			throw new NullPointerException( "Current DAX job not set." );
		
		// fetch variable name
		varname = getVarname( curFilename );
		
		// append variable to command line call
		argBuf.append( " $" ).append( varname );
		
		if( curLink == LINK_INOUT )
			throw new RuntimeException( "Variable cannot be input as well as output." );
		
		// add variable to input variable set
		else if( curLink == LINK_INPUT )
			curDaxJob.addInputVar( varname );
		
		// add variable to output variable set
		else if( curLink == LINK_OUTPUT )
			curDaxJob.addOutputVar( varname );
		
		else
			throw new RuntimeException( "Link type not recognized." );
		
	}
	
	@Override
	public void exitChild( @NotNull DaxParser.ChildContext ctx ) {
		curDaxJob = null;
	}
	
	@Override
	public void exitFilename( @NotNull DaxParser.FilenameContext ctx ) {
		
		if( ctx.parent instanceof ArgumentElContext )
			return;
		
		if( curLink != LINK_INPUT )
			return;
		
		inputFilenameSet.add( curFilename );
	}
	
	@Override
	public void exitJob( @NotNull DaxParser.JobContext ctx ) {
		curDaxJob = null;
	}

	public Set<DaxJob> getLeafDaxJobSet() {
		
		Set<DaxJob> set;
		
		set = new HashSet<>();
		
		for( DaxJob daxJob : daxJobMap.values() )
			if( daxJob.isLeaf() )
				set.add( daxJob );
		
		return set;
	}
	
	@Override
	public String toCuneiform() {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( "// Files Used\n\n" );
		
		for( String inputFilename : inputFilenameSet )
			buf.append( file2VarMap.get( inputFilename ) )
				.append( " = '" )
				.append( inputFilename ).append( "';\n" );
		
		buf.append( "\n// Definition of Jobs\n\n" );
		
		for( DaxJob daxJob : daxJobMap.values() )
			buf.append( daxJob.toCuneiform() ).append( '\n' );
		
		buf.append( "\n// Target Variables\n\n" );
		
		comma = false;
		for( DaxJob daxJob : daxJobMap.values() ) {
			
			if( !daxJob.isLeaf() )
				continue;

			for( String outputVar : daxJob.getOutputVarList() ) {
			
				if( comma )
					buf.append( ' ' );
				comma = true;
				
				buf.append( outputVar );
			}
			
		}
		
		buf.append( ";\n" );
		
		return buf.toString();
	}
	
	private DaxJob getDaxJob( String id ) {
		
		DaxJob daxJob;
		
		daxJob = daxJobMap.get( id );
		
		if( daxJob == null ) {
			daxJob = new DaxJob( id );
			daxJobMap.put( id, daxJob );
		}
		
		return daxJob;
	}
	
	private static String getStringContent( TerminalNode node ) {
		
		String id;
		
		if( node == null )
			throw new NullPointerException( "Node must not be null." );
		
		// fetch child id string
		id = node.getText();
		
		// remove trailing quotes
		id = id.substring( 1, id.length()-1 );
		
		return id;

	}

	private String getVarname( String filename ) {
		
		String varname;
		
		if( filename == null )
			throw new NullPointerException( "File name must not be null." );
		
		if( filename.isEmpty() )
			throw new RuntimeException( "File name must not be empty." );

		varname = file2VarMap.get( filename );
		if( varname == null ) {
			varname = "var"+( nextVar++ );
			file2VarMap.put( filename, varname );
			var2FileMap.put( varname, filename );
		}
		
		return varname;
	}
}
