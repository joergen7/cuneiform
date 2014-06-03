package de.huberlin.cuneiform.dax.semanticmodel;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.TerminalNode;

import de.huberlin.cuneiform.libdax.parser.DaxBaseListener;
import de.huberlin.cuneiform.libdax.parser.DaxParser;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;

public class DaxSemanticModelListener extends DaxBaseListener {

	private URL xmlns;
	private URL xsi;
	private URL schemaLocation;
	private String version;
	private int count;
	private int index;
	private String name;
	private List<DaxFilename> filenameList;
	private Map<String,DaxJob> jobMap;
	private DaxFilename filename;
	private DaxJob job;
	private DaxJobUses jobUses;
	
	@Override
	public void enterAdagPropXmlns( @NotNull DaxParser.AdagPropXmlnsContext ctx ) {
		xmlns = getUrl( ctx.STRING() );
	}
	
	@Override
	public void enterAdagPropXsi( @NotNull DaxParser.AdagPropXsiContext ctx ) {
		xsi = getUrl( ctx.STRING() );
	}
	
	@Override
	public void enterAdagPropSchemaLocation( @NotNull DaxParser.AdagPropSchemaLocationContext ctx ) {
		schemaLocation = getUrl( ctx.STRING() );
	}
	
	@Override
	public void enterAdagPropVersion( @NotNull DaxParser.AdagPropVersionContext ctx ) {
		version = getString( ctx.STRING() );
	}
	
	@Override
	public void enterAdagPropCount( @NotNull DaxParser.AdagPropCountContext ctx ) {
		count = getInt( ctx.STRING() );
	}
	
	@Override
	public void enterAdagPropIndex( @NotNull DaxParser.AdagPropIndexContext ctx ) {
		index = getInt( ctx.STRING() );
	}

	@Override
	public void enterAdagPropName( @NotNull DaxParser.AdagPropNameContext ctx ) {
		name = getString( ctx.STRING() );
	}
	
	@Override
	public void enterFilename( @NotNull DaxParser.FilenameContext ctx ) {
		
		filename = new DaxFilename();
		
		if( job == null )
			filenameList.add( filename );
	}
	
	@Override
	public void exitFilename( @NotNull DaxParser.FilenameContext ctx ) {
		filename = null;
	}
	
	@Override
	public void enterFilenamePropFile( @NotNull DaxParser.FilenamePropFileContext ctx ) {
		filename.setFile( getString( ctx.STRING() ) );
	}
	
	@Override
	public void enterFilenamePropLinkInput( @NotNull DaxParser.FilenamePropLinkInputContext ctx ) {
		filename.setLinkInput();
	}
	
	@Override
	public void enterFilenamePropLinkOutput( @NotNull DaxParser.FilenamePropLinkOutputContext ctx ) {
		filename.setLinkOutput();
	}

	@Override
	public void enterFilenamePropLinkInout( @NotNull DaxParser.FilenamePropLinkInoutContext ctx ) {
		filename.setLinkInout();
	}

	@Override
	public void enterJob( @NotNull DaxParser.JobContext ctx ) {
		job = new DaxJob();
	}

	@Override
	public void exitJob( @NotNull DaxParser.JobContext ctx ) {
		job = null;
	}
	
	@Override
	public void exitJobPropId( @NotNull DaxParser.JobPropIdContext ctx ) {
		
		String id;
		
		id = getString( ctx.STRING() );
		
		job.setId( id );
		jobMap.put( id, job );
	}

	@Override
	public void enterJobPropName( @NotNull DaxParser.JobPropNameContext ctx ) {
		job.setName( getString( ctx.STRING() ) );
	}

	@Override
	public void enterJobPropVersion( @NotNull DaxParser.JobPropVersionContext ctx ) {
		job.setVersion( getString( ctx.STRING() ) );
	}
	
	@Override
	public void enterJobPropLevel( @NotNull DaxParser.JobPropLevelContext ctx ) {
		job.setLevel( getInt( ctx.STRING() ) );
	}

	@Override
	public void exitJobPropDvName( @NotNull DaxParser.JobPropDvNameContext ctx ) {
		job.setDvName( getString( ctx.STRING() ) );
	}
	
	@Override
	public void enterJobPropDvVersion( @NotNull DaxParser.JobPropDvVersionContext ctx ) {
		job.setDvVersion( getString( ctx.STRING() ) );
	}

	@Override
	public void enterArgumentElPlain( @NotNull DaxParser.ArgumentElPlainContext ctx ) {
		job.addPlainArg( ctx.ARG().getText() );
	}
	
	@Override
	public void exitArgumentElFilename( @NotNull DaxParser.ArgumentElFilenameContext ctx ) {
		job.addFilenameArg( filename );
	}

	@Override
	public void enterJobElUses( @NotNull DaxParser.JobElUsesContext ctx ) {
		jobUses = new DaxJobUses();
		job.addJobUses( jobUses );
	}
	
	@Override
	public void exitJobElUses( @NotNull DaxParser.JobElUsesContext ctx ) {		
		jobUses = null;
	}
	
	@Override
	public void enterJobUsesPropFile( @NotNull DaxParser.JobUsesPropFileContext ctx ) {
		jobUses.setFile( getString( ctx.STRING() ) );
	}
	
	@Override
	public void enterJobUsesPropLinkInput( @NotNull DaxParser.JobUsesPropLinkInputContext ctx ) {
		jobUses.setLinkInput();
	}

	@Override
	public void enterJobUsesPropLinkOutput( @NotNull DaxParser.JobUsesPropLinkOutputContext ctx ) {
		jobUses.setLinkOutput();
	}
	
	@Override
	public void enterJobUsesPropRegisterFalse( @NotNull DaxParser.JobUsesPropRegisterFalseContext ctx ) {
		jobUses.setRegister( false );
	}

	@Override
	public void enterJobUsesPropRegisterTrue( @NotNull DaxParser.JobUsesPropRegisterTrueContext ctx ) {
		jobUses.setRegister( true );
	}

	@Override
	public void enterJobUsesPropTransferFalse( @NotNull DaxParser.JobUsesPropTransferFalseContext ctx ) {
		jobUses.setTransfer( false );
	}

	@Override
	public void enterJobUsesPropTransferTrue( @NotNull DaxParser.JobUsesPropTransferTrueContext ctx ) {
		jobUses.setTransfer( true );
	}
	
	@Override
	public void enterJobUsesPropExecutable( @NotNull DaxParser.JobUsesPropExecutableContext ctx ) {
		jobUses.setTypeExecutable();
	}
	
	@Override
	public void enterJobUsesPropOptionalFalse( @NotNull DaxParser.JobUsesPropOptionalFalseContext ctx ) {
		jobUses.setOptional( false );
	}

	@Override
	public void enterJobUsesPropOptionalTrue( @NotNull DaxParser.JobUsesPropOptionalTrueContext ctx ) {
		jobUses.setOptional( true );
	}
	
	@Override
	public void enterChild( @NotNull DaxParser.ChildContext ctx ) {
		
		String id;
		
		id = getString( ctx.STRING() );
		
		job = jobMap.get( id );
		if( job == null )
			throw new NullPointerException( "Could not retrieve referenced child job." );
	}
	
	@Override
	public void exitChild( @NotNull DaxParser.ChildContext ctx ) {
		job = null;
	}
	
	@Override
	public void enterParent( @NotNull DaxParser.ParentContext ctx ) {
		
		DaxJob parent;
		String id;
		
		id = getString( ctx.STRING() );
		
		parent = jobMap.get( id );
		if( job == null )
			throw new NullPointerException( "Could not retrieve referenced parent job." );
		
		parent.addChild( job );
		job.addParent( parent );
	}

	public TopLevelContext toTopLevelContext() {
		
		TopLevelContext tlc;
		CompoundExpr ce;
		
		tlc = new TopLevelContext();
		ce = new CompoundExpr();
		tlc.addTarget( ce );
		
		/* for( DaxFilename filename : filenameList )
			if( filename.isLinkOutput() ) */
				
		
		
		return tlc;
	}
	
	public static int getInt( TerminalNode node ) {
		return Integer.valueOf( getString( node ) );
	}

	public static String getString( TerminalNode node ) {
		
		String s;
		
		s = node.getText();
		s = s.substring( 1, s.length()-1 );
		
		return s;
	}
	
	public static URL getUrl( TerminalNode node ) {
		
		try {
			return new URL( getString( node ) );
		}
		catch( MalformedURLException e ) {
			throw new RuntimeException( e );
		}
	}

}
