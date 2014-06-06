package de.huberlin.cuneiform.dax.semanticmodel;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CfSemanticModelVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.DataType;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ForeignLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Prototype;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Type;

public class DaxJob {
	
	private static final String PREFIX_OUT = "out";
	private static final String PREFIX_IN = "in";

	private String name;
	private final List<DaxJob> parentSet;
	private final List<DaxJob> childSet;
	private final List<Object> argList;
	private final List<DaxJobUses> jobUsesList;
	
	public DaxJob() {
		parentSet = new ArrayList<>();
		childSet = new ArrayList<>();
		argList = new ArrayList<>();
		jobUsesList = new ArrayList<>();
	}
	
	public void addChild( DaxJob child ) {
		
		if( child == null )
			throw new NullPointerException( "Child DAX job must not be null." );
		
		childSet.add( child );
	}
	
	public void addFilenameArg( DaxJobUses filename ) {
		
		if( filename == null )
			throw new NullPointerException( "Filename must not be null." );
		
		argList.add( filename );
	}
	
	public void addJobUses( DaxJobUses jobUses ) {
		
		if( jobUses == null )
			throw new NullPointerException( "Job-Uses element object must not be null." );
		
		jobUsesList.add( jobUses );
	}
	
	public void addParent( DaxJob parent ) {
		
		if( parent == null )
			throw new NullPointerException( "Parent DAX job must not be null." );
		
		parentSet.add( parent );
	}
	
	public void addPlainArg( String arg ) {
		
		if( arg == null )
			throw new NullPointerException( "Argument string must not be null." );
		
		if( arg.isEmpty() )
			throw new RuntimeException( "Argument string must not be empty." );
		
		argList.add( arg );
	}
	
	public String getName() {		
		return name;
	}
	
	public boolean hasName() {
		return name != null;
	}
	
	public boolean isLeaf() {
		return childSet.isEmpty();
	}
	
	public boolean isRoot() {
		return parentSet.isEmpty();
	}
	
	public void setName( String name ) {
		
		if( name == null ) {
			this.name = null;
			return;
		}

		if( name.isEmpty() )
			throw new RuntimeException( "Name string must not be empty." );
		
		this.name = name;		
	}
	
	public Set<DaxJobUses> getInputJobUsesSet() {
		
		HashSet<DaxJobUses> set;
		
		set = new HashSet<>();
		for( DaxJobUses jobUses : jobUsesList )
			if( jobUses.isLinkInput() )
				set.add( jobUses );
		
		return set;
	}
	
	public Set<DaxJobUses>getOutputJobUsesSet() {
		
		HashSet<DaxJobUses> set;
		
		set = new HashSet<>();
		for( DaxJobUses jobUses : jobUsesList ) {
			
			if( isBidirectional( jobUses ) )
				continue;
			
			if( jobUses.isLinkOutput() )
				set.add( jobUses );
		}
		
		return set;
	}
	
	public Prototype getPrototype() {
		
		Prototype prototype;
		Type type;
		int n, m;
		
		prototype = new Prototype();
		prototype.addParam( new NameExpr( CfSemanticModelVisitor.LABEL_TASK ) );
		
		n = 1;
		m = 1;
		type = new DataType( DataType.LABEL_FILE );
		
		for( DaxJobUses jobUses : jobUsesList ) {
			
			if( isBidirectional( jobUses ) )
				continue;
			
			if( jobUses.isLinkOutput() )	
				prototype.addOutput( new NameExpr( PREFIX_OUT+( n++ ), type ) );
			else
				prototype.addParam( new NameExpr( PREFIX_IN+( m++ ), type ) );
			
		}
		
		return prototype;
	}
	
	public boolean isBidirectional( DaxJobUses filename ) {
		
		int i;
		
		i = 0;
		for( DaxJobUses jobUses : jobUsesList ) {
			
			if( !jobUses.equals( filename ) )
				continue;
			
			i++;
		}
		
		return i > 1;
	}
	
	public String getReference( DaxJobUses filename ) {
		
		int i;
		
		i = 1;
		for( DaxJobUses jobUses : jobUsesList ) {
			
			if( !jobUses.isLinkInput() )
				continue;
			
			if( jobUses.equals( filename ) )
				return PREFIX_IN+i;
			
			i++;
		}

		i = 1;
		for( DaxJobUses jobUses : jobUsesList ) {
						
			if( !jobUses.isLinkOutput() )
				continue;
			
			if( jobUses.equals( filename ) )
				return PREFIX_OUT+i;
			
			i++;
		}
		
		throw new RuntimeException( "DAX filename '"+filename.getFile()+"' not registered in any direction." );
	}
	
	public String getInputReference( DaxJobUses filename ) {
		
		int i;
		
		i = 1;
		for( DaxJobUses jobUses : jobUsesList ) {
			
			if( !jobUses.isLinkInput() )
				continue;
			
			if( jobUses.equals( filename ) )
				return PREFIX_IN+i;
			
			i++;
		}
		
		throw new RuntimeException( "DAX filename '"+filename.getFile()+"' not registered as input." );
	}
	
	public String getOutputReference( DaxJobUses filename ) {
		
		int i;
		
		i = 1;
		for( DaxJobUses jobUses : jobUsesList ) {
						
			if( !jobUses.isLinkOutput() )
				continue;
			
			if( jobUses.equals( filename ) )
				return PREFIX_OUT+i;
			
			i++;
		}
		
		throw new RuntimeException( "DAX filename '"+filename.getFile()+"' not registered as output." );

	}
	
	public ForeignLambdaExpr getLambda() {
		
		ForeignLambdaExpr lambda;
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		for( DaxJobUses jobUses : jobUsesList ) {
			
			if( jobUses.isLinkInput() )
				continue;
			
			if( isBidirectional( jobUses ) )				
				continue;
			
			buf.append( getReference( jobUses ) ).append( "=\"" );
			buf.append( jobUses.getFile() ).append( "\"\n" );
		}
		
		buf.append( name );
		for( Object arg : argList ) {
			
			buf.append( ' ' );
			
			if( arg instanceof String ) {
				buf.append( arg );
				continue;
			}
			
			if( arg instanceof DaxJobUses ) {
				
				buf.append( '$' );
				buf.append( getReference( ( DaxJobUses )arg ) );
				continue;
			}
			
			throw new RuntimeException( "Argument type not recognized." );
		}
		
		lambda = new ForeignLambdaExpr(
			getPrototype(),
			ForeignLambdaExpr.LANGID_BASH,
			buf.toString() );
		
		return lambda;
	}
	
	public int getChannel( DaxJobUses filename ) {
		
		int channel;
		
		if( filename == null )
			throw new NullPointerException( "DAX filename must not be null." );
		
		channel = 1;
		for( DaxJobUses jobUses : jobUsesList ) {
			
			if( !jobUses.isLinkOutput() )
				continue;
			
			if( jobUses.equals( filename ) )
				return channel;
			
			channel++;
		}
		
		throw new RuntimeException( "DAX filename not registered." );
	}
	
	public ApplyExpr getApplyExpr( DaxJobUses filename ) {
		
		ApplyExpr applyExpr;
		int channel, i;
		
		// find out channel
		channel = getChannel( filename );
		
		// create new apply expression
		applyExpr = new ApplyExpr( channel, false );
		
		// set task expression
		applyExpr.setTaskExpr( new CompoundExpr( getLambda() ) );
		
		// bind parameters
		i = 1;
		for( DaxJobUses jobUses : jobUsesList ) {
			
			if( jobUses.isLinkOutput() )
				continue;
			
			if( isBidirectional( jobUses ) )
				continue;
			
			applyExpr.putAssign(
				new NameExpr( PREFIX_IN+( i++ ) ),
				new CompoundExpr( jobUses.getNameExpr() ) );
		}
		
		return applyExpr;
	}


}
