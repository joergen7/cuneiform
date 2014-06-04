package de.huberlin.cuneiform.dax.semanticmodel;

import java.util.ArrayList;
import java.util.List;

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

	private String id;
	private String name;
	private String version;
	private Integer level;
	private String dvName;
	private String dvVersion;
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
	
	public void addFilenameArg( DaxFilename filename ) {
		
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
	
	public String getDvName() {
		return dvName;
	}
	
	public String getDvVersion() {
		return dvVersion;
	}
	
	public String getId() {
		return id;
	}
	
	public Integer getLevel() {
		return level;
	}
	
	public String getName() {
		
		if( name == null )
			throw new NullPointerException( "DAX job name string has never been set." );
		
		return name;
	}
	
	public String getVersion() {
		return version;
	}
	
	public boolean hasDvName() {
		return dvName != null;
	}
	
	public boolean hasDvVersion() {
		return dvVersion != null;
	}
	
	public boolean hasId() {
		return id != null;
	}
	
	public boolean hasLevel() {
		return level != null;
	}
	
	public boolean hasName() {
		return name != null;
	}
	
	public boolean hasVersion() {
		return version != null;
	}
	
	public boolean isLeaf() {
		return childSet.isEmpty();
	}
	
	public boolean isRoot() {
		return parentSet.isEmpty();
	}
	
	public void setDvName( String dvName ) {
		
		if( dvName == null ) {
			this.dvName = null;
			return;
		}
		
		if( dvName.isEmpty() )
			throw new RuntimeException( "DV name must not be empty." );
		
		this.dvName = dvName;
	}
	
	public void setDvVersion( String dvVersion ) {
		
		if( dvVersion == null ) {
			this.dvVersion = null;
			return;
		}
		
		if( dvVersion.isEmpty() )
			throw new RuntimeException( "DV version must not be empty." );
		
		this.dvVersion = dvVersion;		
	}
	
	public void setId( String id ) {
		
		if( id == null ) {
			this.id = null;
			return;
		}
		
		if( id.isEmpty() )
			throw new RuntimeException( "Id string must not be empty." );
		
		this.id = id;
	}
	
	public void setLevel( Integer level ) {
		
		if( level == null ) {
			this.level = null;
			return;
		}
		
		if( level < 1 )
			throw new RuntimeException( "Level must be a postive integer." );
		
		this.level = level;
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
	
	public void setVersion( String version ) {
		
		if( version == null ) {
			this.version = null;
			return;
		}
		
		if( version.isEmpty() )
			throw new RuntimeException( "Version string must not be empty." );
		
		this.version = version;
			
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
			
			if( jobUses.isLinkOutput() )	
				prototype.addOutput( new NameExpr( PREFIX_OUT+( n++ ), type ) );
			else
				prototype.addParam( new NameExpr( PREFIX_IN+( m++ ), type ) );
			
		}
		
		return prototype;
	}
	
	public ForeignLambdaExpr getLambda() {
		
		ForeignLambdaExpr lambda;
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( name );
		for( Object arg : argList )
			buf.append( ' ' ).append( arg );
		
		lambda = new ForeignLambdaExpr(
			getPrototype(),
			ForeignLambdaExpr.LANGID_BASH,
			buf.toString() );
		
		return lambda;
	}
	
	public int getChannel( DaxFilename filename ) {
		
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
	
	public ApplyExpr getApplyExpr( DaxFilename filename ) {
		
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
			
			applyExpr.putAssign(
				new NameExpr( PREFIX_IN+( i++ ) ),
				new CompoundExpr( jobUses.getNameExpr() ) );
		}
		
		return applyExpr;
	}
}
