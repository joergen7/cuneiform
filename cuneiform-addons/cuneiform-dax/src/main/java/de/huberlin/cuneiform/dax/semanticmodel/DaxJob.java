package de.huberlin.cuneiform.dax.semanticmodel;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class DaxJob {

	private String id;
	private String name;
	private String version;
	private Integer level;
	private String dvName;
	private String dvVersion;
	private final List<DaxJob> parentSet;
	private final List<DaxJob> childSet;
	private final List<String> inputVarSet;
	private final List<String> outputVarList;
	private final List<String> plainArgList;
	private final List<DaxFilename> filenameArgList;
	private final List<DaxJobUses> jobUsesList;
	
	public DaxJob() {
		parentSet = new ArrayList<>();
		childSet = new ArrayList<>();
		inputVarSet = new ArrayList<>();
		outputVarList = new ArrayList<>();
		plainArgList = new ArrayList<>();
		filenameArgList = new ArrayList<>();
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
		
		filenameArgList.add( filename );
	}
	
	public void addInputVar( String inputVar ) {
		
		if( inputVar == null )
			throw new NullPointerException( "Input variable name must not be null." );

		if( inputVar.isEmpty() )
			throw new RuntimeException( "Input variable name must not be empty." );
		
		inputVarSet.add( inputVar );
	}
	
	public void addJobUses( DaxJobUses jobUses ) {
		
		if( jobUses == null )
			throw new NullPointerException( "Job-Uses element object must not be null." );
		
		jobUsesList.add( jobUses );
	}
	
	public void addOutputVar( String outputVar ) {
		
		if( outputVar == null )
			throw new NullPointerException( "Output variable name must not be null." );

		if( outputVar.isEmpty() )
			throw new RuntimeException( "Output variable name must not be empty." );
		
		outputVarList.add( outputVar );
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
		
		plainArgList.add( arg );
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
	
	public List<String> getOutputVarList() {
		return Collections.unmodifiableList( outputVarList );
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
			id = null;
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
}
