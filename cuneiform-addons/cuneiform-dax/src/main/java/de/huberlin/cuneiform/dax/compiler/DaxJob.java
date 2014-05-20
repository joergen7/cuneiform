package de.huberlin.cuneiform.dax.compiler;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class DaxJob implements CfConvertable {

	private String id;
	private String name;
	private String body;
	private Set<DaxJob> parentSet;
	private Set<DaxJob> childSet;
	private Set<String> inputVarSet;
	private List<String> outputVarList;
	
	public DaxJob( String id ) {
		parentSet = new HashSet<>();
		childSet = new HashSet<>();
		inputVarSet = new HashSet<>();
		outputVarList = new LinkedList<>();
		setId( id );
	}
	
	public void addChild( DaxJob child ) {
		
		if( child == null )
			throw new NullPointerException( "Child DAX job must not be null." );
		
		childSet.add( child );
	}
	
	public void addInputVar( String inputVar ) {
		
		if( inputVar == null )
			throw new NullPointerException( "Input variable name must not be null." );

		if( inputVar.isEmpty() )
			throw new RuntimeException( "Input variable name must not be empty." );
		
		inputVarSet.add( inputVar );
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
	
	public String getBody() {
		
		if( body == null )
			throw new NullPointerException( "DAX job body string has never been set." );
		
		return body;
	}
	
	public String getId() {
		return id;
	}
	
	public String getName() {
		
		if( name == null )
			throw new NullPointerException( "DAX job name string has never been set." );
		
		return name;
	}
	
	public List<String> getOutputVarList() {
		return Collections.unmodifiableList( outputVarList );
	}
	
	public boolean hasBody() {
		return body != null;
	}
	
	public boolean hasName() {
		return name != null;
	}
	
	public boolean isLeaf() {
		return childSet.isEmpty();
	}
	
	public void setBody( String body ) {
		
		if( body == null ) {
			this.body = null;
			return;
		}

		if( body.isEmpty() )
			throw new RuntimeException( "Body string must not be empty." );
		
		this.body = body;		
	}
	
	public void setId( String id ) {
		
		if( id == null )
			throw new NullPointerException( "Id string must not be null." );

		if( id.isEmpty() )
			throw new RuntimeException( "Id string must not be empty." );
		
		this.id = id;
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
	
	@Override
	public String toCuneiform() {
		
		StringBuffer buf;
		
		if( outputVarList.isEmpty() )
			throw new RuntimeException( "Job has no output variables." );
		
		buf = new StringBuffer();
		
		for( String outputVar : outputVarList )			
			buf.append( outputVar ).append( ' ' );
		
		buf.append( "= apply(\n    task: \\( " );
		
		for( String outputVar : outputVarList )			
			buf.append( outputVar ).append( ' ' );
		
		buf.append( ": " );
		
		for( String inputVar : inputVarSet )			
			buf.append( inputVar ).append( ' ' );
		
		buf.append( ")in bash *{\n        " );
		buf.append( body );
		buf.append( "\n    }*\n" );
		
		for( String inputVar : inputVarSet )			
			buf.append( inputVar ).append( ": " ).append( inputVar ).append( '\n' );
		
		buf.append( ");\n" );
		
		return buf.toString();
	}
}
