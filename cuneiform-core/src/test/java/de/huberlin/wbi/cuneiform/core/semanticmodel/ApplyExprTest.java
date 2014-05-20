package de.huberlin.wbi.cuneiform.core.semanticmodel;

import org.junit.Ignore;

public class ApplyExprTest extends BaseSemanticTest {

	@Ignore
	public void nativePrototypeControlsParallelism() {
		
		appendLine( "deftask align( out : read idx )in bash *{ out=\"$read$idx\" }*" );
		appendLine( "deftask merge( out : <inp> )in bash *{" );
	    appendLine( "    out=merge" );
	    appendLine( "    for i in ${inp[@]}" );
	    appendLine( "    do" );
	    appendLine( "        out=$out$i" );
	    appendLine( "    done" );
		appendLine( "}*" );
		appendLine( "deftask variant-call( out : <read> idx ) {" );
	    appendLine( "    out = merge( inp: align( read: read idx: idx ) );" );
		appendLine( "}" );
		appendLine( "idx = 'a' 'b' 'c';" );
		appendLine( "read = '0' '1' '2';" );
		appendLine( "variant-call( read: read idx: idx );" );
		
		interpret();
		
		assertLengthEquals( 3 );
		
		assertContainsString( "merge0a1a2a" );
		assertContainsString( "merge0b1b2b" );
		assertContainsString( "merge0c1c2c" );
	}

	@Ignore
	public void nativeRestNotation() {
		
		appendLine( "deftask x( <out> : ) { out = y( ~ ); }" );
		appendLine( "deftask y( <out> : <param> ) { out = param; }" );
		appendLine( "x( param: 1 );" );
		
		interpret();
		
		assertLengthEquals( 1 );
		assertContainsString( "1" );
	}
	
	@Ignore
	public void foreignRestNotation() {
		
		appendLine( "deftask x( <out> : ) { out = y( ~ ); }" );
		appendLine( "deftask y( out : param )in bash *{ out=$param; }*" );
		appendLine( "x( param: 1 );" );
		
		interpret();
		
		assertLengthEquals( 1 );
		assertContainsString( "1" );
	}
}
