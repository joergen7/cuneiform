package de.huberlin.wbi.cuneiform.core.semanticmodel;

import org.junit.Ignore;

public class CurryExprTest extends BaseSemanticTest {

	@Ignore
	public void nativeCurry() {
	
		appendLine( "deftask my-task( out : a b ) { out = a; }" );
		appendLine( "my-curried-task = curry( task: my-task b: 2 );" );
		appendLine( "my-curried-task( a: 1 );" );
		
		interpret();
		
		assertLengthEquals( 1 );
		assertContainsString( "1" );
	}

	@Ignore
	public void foreignCurry() {
	
		appendLine( "deftask my-task( out : a b )in bash *{ out=$a }*" );
		appendLine( "my-curried-task = curry( task: my-task b: 2 );" );
		appendLine( "my-curried-task( a: 1 );" );
		
		interpret();
		
		assertLengthEquals( 1 );
		assertContainsString( "1" );
	}

}
