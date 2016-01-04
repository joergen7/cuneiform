package de.huberlin.wbi.cuneiform.core.semanticmodel;

import org.junit.Test;
import static org.junit.Assert.assertNotEquals;

import java.util.UUID;


public class HashHelperTest {

	
	@SuppressWarnings("static-method")
	@Test
	public void hashPreservesDifferencesBetweenStrings() {
		
		String a, b;
		long h1, h2;
				
		a = "bla";
		b = "blub";
		
		h1 = HashHelper.add( 0L, a );
		h2 = HashHelper.add( 0L, b );
		
		assertNotEquals( h1, h2 );
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void hashPreservesDifferencesBetweenTickets() {
		
		Ticket a, b;
		long h1, h2;
		UUID uuid;
		ForeignLambdaExpr lambda1, lambda2;
		Block bindingBlock;
		Prototype prototype;
		String lang;
		String body1, body2;
		
		uuid = UUID.randomUUID();
		bindingBlock = new Block();
		
		prototype = new Prototype();
		lang = ForeignLambdaExpr.LANGID_BASH;
		body1 = "bla";
		body2 = "blub";
		
		lambda1 = new ForeignLambdaExpr( prototype, lang, body1 );
		lambda2 = new ForeignLambdaExpr( prototype, lang, body2 );

		a = new Ticket( lambda1, bindingBlock, uuid );
		b = new Ticket( lambda2, bindingBlock, uuid );
		
		h1 = HashHelper.add( 0L, a );
		h2 = HashHelper.add( 0L, b );
		
		assertNotEquals( h1, h2 );
	}
}
