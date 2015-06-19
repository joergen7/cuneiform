package de.huberlin.wbi.cuneiform.core.repl;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Block;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CondExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ForeignLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.LambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NativeLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Prototype;
import de.huberlin.wbi.cuneiform.core.semanticmodel.QualifiedTicket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.StringExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.ticketsrc.NodeVisitorTicketSrc;

public class DynamicNodeVisitorTest {

	private DynamicNodeVisitor dnv;
	private CompoundExpr x;
	
	@Before
	public void setUp() throws HasFailedException, NotDerivableException {
		
		NodeVisitorTicketSrc ticketSrc;
		BaseRepl repl;
		TopLevelContext tlc;
		QualifiedTicket qt;
		
		x = new CompoundExpr( new StringExpr( "Z" ) );
		
		qt = mock( QualifiedTicket.class );
		when( qt.getOutputValue() ).thenThrow( new NotDerivableException( "blub" ) );
		
		ticketSrc = mock( NodeVisitorTicketSrc.class );
		when( ticketSrc.requestTicket( any( BaseRepl.class ), any( UUID.class ), any( ApplyExpr.class ) ) ).
		thenReturn( qt );
		
		repl = mock( BaseRepl.class );
		tlc = new TopLevelContext();
		tlc.putAssign( new NameExpr( "x" ), x );
		
		dnv = new DynamicNodeVisitor( ticketSrc, repl, tlc );
	}
	
	@Test
	public void cndFalseShouldEvalElseExpr() throws HasFailedException, NotBoundException {
		
		CondExpr condExpr;
		CompoundExpr thenExpr, elseExpr, result;
		
		thenExpr = mock( CompoundExpr.class );
		elseExpr = new CompoundExpr( new StringExpr( "B" ) );
		
		condExpr = new CondExpr( new CompoundExpr(), thenExpr, elseExpr );
		result = dnv.accept( condExpr );
		
		assertEquals( elseExpr, result );
		
	}
	
	@Test
	public void cndEvaluatesConditionBeforeDecision1() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		LambdaExpr lam;
		ApplyExpr app;
		CompoundExpr e, thenExpr, elseExpr;
		CondExpr condExpr;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr() );
		
		lam = new NativeLambdaExpr( sign, body );
		
		app = new ApplyExpr( 1, false );
		app.setTaskExpr( new CompoundExpr( lam ) );
		
		thenExpr = new CompoundExpr( new StringExpr( "A" ) );
		elseExpr = new CompoundExpr( new StringExpr( "B" ) );
		
		condExpr = new CondExpr( new CompoundExpr( app ), thenExpr, elseExpr );
		
		e = new CompoundExpr( condExpr );
		
		assertEquals( elseExpr, dnv.accept( e ) );
	}
	
	@Test
	public void cndEvaluatesConditionBeforeDecision2() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		LambdaExpr lam;
		ApplyExpr app;
		CompoundExpr e, thenExpr, elseExpr;
		CondExpr condExpr;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new StringExpr( "true" ) ) );
		
		lam = new NativeLambdaExpr( sign, body );
		
		app = new ApplyExpr( 1, false );
		app.setTaskExpr( new CompoundExpr( lam ) );
		
		thenExpr = new CompoundExpr( new StringExpr( "A" ) );
		elseExpr = new CompoundExpr( new StringExpr( "B" ) );
		
		condExpr = new CondExpr( new CompoundExpr( app ), thenExpr, elseExpr );
		
		e = new CompoundExpr( condExpr );
		
		assertEquals( thenExpr, dnv.accept( e ) );
	}
	
	@Test
	public void cndEvaluatesOnlyOnFinalCondition() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		LambdaExpr lam;
		ApplyExpr app;
		CompoundExpr e, f, thenExpr, elseExpr;
		CondExpr condExpr;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new StringExpr( "true" ) ) );
		
		lam = new ForeignLambdaExpr( sign, "bash", "blub" );
		
		app = new ApplyExpr( 1, false );
		app.setTaskExpr( new CompoundExpr( lam ) );
		
		thenExpr = new CompoundExpr( new StringExpr( "A" ) );
		elseExpr = new CompoundExpr( new StringExpr( "B" ) );
		
		condExpr = new CondExpr( new CompoundExpr( app ), thenExpr, elseExpr );
		
		e = new CompoundExpr( condExpr );
		
		f = dnv.accept( e );
		assertFalse( f.isNormal() );
		assertEquals( 1, f.getNumSingleExpr() );
		assertTrue( f.getSingleExpr( 0 ) instanceof CondExpr );
	}
	
	@Test
	public void cndEvaluatesThenExpression() throws HasFailedException, NotBoundException {
		
		CompoundExpr e, thenExpr, elseExpr;
		CondExpr condExpr;
		

		
		thenExpr = new CompoundExpr( new NameExpr( "x" ) );
		elseExpr = new CompoundExpr( new StringExpr( "B" ) );
		
		condExpr = new CondExpr( new CompoundExpr( new StringExpr( "X" ) ), thenExpr, elseExpr );
		
		e = new CompoundExpr( condExpr );
		
		assertEquals( x, dnv.accept( e ) );
	}
	
	@Test
	public void cndEvaluatesElseExpression() throws HasFailedException, NotBoundException {
		
		CompoundExpr e, thenExpr, elseExpr;
		CondExpr condExpr;
		

		
		thenExpr = new CompoundExpr( new StringExpr( "A" ) );
		elseExpr = new CompoundExpr( new NameExpr( "x" ) );
		
		condExpr = new CondExpr( new CompoundExpr(), thenExpr, elseExpr );
		
		e = new CompoundExpr( condExpr );
		
		assertEquals( x, dnv.accept( e ) );
	}
	
}
