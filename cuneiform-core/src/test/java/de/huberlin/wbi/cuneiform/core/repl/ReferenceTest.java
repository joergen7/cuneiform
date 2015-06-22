package de.huberlin.wbi.cuneiform.core.repl;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static de.huberlin.wbi.cuneiform.core.semanticmodel.ForeignLambdaExpr.LANGID_BASH;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Block;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CondExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CorrelParam;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ForeignLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NativeLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NodeVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Prototype;
import de.huberlin.wbi.cuneiform.core.semanticmodel.QualifiedTicket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ReduceVar;
import de.huberlin.wbi.cuneiform.core.semanticmodel.SemanticModelException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.StringExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.ticketsrc.NodeVisitorTicketSrc;

public class ReferenceTest {

	private DynamicNodeVisitor dnv;
	private TopLevelContext tlc;
	private NodeVisitorTicketSrc ticketSrc;

	@Before
	public void setUp() throws HasFailedException, NotDerivableException {
		
		BaseRepl repl;
		QualifiedTicket qt;
		
		
		qt = mock( QualifiedTicket.class );
		when( qt.getOutputValue() ).thenThrow( new NotDerivableException( "blub" ) );
		when( qt.getNumAtom() ).thenThrow( new NotDerivableException( "blub" ) );
		
		ticketSrc = mock( NodeVisitorTicketSrc.class );
		when( ticketSrc.requestTicket( any( BaseRepl.class ), any( UUID.class ), any( ApplyExpr.class ) ) ).
		thenReturn( qt );
		
		repl = mock( BaseRepl.class );
		tlc = new TopLevelContext();
		
		dnv = new DynamicNodeVisitor( ticketSrc, repl, tlc );
	}
	
	@Test
	public void nilShouldEvalItself() throws HasFailedException, NotBoundException {
		
		CompoundExpr nil, result;
		
		nil = new CompoundExpr();
		result = dnv.accept( nil );
		assertEquals( nil, result );
	}
	
	@Test
	public void strShouldEvalItself() throws HasFailedException, NotBoundException {
		
		CompoundExpr str, result;
		
		str = new CompoundExpr( new StringExpr( "bla" ) );
		result = dnv.accept( str );
		assertEquals( str, result );
	}
	
	@Test( expected=NotBoundException.class )
	public void undefVarShouldFail() throws HasFailedException, NotBoundException {
		
		CompoundExpr freeVar;
		
		freeVar = new CompoundExpr( new NameExpr( "x" ) );
		dnv.accept( freeVar );
	}
	
	@Test
	public void defVarShouldEvalToBoundValue() throws HasFailedException, NotBoundException {
		
		CompoundExpr boundVar, result, content;
		
		content = new CompoundExpr( new StringExpr( "blub" ) );
		tlc.putAssign( new NameExpr( "x" ), content );
		
		boundVar = new CompoundExpr( new NameExpr( "x" ) );
		result = dnv.accept( boundVar );
		
		assertEquals( content, result );
	}
	
	@Test
	public void defVarShouldCascadeBinding() throws HasFailedException, NotBoundException {
		
		CompoundExpr result, str;
		
		str = new CompoundExpr( new StringExpr( "blub" ) );
		
		tlc.putAssign( new NameExpr( "x" ), new CompoundExpr( new NameExpr( "y" ) ) );
		tlc.putAssign( new NameExpr( "y" ), str );
		
		result = dnv.accept( new CompoundExpr( new NameExpr( "x" ) ) );
		assertEquals( str, result );
	}
	
	@Test
	public void unfinishedTicketShouldEvalToItself() throws HasFailedException, NotBoundException {

		CompoundExpr ce;
		QualifiedTicket qt;
		
		qt =  mock( QualifiedTicket.class );
		when( qt.visit( dnv ) ).thenReturn( dnv.accept( qt ) );
		
		ce = new CompoundExpr( );
		
		assertEquals( ce, dnv.accept( ce ) );
	}
	
	@Test
	public void identityFnShouldEvalArg() throws HasFailedException, NotBoundException {
		
		CompoundExpr e, f, lamList;
		Prototype sign;
		Block body;
		ApplyExpr ae;
		
		
		e = new CompoundExpr( new StringExpr( "bla" ) );
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "inp" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "inp" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		ae.putAssign( new NameExpr( "inp" ), e );
		
		f = new CompoundExpr( ae );
		
		assertEquals( e, dnv.accept( f ) );
	}
	
	@Test
	public void lamShouldEvalItself() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		CompoundExpr lamList;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "blub" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		assertEquals( lamList, dnv.accept( lamList ) );
	}
	
	@Test
	public void multipleOutputShouldBeBindable() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CompoundExpr e1, e2, lamList;
		ApplyExpr f1, f2;
		Block body;
		
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out1" ) );
		sign.addOutput( new NameExpr( "out2" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		e1 = new CompoundExpr( new StringExpr( "bla" ) );
		e2 = new CompoundExpr( new StringExpr( "blub" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out1" ), e1 );
		body.putAssign( new NameExpr( "out2" ), e2 );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		f1 = new ApplyExpr( 1, false );
		f1.setTaskExpr( lamList );
		
		f2 = new ApplyExpr( 2, false );
		f2.setTaskExpr( lamList );
		
		assertEquals( e1, dnv.accept( f1 ) );
		assertEquals( e2, dnv.accept( f2 ) );
	}
	
	@Test( expected=SemanticModelException.class )
	public void applicationShouldIgnoreCallingContext() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		CompoundExpr lamList;
		ApplyExpr ae;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "x" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		
		tlc.putAssign( new NameExpr( "x" ), new CompoundExpr( new StringExpr( "blub" ) ) );
		dnv.accept( new CompoundExpr( ae ) );
	}
	
	@Test
	public void bindingShouldOverrideBody() throws HasFailedException, NotBoundException {
		
		CompoundExpr f, g, h, lamList;
		Prototype sign;
		Block body;
		ApplyExpr ae;
		
		f = new CompoundExpr( new StringExpr( "blub" ) );
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "x" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "x" ), new CompoundExpr( new StringExpr( "bla" ) ) );
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new NameExpr( "x" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		ae.putAssign( new NameExpr( "x" ), f );
		
		g = new CompoundExpr( ae );
		h = dnv.accept( g );
		
		assertEquals( f, h );
	}
	
	@Test( expected=NullPointerException.class )
	public void appWithEmptyTaskListShouldFail() throws HasFailedException, NotBoundException {
		
		ApplyExpr ae;
		
		ae = new ApplyExpr( 1, false );
		dnv.accept( new CompoundExpr( ae ) );
	}
	
	@Test
	public void crossProductShouldBeDerivableFromSignature() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CompoundExpr e1, e2, lamList, f1, f2;
		Block body;
		ApplyExpr ae1, ae2;
		
		e1 = new CompoundExpr();
		e1.addSingleExpr( new StringExpr( "A" ) );
		e1.addSingleExpr( new StringExpr( "B" ) );
		
		e2 = new CompoundExpr();
		e2.addSingleExpr( new StringExpr( "1" ) );
		e2.addSingleExpr( new StringExpr( "2" ) );
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out1" ) );
		sign.addOutput( new NameExpr( "out2" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "p1" ) );
		sign.addParam( new NameExpr( "p2" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out1" ), new CompoundExpr( new NameExpr( "p1" ) ) );
		body.putAssign( new NameExpr( "out2" ), new CompoundExpr( new NameExpr( "p2" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae1 = new ApplyExpr( 1, false );
		ae1.setTaskExpr( lamList );
		ae1.putAssign( new NameExpr( "p1" ), e1 );
		ae1.putAssign( new NameExpr( "p2" ), e2 );
		
		ae2 = new ApplyExpr( 2, false );
		ae2.setTaskExpr( lamList );
		ae2.putAssign( new NameExpr( "p1" ), e1 );
		ae2.putAssign( new NameExpr( "p2" ), e2 );
		
		f1 = new CompoundExpr();
		f1.addSingleExpr( new StringExpr( "A" ) );
		f1.addSingleExpr( new StringExpr( "A" ) );
		f1.addSingleExpr( new StringExpr( "B" ) );
		f1.addSingleExpr( new StringExpr( "B" ) );
		
		f2 = new CompoundExpr();
		f2.addSingleExpr( new StringExpr( "1" ) );
		f2.addSingleExpr( new StringExpr( "2" ) );
		f2.addSingleExpr( new StringExpr( "1" ) );
		f2.addSingleExpr( new StringExpr( "2" ) );
		
		assertEquals( f1, dnv.accept( new CompoundExpr( ae1 ) ) );
		assertEquals( f2, dnv.accept( new CompoundExpr( ae2 ) ) );
	}
	   
	public void dotProductShouldBeDerivableFromSignature() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CorrelParam correl;
		CompoundExpr e1, e2, lamList;
		Block body;
		ApplyExpr ae1, ae2;
		
		
		correl = new CorrelParam();
		correl.addName( new NameExpr( "p1" ) );
		correl.addName( new NameExpr( "p2" ) );
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out1" ) );
		sign.addOutput( new NameExpr( "out2" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( correl );
		
		e1 = new CompoundExpr();
		e1.addSingleExpr( new StringExpr( "A" ) );
		e1.addSingleExpr( new StringExpr( "B" ) );
		
		e2 = new CompoundExpr();
		e2.addSingleExpr( new StringExpr( "1" ) );
		e2.addSingleExpr( new StringExpr( "2" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out1" ), new CompoundExpr( new NameExpr( "p1" ) ) );
		body.putAssign( new NameExpr( "out2" ), new CompoundExpr( new NameExpr( "p2" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae1 = new ApplyExpr( 1, false );
		ae1.setTaskExpr( lamList );
		ae1.putAssign( new NameExpr( "p1" ), e1 );
		ae1.putAssign( new NameExpr( "p2" ), e2 );
		
		ae2 = new ApplyExpr( 2, false );
		ae2.setTaskExpr( lamList );
		ae2.putAssign( new NameExpr( "p1" ), e1 );
		ae2.putAssign( new NameExpr( "p2" ), e2 );
		
		assertEquals( e1, dnv.accept( new CompoundExpr( ae1 ) ) );
		assertEquals( e2, dnv.accept( new CompoundExpr( ae2 ) ) );
	}
	
	@Test
	public void aggregateShouldConsumeListAsWhole() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CompoundExpr e1, e2, e3, e4, lamList;
		Block body;
		ApplyExpr ae;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new ReduceVar( "inp", null ) );
		
		e1 = new CompoundExpr( new StringExpr( "A" ) );
		
		e2 = new CompoundExpr();
		e2.addSingleExpr( new StringExpr( "B" ) );
		e2.addSingleExpr( new StringExpr( "C" ) );
		
		e3 = new CompoundExpr();
		e3.addCompoundExpr( e1 );
		e3.addSingleExpr( new NameExpr( "inp" ) );
		
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), e3 );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		ae.putAssign( new NameExpr( "inp" ), e2 );
		
		e4 = new CompoundExpr();
		e4.addCompoundExpr( e1 );
		e4.addCompoundExpr( e2 );
		
		assertEquals( e4, dnv.accept( new CompoundExpr( ae ) ) );
	}
	
	// @Test
	public void taskCorrelationShouldWork() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CorrelParam correl;
		Block body1, body2;
		CompoundExpr e1, e2, e3, e4, e5, lamList;
		ApplyExpr ae;
		
		correl = new CorrelParam();
		correl.addName( new NameExpr( "task" ) );
		correl.addName( new NameExpr( "c" ) );
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( correl );
		sign.addParam( new NameExpr( "p" ) );
		
		e1 = new CompoundExpr();
		e1.addSingleExpr( new NameExpr( "c" ) );
		e1.addSingleExpr( new NameExpr( "p" ) );
		
		e2 = new CompoundExpr();
		e2.addSingleExpr( new NameExpr( "p" ) );
		e2.addSingleExpr( new NameExpr( "c" ) );
		
		body1 = new Block();
		body1.putAssign( new NameExpr( "out" ), e1 );
		
		body2 = new Block();
		body2.putAssign( new NameExpr( "out" ), e2 );
		
		lamList = new CompoundExpr();
		lamList.addSingleExpr( new NativeLambdaExpr( sign, body1 ) );
		lamList.addSingleExpr( new NativeLambdaExpr( sign, body2 ) );
		
		e3 = new CompoundExpr();
		e3.addSingleExpr( new StringExpr( "A" ) );
		e3.addSingleExpr( new StringExpr( "B" ) );
		
		e4 = new CompoundExpr();
		e4.addSingleExpr( new StringExpr( "1" ) );
		e4.addSingleExpr( new StringExpr( "2" ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		ae.putAssign( new NameExpr( "c" ), e3 );
		ae.putAssign( new NameExpr( "p" ), e4 );
		
		e5 = new CompoundExpr();
		e5.addSingleExpr( new StringExpr( "A" ) );
		e5.addSingleExpr( new StringExpr( "1" ) );
		e5.addSingleExpr( new StringExpr( "A" ) );
		e5.addSingleExpr( new StringExpr( "2" ) );
		e5.addSingleExpr( new StringExpr( "1" ) );
		e5.addSingleExpr( new StringExpr( "B" ) );
		e5.addSingleExpr( new StringExpr( "2" ) );
		e5.addSingleExpr( new StringExpr( "B" ) );

		assertEquals( e5, dnv.accept( new CompoundExpr( ae ) ) );
	}
	
	@Test
	public void cndFalseShouldEvalElseExpr() throws HasFailedException, NotBoundException {
		
		CompoundExpr e, ifExpr, thenExpr, elseExpr, e1;
		
		e1 = new CompoundExpr( new StringExpr( "B" ) );
		
		ifExpr = new CompoundExpr();
		thenExpr = new CompoundExpr( new StringExpr( "A" ) );
		elseExpr = e1;
		
		e = new CompoundExpr( new CondExpr( ifExpr, thenExpr, elseExpr ) );
		assertEquals( e1, dnv.accept( e ) );
	}
	
	@Test
	public void cndEvaluatesConditionBeforeDecision1() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		CompoundExpr lamList, e, a, b;
		ApplyExpr ae;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr() );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		
		a = new CompoundExpr( new StringExpr( "A" ) );
		b = new CompoundExpr( new StringExpr( "B" ) );
		
		e = new CompoundExpr( new CondExpr( new CompoundExpr( ae ), a, b ) );
		
		assertEquals( b, dnv.accept( e ) );
	}
	


	@Test
	public void cndEvaluatesConditionBeforeDecision2() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		CompoundExpr lamList, e, a, b, x, y;
		ApplyExpr ae;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		x = new CompoundExpr( new StringExpr( "X" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), x );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		
		a = new CompoundExpr( new StringExpr( "A" ) );
		b = new CompoundExpr( new StringExpr( "B" ) );
		
		e = new CompoundExpr( new CondExpr( new CompoundExpr( ae ), a, b ) );
		
		y = dnv.accept( e );
		
		assertEquals( a, y );
	}
	
	@Test
	public void cndEvaluatesOnlyOnFinalCondition() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CompoundExpr lamList, e, a, b, x;
		ApplyExpr ae;
		CondExpr ce;
		
		sign = new Prototype();
		sign.addOutput( new ReduceVar( "out", null ) );
		sign.addParam( new NameExpr( "task" ) );
		
		lamList = new CompoundExpr(
			new ForeignLambdaExpr(
				sign, ForeignLambdaExpr.LANGID_BASH, "blub" ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		
		a = new CompoundExpr( new NameExpr( "a" ) );
		b = new CompoundExpr( new NameExpr( "b" ) );
		
		e = new CompoundExpr( new CondExpr( new CompoundExpr( ae ), a, b ) );
				
		tlc.putAssign( new NameExpr( "a" ), new CompoundExpr( new StringExpr( "A" ) ) );
		tlc.putAssign( new NameExpr( "b" ), new CompoundExpr( new StringExpr( "A" ) ) );
		x = dnv.accept( e );
		
		assertTrue( x.getSingleExpr( 0 ) instanceof CondExpr );
		
		ce = ( CondExpr )x.getSingleExpr( 0 );
		assertEquals( a, ce.getThenExpr() );
		assertEquals( b, ce.getElseExpr() );
		
		assertTrue( ce.getIfExpr().getSingleExpr( 0 ) instanceof QualifiedTicket );
	}
	
	@Test
	public void cndEvaluatesThenExpr() throws HasFailedException, NotBoundException {
		
		CompoundExpr e, f, x;
		
		e = new CompoundExpr( new CondExpr(
			new CompoundExpr( new StringExpr( "Z" ) ),
			new CompoundExpr( new NameExpr( "x" ) ),
			new CompoundExpr( new StringExpr( "B" ) ) ) );
		
		f = new CompoundExpr( new StringExpr( "A" ) );
		
		tlc.putAssign( new NameExpr( "x" ), f );
		x = dnv.accept( e );
		
		assertEquals( f, x );
	}

	@Test
	public void cndEvaluatesElseExpr() throws HasFailedException, NotBoundException {
		
		CompoundExpr e, f, x;
		
		e = new CompoundExpr( new CondExpr(
			new CompoundExpr(),
			new CompoundExpr( new StringExpr( "A" ) ),
			new CompoundExpr( new NameExpr( "x" ) ) ) );
		
		f = new CompoundExpr( new StringExpr( "B" ) );
		
		tlc.putAssign( new NameExpr( "x" ), f );
		x = dnv.accept( e );
		
		assertEquals( f, x );
	}

	
	@Test
	public void foreignAppWithCndParamIsLeftUntouched() throws HasFailedException, NotBoundException {
		
		CompoundExpr lamList, e, x;
		Prototype sign;
		ApplyExpr ae1, ae2, ae3;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "p" ) );
		
		lamList = new CompoundExpr( new ForeignLambdaExpr(
			sign, LANGID_BASH, "blub" ) );
		
		ae1 = new ApplyExpr( 1, false );
		ae1.setTaskExpr( lamList );
		ae1.putAssign(
			new NameExpr( "p" ), new CompoundExpr( new StringExpr( "A" ) ) );
		
		e = new CompoundExpr( new CondExpr(
			new CompoundExpr( ae1 ), new CompoundExpr(), new CompoundExpr() ) );
		
		ae2 = new ApplyExpr( 1, false );
		ae2.setTaskExpr( lamList );
		ae2.putAssign( new NameExpr( "p" ), e );
		
		x = dnv.accept( new CompoundExpr( ae2 ) );
				
		assertTrue( x.getSingleExpr( 0 ) instanceof ApplyExpr );
		
		ae3 = ( ApplyExpr )x.getSingleExpr( 0 );
		
		assertEquals( lamList, ae3.getTaskExpr() );
	}
	
	@Test
	public void foreignAppWithSelectParamIsLeftUntouched() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		CompoundExpr lamList, x;
		ApplyExpr ae1, ae2, ae3;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		sign.addParam( new NameExpr( "p" ) );
		
		lamList = new CompoundExpr( new ForeignLambdaExpr(
			sign, LANGID_BASH, "blub" ) );
		
		ae1 = new ApplyExpr( 1, false );
		ae1.setTaskExpr( lamList );
		ae1.putAssign( new NameExpr( "p" ), new CompoundExpr( new StringExpr( "A" ) ) );
		
		ae2 = new ApplyExpr( 1, false );
		ae2.setTaskExpr( lamList );
		ae2.putAssign( new NameExpr( "p" ), new CompoundExpr( ae1 ) );
		
		x = dnv.accept( new CompoundExpr( ae2 ) );
		
		assertTrue( x.getSingleExpr( 0 ) instanceof ApplyExpr );
		
		ae3 = ( ApplyExpr )x.getSingleExpr( 0 );
		
		assertEquals( lamList, ae3.getTaskExpr() );
	}
	
	@Test
	public void cascadingAppDoesNotBreakEnum() throws HasFailedException, NotBoundException, NotDerivableException {
		
		Prototype sign1, sign2;
		CompoundExpr lam1, lam2, out1, out2, x;
		ApplyExpr ae1, ae2;
		Block body2;
		QualifiedTicket qt1;
		
		sign1 = new Prototype();
		sign1.addOutput( new ReduceVar( "out", null ) );
		sign1.addParam( new NameExpr( "task" ) );
		
		lam1 = new CompoundExpr( new ForeignLambdaExpr( sign1, LANGID_BASH, "out=(1 2 3)" ) );
		
		ae1 = new ApplyExpr( 1, false );
		ae1.setTaskExpr( lam1 );
		
		sign2 = new Prototype();
		sign2.addOutput( new NameExpr( "out" ) );
		sign2.addParam( new NameExpr( "task" ) );
		sign2.addParam( new NameExpr( "p1" ) );
		sign2.addParam( new NameExpr( "p2" ) );
		
		out1 = new CompoundExpr();
		out1.addSingleExpr( new NameExpr( "p1" ) );
		out1.addSingleExpr( new NameExpr( "p2" ) );
		
		body2 = new Block();
		body2.putAssign( new NameExpr( "out" ), out1 );
		
		lam2 = new CompoundExpr( new NativeLambdaExpr( sign2, body2 ) );
		
		ae2 = new ApplyExpr( 1, false );
		ae2.setTaskExpr( lam2 );
		ae2.putAssign( new NameExpr( "p1" ), new CompoundExpr( new StringExpr( "A" ) ) );
		ae2.putAssign( new NameExpr( "p2" ), new CompoundExpr( ae1 ) );
		
		out2 = new CompoundExpr();
		out2.addSingleExpr( new StringExpr( "1" ) );
		out2.addSingleExpr( new StringExpr( "2" ) );
		
		qt1 = mock( QualifiedTicket.class );
		when( qt1.getChannel() ).thenReturn( 1 );
		when( qt1.getNumAtom() ).thenReturn( 2 );
		when( qt1.getOutputValue() ).thenReturn( out2 );
		when( qt1.getStringExprValue( 0 ) ).thenReturn( new StringExpr( "1" ) );
		when( qt1.getStringExprValue( 1 ) ).thenReturn( new StringExpr( "2" ) );
		
		when( ticketSrc.requestTicket(
				any( BaseRepl.class ),
				any( UUID.class ),
				any( ApplyExpr.class ) ) )
			.thenReturn( qt1 );
		
		x = dnv.accept( new CompoundExpr( ae2 ) );
		
		assertEquals( "'A' '1' 'A' '2'", x.toString() );
	}
	
	@Test
	public void appTaskParamIsEvaluated() throws HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		CompoundExpr lamList, x;
		ApplyExpr ae;
		
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( new StringExpr( "A" ) ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( new CompoundExpr( new NameExpr( "f" ) ) );
		
		tlc.putAssign( new NameExpr( "f" ), lamList );
		
		x = dnv.accept( new CompoundExpr( ae ) );
		
		assertEquals( new CompoundExpr( new StringExpr( "A" ) ), x );
	}
	
	@Test
	public void appNonFinalResultPreservesApp() throws NotDerivableException, HasFailedException, NotBoundException {
		
		Prototype sign;
		Block body;
		QualifiedTicket qt1;
		CompoundExpr lamList, x;
		ApplyExpr ae;
		
		sign = new Prototype();
		sign.addOutput( new NameExpr( "out" ) );
		sign.addParam( new NameExpr( "task" ) );
		
		qt1 = mock( QualifiedTicket.class );
		when( qt1.getNumAtom() ).thenThrow( new NotDerivableException( "blub" ) );
		when( qt1.getOutputValue() ).thenThrow( new NotDerivableException( "blub" ) );
		when( qt1.visit( dnv ) ).thenReturn( new CompoundExpr( qt1 ) );
		
		body = new Block();
		body.putAssign( new NameExpr( "out" ), new CompoundExpr( qt1 ) );
		
		lamList = new CompoundExpr( new NativeLambdaExpr( sign, body ) );
		
		ae = new ApplyExpr( 1, false );
		ae.setTaskExpr( lamList );
		
		x = dnv.accept( new CompoundExpr( ae ) );
		assertEquals( new CompoundExpr( ae ), x );
	}
	

}
