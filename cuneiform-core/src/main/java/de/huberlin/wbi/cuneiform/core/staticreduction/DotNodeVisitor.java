package de.huberlin.wbi.cuneiform.core.staticreduction;

import java.util.HashMap;
import java.util.Map;

import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Block;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CondExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CorrelParam;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CurryExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.DataType;
import de.huberlin.wbi.cuneiform.core.semanticmodel.DrawParam;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ForeignLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.LambdaType;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NativeLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CfNode;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NodeVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Prototype;
import de.huberlin.wbi.cuneiform.core.semanticmodel.QualifiedTicket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ReduceVar;
import de.huberlin.wbi.cuneiform.core.semanticmodel.SingleExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.StringExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;

public class DotNodeVisitor implements NodeVisitor<String> {

	private static final int MAX_LEN = 20;
	private static final String LABEL_FOREIGN_LAMBDA_EXPR = "\\\\ {}";
	private static final String LABEL_NATIVE_LAMBDA_EXPR = "\\\\ *{}*";
	private static final String LABEL_TASK = "task";
	private static final String LABEL_APPLY = "apply";
	private static final String LABEL_CURRY = "curry";

	private final StringBuffer buf;
	private final Map<CfNode,String> exprMap;
	private int nextNodeId;


	public DotNodeVisitor() {

		buf = new StringBuffer();
		nextNodeId = 0;
		exprMap = new HashMap<>();		
	}
	
	@Override
	public String toString() {
		return "digraph G {\n"+buf.toString()+"}\n";
	}
	
	@Override
	public String accept(StringExpr stringExpr) {
		
		String nodeId;
		String value;
		
		if( exprMap.containsKey( stringExpr ) )
			return exprMap.get( stringExpr );
		
		nodeId = popNodeId();
		value = stringExpr.getContent();
		
		if( value.length() > MAX_LEN )
			value = "..";
		
		buf.append( nodeId ).append( " [shape=none,label=\"'"+value+"'\"];\n" );
		
		exprMap.put( stringExpr, nodeId );
		
		return nodeId;
	}



	@Override
	public String accept(QualifiedTicket qualifiedTicket) {
		throw new RuntimeException( "Illegal qualified ticket." );
	}



	@Override
	public String accept(NativeLambdaExpr nativeLambdaExpr) {
		
		String nodeId;
		
		if( exprMap.containsKey( nativeLambdaExpr ) )
			return exprMap.get( nativeLambdaExpr );
		
		nodeId = popNodeId();
		
		buf.append( nodeId ).append( " [shape=none,label=\""+LABEL_NATIVE_LAMBDA_EXPR+"\"];\n" );
		
		exprMap.put( nativeLambdaExpr, nodeId );
		
		return nodeId;
	}



	@Override
	public String accept( NameExpr nameExpr ) {
		
		String nodeId;
		String value;
		
		if( exprMap.containsKey( nameExpr ) )
			return exprMap.get( nameExpr );
		
		nodeId = popNodeId();
		value = nameExpr.getId();
		
		if( value.length() > MAX_LEN )
			value = "..";
		
		buf.append( nodeId ).append( " [shape=none,label=\""+value+"\"];\n" );
		
		exprMap.put( nameExpr, nodeId );
		
		return nodeId;
	}



	@Override
	public String accept( LambdaType lambdaType ) {
		throw new RuntimeException( "Lambda type is not an expression." );
	}



	@Override
	public String accept(DrawParam drawParam) {
		throw new RuntimeException( "Draw parameter is not an expression." );
	}



	@Override
	public String accept( DataType dataType ) {
		throw new RuntimeException( "Data type is not an expression." );
	}



	@Override
	public String accept(CondExpr condExpr) {
		
		String nodeId;
		String parentId;
		
		if( exprMap.containsKey( condExpr ) )
			return exprMap.get( condExpr );
		
		nodeId = popNodeId();
		
		buf.append( nodeId ).append( " [label=\"cond\",shape=diamond];\n" );
		
		parentId = condExpr.getIfExpr().visit( this );
		buf.append( parentId ).append( " -> " ).append( nodeId ).append( " [label=\"if\"];\n" );
		
		parentId = condExpr.getThenExpr().visit( this );
		buf.append( parentId ).append( " -> " ).append( nodeId ).append( " [label=\"then\"];\n" );

		parentId = condExpr.getElseExpr().visit( this );
		buf.append( parentId ).append( " -> " ).append( nodeId ).append( " [label=\"else\"];\n" );
		
		exprMap.put( condExpr, nodeId );

		return nodeId;
	}



	@Override
	public String accept( Block block ) {
		throw new RuntimeException( "Block is not an expression." );
	}



	@Override
	public String accept(ApplyExpr applyExpr) {
		
		String taskNodeId, refNodeId;
		SingleExpr se;
		String taskName;
		
		if( exprMap.containsKey( applyExpr ) )
			return exprMap.get( applyExpr );
		
		taskNodeId = popNodeId();

		if( applyExpr.getTaskExpr().getNumSingleExpr() == 0 )
			throw new RuntimeException( "Task expression must not be nil." );

		se = applyExpr.getTaskExpr().getSingleExpr( 0 );

		if( applyExpr.getTaskExpr().getNumSingleExpr() != 1 || se instanceof CurryExpr ) {
			
			refNodeId = applyExpr.getTaskExpr().visit( this );
			
			buf.append( taskNodeId ).append( " [shape=box,label=\""+LABEL_APPLY+"\"];\n" );
			buf.append( refNodeId ).append( " -> " ).append( taskNodeId ).append( " [label=\""+LABEL_TASK+"\"];\n" );
		}
		else {
			
			if( se instanceof NativeLambdaExpr )
				taskName = LABEL_NATIVE_LAMBDA_EXPR;
			else if( se instanceof ForeignLambdaExpr )
				taskName = LABEL_FOREIGN_LAMBDA_EXPR;
			else if( se instanceof NameExpr )
				taskName = ( ( NameExpr)se ).getId();
			else
				throw new RuntimeException( "Task type not recognized." );
			
			buf.append( taskNodeId ).append( " [shape=box,label=\""+taskName+"\"];\n" );
		}
		
		for( NameExpr nameExpr : applyExpr.getNameSet() )
			
			try {
				
				refNodeId = applyExpr.getExpr( nameExpr ).visit( this );
			
				buf.append( refNodeId ).append( " -> " ).append( taskNodeId ).append( ";\n" );
			}
			catch( NotBoundException e ) {
				// cannot happen, because we only ask for what is there
				throw new RuntimeException( e.getMessage() );
			}
		
		
		exprMap.put( applyExpr, taskNodeId );
		
		return taskNodeId;
	}



	@Override
	public String accept( CompoundExpr compoundExpr ) {
		
		String nodeId;
		int n;
		
		if( exprMap.containsKey( compoundExpr ) )
			return exprMap.get( compoundExpr );
			
		
		
		n = compoundExpr.getNumSingleExpr();
		
		if( n == 0 ) {
			
			nodeId = popNodeId();

			buf.append( nodeId+" [shape=none,label=\"nil\"];\n" );
			
			exprMap.put( compoundExpr, nodeId );
			
			return nodeId;
		}
		
		if( n > 1 ) {
			
			nodeId = popNodeId();

			buf.append( nodeId ).append( " [shape=point];\n" );
			for( SingleExpr se : compoundExpr.getSingleExprList() )
				buf.append( se.visit( this ) ).append( " -> " ).append( nodeId ).append( ";\n" );
			
			exprMap.put( compoundExpr, nodeId );
			
			return nodeId;
		}
		
		
		return compoundExpr.getSingleExpr( 0 ).visit( this );
	}



	@Override
	public String accept( CorrelParam correlParam ) {
		throw new RuntimeException( "Correlated parameter is not an expression." );
	}



	@Override
	public String accept(ForeignLambdaExpr foreignLambdaExpr) {
		
		String nodeId;
		
		if( exprMap.containsKey( foreignLambdaExpr ) )
			return exprMap.get( foreignLambdaExpr );
		
		nodeId = popNodeId();
		
		buf.append( nodeId ).append( " [shape=none,label=\""+LABEL_FOREIGN_LAMBDA_EXPR+"\"];\n" );
		
		exprMap.put( foreignLambdaExpr, nodeId );
		
		return nodeId;
	}



	@Override
	public String accept(CurryExpr curryExpr) {
		
		String curryNodeId, refNodeId;
		
		if( exprMap.containsKey( curryExpr ) )
			return exprMap.get( curryExpr );
		
		curryNodeId = popNodeId();
		
		refNodeId = curryExpr.getTaskExpr().visit( this );
		
		buf.append( curryNodeId ).append( " [shape=box,label=\""+LABEL_CURRY+"\"];\n" );
		buf.append( refNodeId ).append( " -> " ).append( curryNodeId ).append( " [label=\""+LABEL_TASK+"\"];\n" );
		
		for( NameExpr nameExpr : curryExpr.getNameSet() )
			
			try {
				
				refNodeId = curryExpr.getExpr( nameExpr ).visit( this );
			
				buf.append( refNodeId ).append( " -> " ).append( curryNodeId ).append( ";\n" );
			}
			catch( NotBoundException e ) {
				throw new RuntimeException( e.getMessage() );
			}
		
		
		exprMap.put( curryExpr, curryNodeId );
		
		return curryNodeId;
	}



	@Override
	public String accept( Prototype prototype ) {
		throw new RuntimeException( "Prototype is not an expression." );
	}



	@Override
	public String accept( ReduceVar reduceVar ) {
		throw new RuntimeException( "Reduce variable is not an expression." );
	}
	
	private String popNodeId() {
		return "node"+( nextNodeId++ );
	}

	@Override
	public String accept( TopLevelContext tlc ) {
		
		for( CompoundExpr ce : tlc.getTargetList() )
			ce.visit( this );
		
		return null;
	}

}
