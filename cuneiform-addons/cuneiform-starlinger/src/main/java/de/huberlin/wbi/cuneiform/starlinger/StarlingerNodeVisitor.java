package de.huberlin.wbi.cuneiform.starlinger;

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
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
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

public class StarlingerNodeVisitor extends StarlingerWorkflow implements NodeVisitor<String> {

	private Map<CfNode,String> exprMap;
	private int nextNodeId;

	public StarlingerNodeVisitor() {
		exprMap = new HashMap<>();
		nextNodeId = 0;
	}
	
	@Override
	public String accept(StringExpr stringExpr) {
		
		String nodeId;
		StarlingerNode starlingerNode;
		
		if( exprMap.containsKey( stringExpr ) )
			return exprMap.get( stringExpr );
		
		nodeId = popNodeId();
		
		starlingerNode = StarlingerNode.createStringNode( nodeId, stringExpr.getContent() );
		
		addNode( starlingerNode );
		
		exprMap.put( stringExpr, nodeId );
		
		return nodeId;
	}

	@Override
	public String accept( QualifiedTicket qualifiedTicket ) {
		throw new RuntimeException( "invalid qualified ticket." );
	}

	@Override
	public String accept(NativeLambdaExpr nativeLambdaExpr) {
		
		String nodeId;
		StarlingerNode starlingerNode;
		
		if( exprMap.containsKey( nativeLambdaExpr ) )
			return exprMap.get( nativeLambdaExpr );
		
		nodeId = popNodeId();
		
		starlingerNode = StarlingerNode.createNativeLambdaNode( nodeId );
		addNode( starlingerNode );
		
		exprMap.put( nativeLambdaExpr, nodeId );
		
		return nodeId;
	}

	@Override
	public String accept(NameExpr nameExpr) {
		
		String nodeId;
		StarlingerNode starlingerNode;
		
		if( exprMap.containsKey( nameExpr ) )
			return exprMap.get( nameExpr );
		
		nodeId = popNodeId();
		
		starlingerNode = StarlingerNode.createNameNode( nodeId, nameExpr.getId() );
		
		addNode( starlingerNode );
		
		exprMap.put( nameExpr, nodeId );
		
		return nodeId;
	}

	@Override
	public String accept( LambdaType lambdaType ) {
		throw new RuntimeException( "Not an expression." );
	}

	@Override
	public String accept(DrawParam drawParam) {
		throw new RuntimeException( "Not an expression." );
	}

	@Override
	public String accept(DataType dataType) {
		throw new RuntimeException( "Not an expression." );
	}

	@Override
	public String accept(CondExpr condExpr) throws HasFailedException {
		
		String nodeId;
		String childId;
		StarlingerNode starlingerNode;
		
		if( exprMap.containsKey( condExpr ) )
			return exprMap.get( condExpr );
		
		nodeId = popNodeId();
		
		starlingerNode = StarlingerNode.createCondNode( nodeId );
		
		addNode( starlingerNode );
		
		childId = condExpr.getIfExpr().visit( this );
		addEdge( childId, nodeId );
		
		childId = condExpr.getThenExpr().visit( this );
		addEdge( childId, nodeId );

		childId = condExpr.getElseExpr().visit( this );
		addEdge( childId, nodeId );

		exprMap.put( condExpr, nodeId );

		return nodeId;
	}

	@Override
	public String accept(Block block) {
		throw new RuntimeException( "Not an expression." );
	}

	@Override
	public String accept(ApplyExpr applyExpr) throws HasFailedException {
		String taskNodeId, refNodeId, taskName;
		SingleExpr se;
		StarlingerNode starlingerNode;
		
		if( exprMap.containsKey( applyExpr ) )
			return exprMap.get( applyExpr );
		
		taskNodeId = popNodeId();
		
		if( applyExpr.getTaskExpr().getNumSingleExpr() == 0 )
			throw new RuntimeException( "Task expression must not be nil." );
		
		se = applyExpr.getTaskExpr().getSingleExpr( 0 );

		if( applyExpr.getTaskExpr().getNumSingleExpr() != 1 || se instanceof CurryExpr ) {
			
			refNodeId = applyExpr.getTaskExpr().visit( this );
			starlingerNode = StarlingerNode.createApplyNode( taskNodeId );
			
			addNode( starlingerNode );
			addEdge( refNodeId, taskNodeId );
		}
		else {
			
			if( se instanceof NativeLambdaExpr )
				taskName = StarlingerNode.LABEL_NATIVE_LAMBDA;
			else if( se instanceof ForeignLambdaExpr )
				taskName = StarlingerNode.LABEL_FOREIGN_LAMBDA;
			else if( se instanceof NameExpr )
				taskName = ( ( NameExpr)se ).getId();
			else
				throw new RuntimeException( "Task type not recognized." );
			
			starlingerNode = StarlingerNode.createApplyNode( taskNodeId, taskName );
			
			addNode( starlingerNode );
		}
		
		
		for( NameExpr nameExpr : applyExpr.getNameSet() )
			try {
			
				refNodeId = applyExpr.getExpr( nameExpr ).visit( this );
				
				addEdge( refNodeId, taskNodeId );
			}
			catch( NotBoundException e ) {
				// cannot happen, because we only ask for what is there
				throw new RuntimeException( e.getMessage() );
			}
		
		exprMap.put( applyExpr, taskNodeId );
		
		return taskNodeId;
	}

	@Override
	public String accept(CompoundExpr compoundExpr) throws HasFailedException {
		
		String nodeId;
		int n;
		StarlingerNode starlingerNode;
					
		if( exprMap.containsKey( compoundExpr ) )
			return exprMap.get( compoundExpr );
					
		n = compoundExpr.getNumSingleExpr();
		
		if( n == 0 ) {
			
			nodeId = popNodeId();
			
			starlingerNode = StarlingerNode.createNilNode( nodeId );
			
			addNode( starlingerNode );
			exprMap.put( compoundExpr, nodeId );
			
			return nodeId;
		}
		
		if( n > 1 ) {
			
			nodeId = popNodeId();
			
			
			starlingerNode = StarlingerNode.createAppendNode( nodeId );
			addNode( starlingerNode );
			
			
			for( SingleExpr se : compoundExpr.getSingleExprList() )
				addEdge( se.visit( this ), nodeId );
			
			exprMap.put( compoundExpr, nodeId );
			
			return nodeId;
		}
		
		
		return compoundExpr.getSingleExpr( 0 ).visit( this );
	}

	@Override
	public String accept(CorrelParam correlParam) {
		throw new RuntimeException( "Not an expression." );
	}

	@Override
	public String accept(ForeignLambdaExpr foreignLambdaExpr) {
		
		String nodeId;
		StarlingerNode starlingerNode;
		
		if( exprMap.containsKey( foreignLambdaExpr ) )
			return exprMap.get( foreignLambdaExpr );
		
		nodeId = popNodeId();
		
		starlingerNode = StarlingerNode.createForeignLambdaNode( nodeId );
		
		addNode( starlingerNode );
		
		exprMap.put( foreignLambdaExpr, nodeId );
		
		return nodeId;
	}

	@Override
	public String accept(CurryExpr curryExpr) throws HasFailedException {
		String taskNodeId, refNodeId;
		StarlingerNode starlingerNode;
		
		if( exprMap.containsKey( curryExpr ) )
			return exprMap.get( curryExpr );
		
		taskNodeId = popNodeId();
		
			
		refNodeId = curryExpr.getTaskExpr().visit( this );
		starlingerNode = StarlingerNode.createCurryNode( taskNodeId );
		
		addNode( starlingerNode );
		addEdge( refNodeId, taskNodeId );
		
		
		for( NameExpr nameExpr : curryExpr.getNameSet() )
			try {
			
				refNodeId = curryExpr.getExpr( nameExpr ).visit( this );
				
				addEdge( refNodeId, taskNodeId );
			}
			catch( NotBoundException e ) {
				// cannot happen, because we only ask for what is there
				throw new RuntimeException( e.getMessage() );
			}
		
		exprMap.put( curryExpr, taskNodeId );
		
		return taskNodeId;

	}

	@Override
	public String accept(Prototype prototype) {
		throw new RuntimeException( "Not an expression." );
	}

	@Override
	public String accept(ReduceVar reduceVar) {
		throw new RuntimeException( "Not an expression." );
	}

	@Override
	public String accept( TopLevelContext tlc ) throws HasFailedException {
		
		for( CompoundExpr ce : tlc.getTargetList() )
			ce.visit( this );
		
		return null;
	}

	private String popNodeId() {
		return "node"+( nextNodeId++ );
	}

}
