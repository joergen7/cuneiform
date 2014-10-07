package de.huberlin.wbi.cuneiform.logview.gui;

import java.awt.BorderLayout;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;

public class TaskBrowser extends JPanel {

	private static final long serialVersionUID = 2782394011894606586L;
	
	private final TaskView taskView;
	private final DefaultMutableTreeNode top;
	private DefaultMutableTreeNode unnamed;
	private final Map<Long,InvocationItem> invocMap;
	private final Map<String,DefaultMutableTreeNode> taskMap;
	private final JTree tree;
	private final DefaultTreeModel treeModel;

	public TaskBrowser( TaskView taskView ) {
		
		
		if( taskView == null )
			throw new NullPointerException( "Task view must not be null." );
		
		this.taskView = taskView;
		invocMap = new HashMap<>();
		taskMap = new HashMap<>();
		
		setLayout( new BorderLayout() );
		
		top = new DefaultMutableTreeNode( "Cuneiform tasks" );
		
		treeModel = new DefaultTreeModel( top );
		tree = new JTree( treeModel );
		tree.getSelectionModel().setSelectionMode( TreeSelectionModel.SINGLE_TREE_SELECTION );
		add( new JScrollPane( tree ), BorderLayout.CENTER );
		
		tree.addTreeSelectionListener( taskView );
		
	}
	
	public void register( JsonReportEntry entry ) {
		
		long invocId;
		InvocationItem invocItem;
		String taskName;
		DefaultMutableTreeNode taskItem;
		
		if( !entry.hasInvocId() )
			return;
		
		tree.expandPath( new TreePath( top.getPath() ) );

		
		invocId = entry.getInvocId();
		
		invocItem = invocMap.get( invocId );
		if( invocItem == null ) {
			
			
			taskName = null;
			if( entry.hasTaskname() ) {
				
				taskName = entry.getTaskName();
				taskItem = taskMap.get( taskName );
				if( taskItem == null ) {
					taskItem = new DefaultMutableTreeNode( taskName );
					taskMap.put( taskName, taskItem );
					treeModel.insertNodeInto( taskItem, top, top.getChildCount() );
					top.add( taskItem );
				}
			}
			else {
				
				if( unnamed == null ) {
					unnamed = new DefaultMutableTreeNode( "[unnamed]" );
					treeModel.insertNodeInto( unnamed, top, 0 );
				}
				taskItem = unnamed;
			}
			invocItem = new InvocationItem( invocId, taskName );
			invocMap.put( invocId, invocItem );

			treeModel.insertNodeInto( invocItem, taskItem, taskItem.getChildCount() );
		}
		
		if( entry.isKeyInvocStdErr() )
			invocItem.setStdErr( entry.getValueRawString() );
		
		if( entry.isKeyInvocStdOut() )
			invocItem.setStdOut( entry.getValueRawString() );
		
	}
}
