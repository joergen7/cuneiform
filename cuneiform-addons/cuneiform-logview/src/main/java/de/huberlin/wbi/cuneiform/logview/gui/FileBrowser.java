package de.huberlin.wbi.cuneiform.logview.gui;

import java.awt.BorderLayout;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

public class FileBrowser extends JPanel {

	private static final long serialVersionUID = 3161764896005980182L;

	private final JTree tree;
	private final DefaultMutableTreeNode top;
	private final DefaultTreeModel treeModel;
	private final JTextArea contentArea;
	private final Path buildPath;
	
	public FileBrowser() {
		
		JSplitPane splitPane;
		JScrollPane scrollPane;
		
		buildPath = Paths.get( System.getProperty( "user.home" ) ).resolve( ".cuneiform" );
		top = new DefaultMutableTreeNode( "[Container]" );
		treeModel = new DefaultTreeModel( top );
		tree = new JTree( treeModel );
		
		
		
		setLayout( new BorderLayout() );
		
		contentArea = new JTextArea();
		contentArea.setEditable( false );
		
		scrollPane = new JScrollPane( tree );
		scrollPane.setHorizontalScrollBarPolicy( ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER );
		
		splitPane = new JSplitPane(
			JSplitPane.HORIZONTAL_SPLIT,
			scrollPane,
			contentArea );
		splitPane.setDividerLocation( 300 );
		
		add( splitPane, BorderLayout.CENTER );
	}
	
	public void blank() {
		
		int i;
		
		for( i = top.getChildCount()-1; i >= 0; i-- )
			treeModel.removeNodeFromParent( ( DefaultMutableTreeNode )top.getChildAt( i ) );
		
		
		
		contentArea.setText( null );
	}
	
	public void setInvocId( long invocId ) {

		Path currentPath;
		
		try {
		
			currentPath = buildPath.resolve( String.valueOf( invocId ) );
			ls( currentPath, top );
			tree.expandPath( new TreePath( top.getPath() ) );
		}
		catch( IOException e ) {
			e.printStackTrace();
		}
	}
	
	private void ls( Path currentPath, DefaultMutableTreeNode currentNode ) throws IOException {
		
		DefaultMutableTreeNode node;
		
		for( Path path : Files.newDirectoryStream( currentPath ) ) {
			
			node = new DefaultMutableTreeNode( path.getFileName() );
			treeModel.insertNodeInto(
				node,
				currentNode,
				treeModel.getChildCount( currentNode ) );
			
			if( Files.isDirectory( path ) )
				ls( path, node );
		}
			
	}
}
