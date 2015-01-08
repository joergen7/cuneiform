package de.huberlin.wbi.cuneiform.taskview;

import java.awt.BorderLayout;
import java.awt.Font;
import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public class FileBrowser extends JPanel implements TreeSelectionListener {

	private static final long serialVersionUID = 3161764896005980182L;

	private final JTree tree;
	private final DefaultMutableTreeNode top;
	private final DefaultTreeModel treeModel;
	private final JTextArea contentArea;
	private final Path buildPath;
	
	public FileBrowser( Path buildPath ) {
		
		JSplitPane splitPane;
		JScrollPane scrollPane;
		
		if( buildPath == null )
			throw new IllegalArgumentException( "Build path must not be null." );
		
		this.buildPath = buildPath;
		top = new DefaultMutableTreeNode( "[Container]" );
		treeModel = new DefaultTreeModel( top );
		tree = new JTree( treeModel );
		tree.getSelectionModel().setSelectionMode( TreeSelectionModel.SINGLE_TREE_SELECTION );
		tree.addTreeSelectionListener( this );
		
		
		setLayout( new BorderLayout() );
		
		contentArea = new JTextArea();
		contentArea.setEditable( false );
		contentArea.setFont( new Font( Font.MONOSPACED, Font.PLAIN, 11 ) );
		
		scrollPane = new JScrollPane( tree );
		scrollPane.setHorizontalScrollBarPolicy( ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER );
		
		splitPane = new JSplitPane(
			JSplitPane.HORIZONTAL_SPLIT,
			scrollPane,
			new JScrollPane ( contentArea ) );
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
		
			blank();
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
			
			node = new FileMutableTreeNode( path );
			treeModel.insertNodeInto(
				node,
				currentNode,
				treeModel.getChildCount( currentNode ) );
			
			if( Files.isDirectory( path ) )
				ls( path, node );
		}
			
	}

	@Override
	public void valueChanged( TreeSelectionEvent e ) {
		
		JTree t;
		DefaultMutableTreeNode node;
		int i;
		StringBuffer buf;
		String line;
		
		t = ( JTree )e.getSource();
		node = ( DefaultMutableTreeNode )t.getLastSelectedPathComponent();
		
		if( node.isLeaf() )
			
			if( node != top ) {
				
			
				try( BufferedReader reader =
					Files.newBufferedReader( ( ( FileMutableTreeNode )node ).getFilePath(), Charset.forName( "UTF-8" ) ) ) {
					
					buf = new StringBuffer();
					for( i = 0; i < 1024; i++ ) {
						
						line = reader.readLine();
						if( line == null )
							break;
						
						buf.append( line ).append( '\n' );
					}
					contentArea.setText( buf.toString() );
				
					return;
				}
				catch( IOException x ) {
					contentArea.setText( "[binary]" );
					return;
				}
			}
		
		contentArea.setText( null );
		
	}
}
