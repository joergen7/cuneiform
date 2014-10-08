package de.huberlin.wbi.cuneiform.logview.gui;

import java.awt.BorderLayout;
import java.awt.Font;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;

public class TaskView extends JPanel implements TreeSelectionListener {

	private static final long serialVersionUID = -5409910527314838886L;
	
	private final JTextField invocIdField;
	private final JTextField taskNameField;
	private final JTextArea stdOutArea;
	private final JTextArea stdErrArea;
	private final FileBrowser fileBrowser;
	
	public TaskView() {
		
		JPanel infoBox;
		JPanel col;
		JTabbedPane tabbedPane;
		Font font;
		
		setLayout( new BorderLayout() );
		
		infoBox = new JPanel();
		infoBox.setLayout( new BorderLayout() );
		add( infoBox, BorderLayout.NORTH );
		
		col = new JPanel();
		col.setLayout( new BoxLayout( col, BoxLayout.Y_AXIS ) );
		infoBox.add( col, BorderLayout.WEST );
		
		col.add( new JLabel( "Invoc. ID:" ) );
		col.add( new JLabel( "Task name:" ) );
		
		
		col = new JPanel();
		col.setLayout( new BoxLayout( col, BoxLayout.Y_AXIS ) );
		infoBox.add( col, BorderLayout.CENTER );
		
		invocIdField = new JTextField();
		invocIdField.setEditable( false );
		taskNameField = new JTextField();
		taskNameField.setEditable( false );
		
		col.add( invocIdField );
		col.add( taskNameField );
		
		font = new Font( Font.MONOSPACED, Font.PLAIN, 11 );
		
		stdOutArea = new JTextArea();
		stdOutArea.setEditable( false );
		stdOutArea.setFont( font );
		stdErrArea = new JTextArea();
		stdErrArea.setEditable( false );
		stdErrArea.setFont( font );
		
		fileBrowser = new FileBrowser();
		
		tabbedPane = new JTabbedPane();
		tabbedPane.addTab( "Stdout", new JScrollPane( stdOutArea ) );
		tabbedPane.addTab( "Stderr", new JScrollPane( stdErrArea ) );
		tabbedPane.addTab( "Container", fileBrowser );
		add( tabbedPane, BorderLayout.CENTER );
	}

	@Override
	public void valueChanged( TreeSelectionEvent e ) {
		
		JTree tree;
		DefaultMutableTreeNode node;
		InvocationItem invocItem;
		
		tree = ( JTree )e.getSource();
		node = ( DefaultMutableTreeNode )tree.getLastSelectedPathComponent();
		
		if( node instanceof InvocationItem ) {
			
			invocItem = ( InvocationItem )node;
			invocIdField.setText( String.valueOf( invocItem.getInvocId() ) );
			taskNameField.setText( invocItem.getTaskName() );
			stdOutArea.setText( invocItem.getStdOut() );
			stdErrArea.setText( invocItem.getStdErr() );
			fileBrowser.setInvocId( invocItem.getInvocId() );
		}
		else {
			invocIdField.setText( null );
			taskNameField.setText( null );
			stdOutArea.setText( null );
			stdErrArea.setText( null );
			fileBrowser.blank();
		}
		
	}

}
