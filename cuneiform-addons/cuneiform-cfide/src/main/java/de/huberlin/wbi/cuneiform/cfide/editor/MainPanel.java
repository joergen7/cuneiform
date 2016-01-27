/*******************************************************************************
 * In the Hi-WAY project we propose a novel approach of executing scientific
 * workflows processing Big Data, as found in NGS applications, on distributed
 * computational infrastructures. The Hi-WAY software stack comprises the func-
 * tional workflow language Cuneiform as well as the Hi-WAY ApplicationMaster
 * for Apache Hadoop 2.x (YARN).
 *
 * List of Contributors:
 *
 * Jörgen Brandt (HU Berlin)
 * Marc Bux (HU Berlin)
 * Ulf Leser (HU Berlin)
 *
 * Jörgen Brandt is funded by the European Commission through the BiobankCloud
 * project. Marc Bux is funded by the Deutsche Forschungsgemeinschaft through
 * research training group SOAMED (GRK 1651).
 *
 * Copyright 2014 Humboldt-Universität zu Berlin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.huberlin.wbi.cuneiform.cfide.editor;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Event;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;

import de.huberlin.wbi.cuneiform.core.repl.BaseRepl;

public class MainPanel extends JPanel implements ActionListener, WindowListener {

	private static final long serialVersionUID = -8425834761771677470L;
	
	private static final String LABEL_NEW = "New";
	private static final String LABEL_OPEN = "Open ...";
	private static final String LABEL_CLOSE = "Close";
	private static final String LABEL_SAVE = "Save";
	private static final String LABEL_SAVEAS = "Save as ...";
	private static final String LABEL_QUIT = "Quit";
	private static final String LABEL_ABOUT = "About ...";
	private static final String LABEL_REVERT = "Revert";
	
	private JTabbedPane editTabbedPane;
	private JFileChooser fc; 
	private JFrame parent;
	private JMenuItem saveItem, closeItem, saveAsItem, revertItem;
	
	public MainPanel( JFrame parentFrame ) {
		
		setParent( parentFrame );
		setLayout( new BorderLayout() );
		
		fc = new JFileChooser();
		editTabbedPane = new JTabbedPane();
		
		
		add( editTabbedPane );
	}

	@Override
	public void actionPerformed( ActionEvent arg0 ) {
		
		int returnVal;
		File f;
		String ac;
		EditPanel editPanel;

		
		ac = arg0.getActionCommand();
		
		if( ac.equals( LABEL_REVERT ) ) {

			editPanel = ( EditPanel )editTabbedPane.getSelectedComponent();

			if( !editPanel.hasChanged() )
				return;
			
			returnVal = JOptionPane.showConfirmDialog( parent, "Unsaved changes will be lost.\nAre you sure?", "Revert "+editPanel.getTitle(), JOptionPane.YES_NO_OPTION );
			
			if( returnVal == JOptionPane.NO_OPTION )
				return;
			
			editPanel.revert();
			updateSelectedEditPanelTitle();
			
			return;
		}
		
		if( ac.equals( LABEL_ABOUT ) ) {
			
			JOptionPane.showMessageDialog( parent, "Cuneiform Editor\nversion "+BaseRepl.LABEL_VERSION+" build "+BaseRepl.LABEL_BUILD+"\nJörgen Brandt", "About Cuneiform Editor", JOptionPane.INFORMATION_MESSAGE );
			
			return;
		}

		if( ac.equals( LABEL_NEW ) ) {

			editPanel = new EditPanel( this );
			editTabbedPane.add( editPanel.getTitle(), editPanel );
			editTabbedPane.setSelectedComponent( editPanel );
			updateMenu();

			return;
		}
		
		if( ac.equals( LABEL_OPEN ) ) {
			
			returnVal = fc.showOpenDialog( parent );
			
			if( returnVal == JFileChooser.APPROVE_OPTION ) {
				
				f = fc.getSelectedFile();
				open( f );
			}
			
			return;
		}
		
		if( ac.equals( LABEL_CLOSE ) ) {

			editPanel = ( EditPanel )editTabbedPane.getSelectedComponent();
			
			if( editPanel.hasChanged() ) {
				

				if( editPanel.hasFile() ) {
					
					returnVal = JOptionPane.showConfirmDialog( parent,
						    "Save changes in "+editPanel.getFile().getName()+"?",
						    "Save changes",
						    JOptionPane.YES_NO_CANCEL_OPTION );
					
					if( returnVal == JOptionPane.OK_OPTION )
						saveAction( editPanel.getFile() );
				}
				else {
					
					returnVal = JOptionPane.showConfirmDialog( parent,
						    "Save changes in unsaved file?",
						    "Save changes",
						    JOptionPane.YES_NO_CANCEL_OPTION );
					
					if( returnVal == JOptionPane.OK_OPTION ) {
	
						returnVal = saveAsAction();
					
						if( returnVal == JFileChooser.CANCEL_OPTION )
							return;
					}
					else
						if( returnVal == JOptionPane.CANCEL_OPTION )
							return;
					
	
				}
			
				if( returnVal == JOptionPane.CANCEL_OPTION )
					return;
			}
			

			editTabbedPane.remove( editTabbedPane.getSelectedComponent() );
			updateMenu();
			
			return;
		}
		
		if( ac.equals( LABEL_SAVE ) ) {

			editPanel = ( EditPanel )editTabbedPane.getSelectedComponent();
			
			if( !editPanel.hasChanged() )
				return;

			if( editPanel.hasFile() ) {
				
				saveAction( editPanel.getFile() );
				return;
			}
				
			saveAsAction();
			
			return;
		}
		
		if( ac.equals( LABEL_SAVEAS ) ) {
			
			saveAsAction();
			return;
		}
		
		if( ac.equals( LABEL_QUIT ) ) {
			quitAction();
			return;
		}
		
		throw new RuntimeException( "Action not recognized: '"+arg0.getActionCommand()+"'." );
	}
	
	public JMenuBar getMenuBar() {
		
		JMenuBar menuBar;
		JMenu menu;
		JMenuItem menuItem;
		
		menuBar = new JMenuBar();
		
		menu = new JMenu( "File" );
		menuBar.add( menu );
		
		menuItem = new JMenuItem( LABEL_NEW );
		menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_N, Event.CTRL_MASK ) );
		menuItem.addActionListener( this );
		menu.add( menuItem );
		
		menuItem = new JMenuItem( LABEL_OPEN );
		menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_O, Event.CTRL_MASK ) );
		menuItem.addActionListener( this );
		menu.add( menuItem );
		
		menu.addSeparator();

		saveItem = new JMenuItem( LABEL_SAVE );
		saveItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_S, Event.CTRL_MASK ) );
		saveItem.setEnabled( false );
		saveItem.addActionListener( this );
		menu.add( saveItem );
		
		saveAsItem = new JMenuItem( LABEL_SAVEAS );
		saveAsItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_S, Event.CTRL_MASK | Event.SHIFT_MASK ) );
		saveAsItem.setEnabled( false );
		saveAsItem.addActionListener( this );
		menu.add( saveAsItem );
		
		revertItem = new JMenuItem( LABEL_REVERT );
		revertItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_R, Event.CTRL_MASK ) );
		revertItem.setEnabled( false );
		revertItem.addActionListener( this );
		menu.add( revertItem );
		
		menu.addSeparator();

		closeItem = new JMenuItem( LABEL_CLOSE );
		closeItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_W, Event.CTRL_MASK ) );
		closeItem.setEnabled( false );
		closeItem.addActionListener( this );
		menu.add( closeItem );
		
		
		menuItem = new JMenuItem( LABEL_QUIT );
		menuItem.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_Q, Event.CTRL_MASK ) );
		menuItem.addActionListener( this );
		menu.add( menuItem );
		
		menu = new JMenu( "Help" );
		menuBar.add( menu );		
		
		menuItem = new JMenuItem( LABEL_ABOUT );
		menuItem.addActionListener( this );
		menu.add( menuItem );
		
		return menuBar;
		
	}
	
	public void setParent( JFrame parent ) {
		
		if( parent == null )
			throw new NullPointerException( "Parent frame must not be null." );
		
		this.parent = parent;
	}
	
	public void updateSelectedEditPanelTitle() {
		
		int i;
		EditPanel editPanel;
		
		i = editTabbedPane.getSelectedIndex();
		editPanel = ( EditPanel )editTabbedPane.getComponent( i );
		editTabbedPane.setTitleAt( i, editPanel.getTitle() );
	}
	
	public void updateMenu() {
		
		boolean enabled;
		
		
		enabled = editTabbedPane.getComponentCount() > 0;
		
		saveItem.setEnabled( enabled );
		saveAsItem.setEnabled( enabled );
		closeItem.setEnabled( enabled );
		revertItem.setEnabled( enabled );
		
	}
	
	
	public void open( File f ) {
		
		EditPanel editPanel;
		StringBuffer buf;
		String line;
		
		
		try( BufferedReader reader = new BufferedReader( new FileReader( f ) ) ) {
		
			buf = new StringBuffer();
			
			while( ( line = reader.readLine() ) != null )
				buf.append( line ).append( '\n' );
			
			editPanel = new EditPanel( this, f, buf.toString() );
			editTabbedPane.add( f.getName(), editPanel );
			editTabbedPane.setSelectedComponent( editPanel );
			updateMenu();
		}
		catch( IOException e ) {
			JOptionPane.showMessageDialog( parent,
				    e.getMessage(),
				    "IOException",
				    JOptionPane.ERROR_MESSAGE );
		}
	}
	
	@Override
	public void windowOpened( WindowEvent e ) {}

	@Override
	public void windowClosing( WindowEvent e ) {
		quitAction();
	}

	@Override
	public void windowClosed( WindowEvent e ) {}

	@Override
	public void windowIconified( WindowEvent e ) {}

	@Override
	public void windowDeiconified( WindowEvent e ) {}

	@Override
	public void windowActivated( WindowEvent e ) {}

	@Override
	public void windowDeactivated( WindowEvent e ) {}
	
	private int saveAsAction() {
		
		int returnVal;

		returnVal = fc.showSaveDialog( parent );
		
		if( returnVal == JFileChooser.APPROVE_OPTION ) {
			saveAction( fc.getSelectedFile() );
		}
		
		return returnVal;
	}
	
	private void saveAction( File f ) {

		EditPanel editPanel;
		
		editPanel = ( EditPanel )editTabbedPane.getSelectedComponent();
		
		try( BufferedWriter writer = new BufferedWriter( new FileWriter( f ) ) ) {
			
			writer.write( editPanel.getText() );
			editPanel.setFile( f, editPanel.getText() );
			updateSelectedEditPanelTitle();
			
		}
		catch( IOException e ) {
			JOptionPane.showMessageDialog( parent,
				    e.getMessage(),
				    "IOException",
				    JOptionPane.ERROR_MESSAGE );
		}
		
	}
	
	private void quitAction() {
		
		EditPanel editPanel;
		int option;
		int dialogOption;
		
		for( Component c : editTabbedPane.getComponents() ) {
			
			editPanel = ( EditPanel )c;
			if( editPanel.hasChanged() ) {
				
				editTabbedPane.setSelectedComponent( editPanel );
				
				if( editPanel.hasFile() ) {
				
					option = JOptionPane.showConfirmDialog( parent,
						    "Save changes in "+editPanel.getFile().getName()+"?",
						    "Save changes",
						    JOptionPane.YES_NO_CANCEL_OPTION );
					
					if( option == JOptionPane.OK_OPTION )
						saveAction( editPanel.getFile() );
				}
				else {
					
					option = JOptionPane.showConfirmDialog( parent,
						    "Save changes in unsaved file?",
						    "Save changes",
						    JOptionPane.YES_NO_CANCEL_OPTION );
					
					dialogOption = JFileChooser.APPROVE_OPTION;
					
					if( option == JOptionPane.OK_OPTION )
						dialogOption = saveAsAction();
					
					if( dialogOption == JFileChooser.CANCEL_OPTION )
						return;
					

				}
				
				if( option == JOptionPane.CANCEL_OPTION )
					return;
			}
		}
		
		System.exit( 0 );
	}


}
