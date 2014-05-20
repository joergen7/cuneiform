package de.huberlin.wbi.cuneiform.cfide.main;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import de.huberlin.wbi.cuneiform.cfide.editor.MainPanel;

public class Main implements Runnable {

	public static void main( String[] args )  {
		
		Main m;
		
		m = new Main();
		
		SwingUtilities.invokeLater( m );
	}

	@Override
	public void run() {
		
		JFrame frame;
		JMenuBar menuBar;
		MainPanel editRunPanel;
				
		frame = new JFrame( "Cuneiform Editor" );
		frame.setSize( 1000, 750 );
		frame.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE );
		
		editRunPanel = new MainPanel( frame );
		frame.addWindowListener( editRunPanel );
		
		menuBar = editRunPanel.getMenuBar();
		
		frame.setJMenuBar( menuBar );
		frame.add( editRunPanel );
		frame.setVisible( true );
	}
}
