package de.huberlin.wbi.cuneiform.logview.gui;

import java.nio.file.Path;

import javax.swing.tree.DefaultMutableTreeNode;

public class FileMutableTreeNode extends DefaultMutableTreeNode {

	private static final long serialVersionUID = -3967012888405802072L;

	private Path filePath;
	
	public FileMutableTreeNode( Path filePath ) {
		super( filePath.getFileName() );
		this.filePath = filePath;
	}
	
	public Path getFilePath() {
		return filePath;
	}
}
