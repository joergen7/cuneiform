package de.huberlin.wbi.cuneiform.cfide.editor;

import java.util.LinkedList;
import java.util.List;

import javax.swing.table.AbstractTableModel;

import de.huberlin.wbi.cuneiform.core.preprocess.ParseException;


public class ErrorTableModel extends AbstractTableModel {

	private static final long serialVersionUID = -5752707318286728959L;
	private List<ParseException> errorList;
	
	public ErrorTableModel() {
		errorList = new LinkedList<>();
	}
	
	public void clear() {
		errorList.clear();
	}
	
	public void add( ParseException entry ) {
		
		if( entry == null )
			throw new NullPointerException( "Error entry must not be null." );
		
		errorList.add( entry );
	}
	
	@Override
	public int getColumnCount() {
		return 5;		
	}

	@Override
	public Object getValueAt( int rowIndex, int columnIndex ) {
		
		ParseException entry;
		
		entry = errorList.get( rowIndex );
		
		switch( columnIndex ) {
			case 0 : return entry.hasLine() ? entry.getLine() : "";
			case 1 : return entry.hasCharPositionInLine() ? entry.getCharPositionInLine() : "";
			case 2 : return entry.hasNear() ? entry.getNear() : "";
			case 3 : return "Error";
			case 4 : return entry.getMessage();
			default :
				throw new UnsupportedOperationException(
					"Invalid column." );
		}
	}

	@Override
	public int getRowCount() {
		return errorList.size();
	}
	
	@Override
	public String getColumnName( int columnIndex ) {
		
		switch( columnIndex ) {
			case 0 : return "Line";
			case 1 : return "Column";
			case 2 : return "Near";
			case 3 : return "Level";
			case 4 : return "Message";
			default : throw new UnsupportedOperationException( "Invalid column." );
		}
	}
}
