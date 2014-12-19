package de.huberlin.wbi.cuneiform.core.invoc;

import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class HaskellInvocation extends Invocation {

	public HaskellInvocation( Ticket ticket, String libPath ) {
		super( ticket, libPath );
	}

	@Override
	protected String callFunction(String name, String... argValue) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String callProcedure(String name, String... argValue) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String clip(String varName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String comment(String comment) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String copyArray(String from, String to) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String defFunctionLog() throws NotDerivableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String defFunctionLogFile() throws NotDerivableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String defFunctionNormalize() throws NotDerivableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String dereference(String varName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String fileSize(String filename) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String forEach(String listName, String elementName, String body) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String getShebang() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String ifListIsNotEmpty(String listName, String body) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String ifNotFileExists(String fileName, String body) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String join(String... elementList) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String listAppend(String listName, String element) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String listToBraceCommaSeparatedString(String listName,
			String stringName, String open, String close) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String newList(String listName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String quote(String content) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String raise(String msg) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String symlink(String src, String dest) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String varDef(String varname, String value) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String varDef(String varname, CompoundExpr ce)
			throws NotDerivableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String getLibPath() {
		// TODO Auto-generated method stub
		return null;
	}

}
