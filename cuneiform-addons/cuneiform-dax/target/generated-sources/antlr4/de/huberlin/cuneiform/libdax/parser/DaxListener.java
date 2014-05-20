// Generated from de/huberlin/cuneiform/libdax/parser/Dax.g4 by ANTLR 4.2
package de.huberlin.cuneiform.libdax.parser;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link DaxParser}.
 */
public interface DaxListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link DaxParser#parent}.
	 * @param ctx the parse tree
	 */
	void enterParent(@NotNull DaxParser.ParentContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#parent}.
	 * @param ctx the parse tree
	 */
	void exitParent(@NotNull DaxParser.ParentContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#argument}.
	 * @param ctx the parse tree
	 */
	void enterArgument(@NotNull DaxParser.ArgumentContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#argument}.
	 * @param ctx the parse tree
	 */
	void exitArgument(@NotNull DaxParser.ArgumentContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#adag}.
	 * @param ctx the parse tree
	 */
	void enterAdag(@NotNull DaxParser.AdagContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#adag}.
	 * @param ctx the parse tree
	 */
	void exitAdag(@NotNull DaxParser.AdagContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#adagEl}.
	 * @param ctx the parse tree
	 */
	void enterAdagEl(@NotNull DaxParser.AdagElContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#adagEl}.
	 * @param ctx the parse tree
	 */
	void exitAdagEl(@NotNull DaxParser.AdagElContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#JobPropDvname}.
	 * @param ctx the parse tree
	 */
	void enterJobPropDvname(@NotNull DaxParser.JobPropDvnameContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#JobPropDvname}.
	 * @param ctx the parse tree
	 */
	void exitJobPropDvname(@NotNull DaxParser.JobPropDvnameContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#FilenamePropLink}.
	 * @param ctx the parse tree
	 */
	void enterFilenamePropLink(@NotNull DaxParser.FilenamePropLinkContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#FilenamePropLink}.
	 * @param ctx the parse tree
	 */
	void exitFilenamePropLink(@NotNull DaxParser.FilenamePropLinkContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#JobPropName}.
	 * @param ctx the parse tree
	 */
	void enterJobPropName(@NotNull DaxParser.JobPropNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#JobPropName}.
	 * @param ctx the parse tree
	 */
	void exitJobPropName(@NotNull DaxParser.JobPropNameContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#jobEl}.
	 * @param ctx the parse tree
	 */
	void enterJobEl(@NotNull DaxParser.JobElContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#jobEl}.
	 * @param ctx the parse tree
	 */
	void exitJobEl(@NotNull DaxParser.JobElContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#jobUsesProp}.
	 * @param ctx the parse tree
	 */
	void enterJobUsesProp(@NotNull DaxParser.JobUsesPropContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#jobUsesProp}.
	 * @param ctx the parse tree
	 */
	void exitJobUsesProp(@NotNull DaxParser.JobUsesPropContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#JobPropVersion}.
	 * @param ctx the parse tree
	 */
	void enterJobPropVersion(@NotNull DaxParser.JobPropVersionContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#JobPropVersion}.
	 * @param ctx the parse tree
	 */
	void exitJobPropVersion(@NotNull DaxParser.JobPropVersionContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#JobPropId}.
	 * @param ctx the parse tree
	 */
	void enterJobPropId(@NotNull DaxParser.JobPropIdContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#JobPropId}.
	 * @param ctx the parse tree
	 */
	void exitJobPropId(@NotNull DaxParser.JobPropIdContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#filename}.
	 * @param ctx the parse tree
	 */
	void enterFilename(@NotNull DaxParser.FilenameContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#filename}.
	 * @param ctx the parse tree
	 */
	void exitFilename(@NotNull DaxParser.FilenameContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#adagProp}.
	 * @param ctx the parse tree
	 */
	void enterAdagProp(@NotNull DaxParser.AdagPropContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#adagProp}.
	 * @param ctx the parse tree
	 */
	void exitAdagProp(@NotNull DaxParser.AdagPropContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#ArgumentElPlain}.
	 * @param ctx the parse tree
	 */
	void enterArgumentElPlain(@NotNull DaxParser.ArgumentElPlainContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#ArgumentElPlain}.
	 * @param ctx the parse tree
	 */
	void exitArgumentElPlain(@NotNull DaxParser.ArgumentElPlainContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#FilenamePropFile}.
	 * @param ctx the parse tree
	 */
	void enterFilenamePropFile(@NotNull DaxParser.FilenamePropFileContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#FilenamePropFile}.
	 * @param ctx the parse tree
	 */
	void exitFilenamePropFile(@NotNull DaxParser.FilenamePropFileContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#job}.
	 * @param ctx the parse tree
	 */
	void enterJob(@NotNull DaxParser.JobContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#job}.
	 * @param ctx the parse tree
	 */
	void exitJob(@NotNull DaxParser.JobContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#JobPropDvversion}.
	 * @param ctx the parse tree
	 */
	void enterJobPropDvversion(@NotNull DaxParser.JobPropDvversionContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#JobPropDvversion}.
	 * @param ctx the parse tree
	 */
	void exitJobPropDvversion(@NotNull DaxParser.JobPropDvversionContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#ArgumentElFilename}.
	 * @param ctx the parse tree
	 */
	void enterArgumentElFilename(@NotNull DaxParser.ArgumentElFilenameContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#ArgumentElFilename}.
	 * @param ctx the parse tree
	 */
	void exitArgumentElFilename(@NotNull DaxParser.ArgumentElFilenameContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#JobPropLevel}.
	 * @param ctx the parse tree
	 */
	void enterJobPropLevel(@NotNull DaxParser.JobPropLevelContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#JobPropLevel}.
	 * @param ctx the parse tree
	 */
	void exitJobPropLevel(@NotNull DaxParser.JobPropLevelContext ctx);

	/**
	 * Enter a parse tree produced by {@link DaxParser#child}.
	 * @param ctx the parse tree
	 */
	void enterChild(@NotNull DaxParser.ChildContext ctx);
	/**
	 * Exit a parse tree produced by {@link DaxParser#child}.
	 * @param ctx the parse tree
	 */
	void exitChild(@NotNull DaxParser.ChildContext ctx);
}