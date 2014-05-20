// Generated from de/huberlin/cuneiform/libdax/parser/Dax.g4 by ANTLR 4.2
package de.huberlin.cuneiform.libdax.parser;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link DaxParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface DaxVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link DaxParser#parent}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParent(@NotNull DaxParser.ParentContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#argument}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgument(@NotNull DaxParser.ArgumentContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#adag}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAdag(@NotNull DaxParser.AdagContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#adagEl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAdagEl(@NotNull DaxParser.AdagElContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#JobPropDvname}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobPropDvname(@NotNull DaxParser.JobPropDvnameContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#FilenamePropLink}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFilenamePropLink(@NotNull DaxParser.FilenamePropLinkContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#JobPropName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobPropName(@NotNull DaxParser.JobPropNameContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#jobEl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobEl(@NotNull DaxParser.JobElContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#jobUsesProp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobUsesProp(@NotNull DaxParser.JobUsesPropContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#JobPropVersion}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobPropVersion(@NotNull DaxParser.JobPropVersionContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#JobPropId}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobPropId(@NotNull DaxParser.JobPropIdContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#filename}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFilename(@NotNull DaxParser.FilenameContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#adagProp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAdagProp(@NotNull DaxParser.AdagPropContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#ArgumentElPlain}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgumentElPlain(@NotNull DaxParser.ArgumentElPlainContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#FilenamePropFile}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFilenamePropFile(@NotNull DaxParser.FilenamePropFileContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#job}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJob(@NotNull DaxParser.JobContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#JobPropDvversion}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobPropDvversion(@NotNull DaxParser.JobPropDvversionContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#ArgumentElFilename}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgumentElFilename(@NotNull DaxParser.ArgumentElFilenameContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#JobPropLevel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitJobPropLevel(@NotNull DaxParser.JobPropLevelContext ctx);

	/**
	 * Visit a parse tree produced by {@link DaxParser#child}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitChild(@NotNull DaxParser.ChildContext ctx);
}