// Generated from de/huberlin/wbi/cuneiform/core/parser/Cuneiform.g4 by ANTLR 4.2
package de.huberlin.wbi.cuneiform.core.parser;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link CuneiformParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface CuneiformVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link CuneiformParser#paramBind}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamBind(@NotNull CuneiformParser.ParamBindContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#channel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitChannel(@NotNull CuneiformParser.ChannelContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#instat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInstat(@NotNull CuneiformParser.InstatContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#DrawComb}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDrawComb(@NotNull CuneiformParser.DrawCombContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#ForeignLambdaExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForeignLambdaExpr(@NotNull CuneiformParser.ForeignLambdaExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#output}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOutput(@NotNull CuneiformParser.OutputContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#danglingExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDanglingExpr(@NotNull CuneiformParser.DanglingExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#SingleExprErr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleExprErr1(@NotNull CuneiformParser.SingleExprErr1Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#SingleExprErr3}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleExprErr3(@NotNull CuneiformParser.SingleExprErr3Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#SingleExprErr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleExprErr2(@NotNull CuneiformParser.SingleExprErr2Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#CallExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCallExpr(@NotNull CuneiformParser.CallExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlock(@NotNull CuneiformParser.BlockContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NameInferredType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNameInferredType(@NotNull CuneiformParser.NameInferredTypeContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#DrawVar}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDrawVar(@NotNull CuneiformParser.DrawVarContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#DrawCombr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDrawCombr(@NotNull CuneiformParser.DrawCombrContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#ForeignDefTask}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForeignDefTask(@NotNull CuneiformParser.ForeignDefTaskContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#script}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitScript(@NotNull CuneiformParser.ScriptContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NameErr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNameErr2(@NotNull CuneiformParser.NameErr2Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#foreignBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForeignBody(@NotNull CuneiformParser.ForeignBodyContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#FromStackExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFromStackExpr(@NotNull CuneiformParser.FromStackExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#ParamBindErr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamBindErr1(@NotNull CuneiformParser.ParamBindErr1Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#FnPrototypeErr3}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFnPrototypeErr3(@NotNull CuneiformParser.FnPrototypeErr3Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#FnPrototypeErr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFnPrototypeErr2(@NotNull CuneiformParser.FnPrototypeErr2Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#FnPrototypeErr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFnPrototypeErr1(@NotNull CuneiformParser.FnPrototypeErr1Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NativeLambdaExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNativeLambdaExpr(@NotNull CuneiformParser.NativeLambdaExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#IntExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntExpr(@NotNull CuneiformParser.IntExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NameErr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNameErr1(@NotNull CuneiformParser.NameErr1Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#StringExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStringExpr(@NotNull CuneiformParser.StringExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#correlParam}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCorrelParam(@NotNull CuneiformParser.CorrelParamContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NameDataType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNameDataType(@NotNull CuneiformParser.NameDataTypeContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#DefTaskErr3}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDefTaskErr3(@NotNull CuneiformParser.DefTaskErr3Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#DefTaskErr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDefTaskErr1(@NotNull CuneiformParser.DefTaskErr1Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#DefTaskErr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDefTaskErr2(@NotNull CuneiformParser.DefTaskErr2Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#CondExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCondExpr(@NotNull CuneiformParser.CondExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#ApplyExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitApplyExpr(@NotNull CuneiformParser.ApplyExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#param}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParam(@NotNull CuneiformParser.ParamContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NativeDefTask}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNativeDefTask(@NotNull CuneiformParser.NativeDefTaskContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#CurryExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCurryExpr(@NotNull CuneiformParser.CurryExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#ForeignFnBodyErr1}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForeignFnBodyErr1(@NotNull CuneiformParser.ForeignFnBodyErr1Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#ForeignFnBodyErr2}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForeignFnBodyErr2(@NotNull CuneiformParser.ForeignFnBodyErr2Context ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStat(@NotNull CuneiformParser.StatContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NilExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNilExpr(@NotNull CuneiformParser.NilExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#IdExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdExpr(@NotNull CuneiformParser.IdExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#importFile}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImportFile(@NotNull CuneiformParser.ImportFileContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#reduceVar}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReduceVar(@NotNull CuneiformParser.ReduceVarContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NameDeepFnType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNameDeepFnType(@NotNull CuneiformParser.NameDeepFnTypeContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#prototype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrototype(@NotNull CuneiformParser.PrototypeContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#target}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTarget(@NotNull CuneiformParser.TargetContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#CompoundExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCompoundExpr(@NotNull CuneiformParser.CompoundExprContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#NamePlainFnType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNamePlainFnType(@NotNull CuneiformParser.NamePlainFnTypeContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#DrawPerm}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDrawPerm(@NotNull CuneiformParser.DrawPermContext ctx);

	/**
	 * Visit a parse tree produced by {@link CuneiformParser#assign}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssign(@NotNull CuneiformParser.AssignContext ctx);
}