// Generated from C:/Sandbox/sources/grammars\Pipe.g4 by ANTLR 4.5.3
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link PipeParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface PipeVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link PipeParser#r}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitR(PipeParser.RContext ctx);
}