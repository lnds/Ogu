package org.ogu.lang.parser.ast.expressions;

import com.google.common.collect.ImmutableList;
import org.ogu.lang.parser.ast.Node;
import org.ogu.lang.resolvers.SymbolResolver;
import org.ogu.lang.symbols.FormalParameter;
import org.ogu.lang.typesystem.TypeUsage;
import org.ogu.lang.util.Logger;

import java.util.List;

/**
 * Constructor Call (new T(expr))
 * Created by ediaz on 23-01-16.
 */
public class ConstructorNode extends InvocableExpressionNode {

    protected TypeReferenceNode type;

    public ConstructorNode(TypeReferenceNode type, List<ActualParamNode> params) {
        super(params);
        this.type = type;
        this.type.setParent(this);
    }

    @Override
    public String toString() {
        return "Constructor {"+
                "type = "+type +
                "params = "+ actualParamNodes +
                '}';
    }

    @Override
    public Iterable<Node> getChildren() {
        return ImmutableList.<Node>builder().add(type).addAll(actualParamNodes).build();
    }

    @Override
    protected List<? extends FormalParameter> formalParameters(SymbolResolver resolver) {
        return null;
    }

    @Override
    public boolean isOnOverloaded(SymbolResolver resolver) {
        throw new RuntimeException("NO IMPLEMENTADO");
    }

    @Override
    public TypeUsage calcType() {
        return null;
    }
}
