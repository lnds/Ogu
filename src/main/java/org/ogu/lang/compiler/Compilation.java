package org.ogu.lang.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.ogu.lang.classloading.ClassFileDefinition;
import org.ogu.lang.codegen.jvm.JvmNameUtils;
import org.ogu.lang.compiler.errorhandling.ErrorCollector;
import org.ogu.lang.parser.ast.Node;
import org.ogu.lang.parser.ast.modules.ModuleNode;
import org.ogu.lang.resolvers.SymbolResolver;
import org.ogu.lang.util.Feedback;
import org.ogu.lang.util.Logger;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.objectweb.asm.Opcodes.*;


/**
 * A Compilation phase
 * Created by ediaz on 21-01-16.
 */
public class Compilation {

    private static final int JAVA_8_CLASS_VERSION = 52;

    private SymbolResolver resolver;
    private ErrorCollector errorCollector;
    private Options options;
    private ClassWriter cw;

    public Compilation(SymbolResolver resolver, ErrorCollector errorCollector, Options options) {
        this.resolver = resolver;
        this.errorCollector = errorCollector;
        this.options = options;
    }

    public List<ClassFileDefinition> compile(ModuleNode module) {
        boolean valid = module.validate(resolver, errorCollector);

        if (!valid) {
            return Collections.emptyList();
        }

        List<ClassFileDefinition> classFileDefinitions = new ArrayList<>();

        if (options.isShowTree()) {
            for (Node node : module.getChildren()) {
                Feedback.message("Node: "+node+" context: "+node.contextName());
            }
        }

        classFileDefinitions.add(compileProgram(module));
        return classFileDefinitions;
    }

    private ClassFileDefinition compileProgram(ModuleNode module) {

        String canonicalName = module.getNameDefinition().contextName();
        String internalName = JvmNameUtils.canonicalToInternal(canonicalName);

        Feedback.message("class File Definiton "+canonicalName+" ("+internalName+")");

        cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        cw.visit(JAVA_8_CLASS_VERSION, ACC_PUBLIC + ACC_SUPER, internalName, null, "java/lang/Object", null);
        MethodVisitor mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
        mv.visitCode();
        mv.visitInsn(RETURN);
        mv.visitMaxs(0, 0);
        return endClass(canonicalName);
    }

    private ClassFileDefinition endClass(String canonicalName) {
        cw.visitEnd();

        byte[] programByteCode = cw.toByteArray();
        cw = null;
        return new ClassFileDefinition(canonicalName, programByteCode);
    }
}
