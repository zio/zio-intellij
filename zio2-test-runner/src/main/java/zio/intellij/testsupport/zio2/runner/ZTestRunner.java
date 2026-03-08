package zio.intellij.testsupport.zio2.runner;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Test runner for ZIO 2.x "All in package" runs.
 *
 * <p>Loads {@code ZIOSpecAbstract} singletons via reflection, combines them using the {@code <>}
 * operator, and calls {@code main()} on the result once. This avoids the
 * {@code System.exit}-after-first-spec problem of sequential {@code main()} calls.
 *
 * <p>Written in Java (no Scala runtime dependency) so the JAR remains lightweight and works
 * regardless of which Scala version the user's project uses — matching the pattern of
 * {@code ScalaTestRunner} and {@code Specs2Runner} in the IntelliJ Scala plugin.
 */
public class ZTestRunner {

    public static void main(String[] argsRaw) {
        int rc = 0;
        try {
            List<String> args = Arrays.asList(argsRaw);
            ParsedArgs parsed = parseArgs(args);

            if (parsed.specNames.isEmpty()) {
                System.err.println("[ZIO IntelliJ] No spec classes specified. Use -s <ClassName> to specify specs.");
                rc = 1;
            } else {
                Class<?> specAbstractClass = Class.forName("zio.test.ZIOSpecAbstract");
                List<Object> specs = new ArrayList<>();
                for (String name : parsed.specNames) {
                    specs.add(loadSpec(name));
                }
                Object combined = combineSpecs(specAbstractClass, specs);
                combined.getClass()
                        .getMethod("main", String[].class)
                        .invoke(combined, (Object) parsed.otherArgs.toArray(new String[0]));
                // ZIO's main() calls System.exit(), so execution usually does not reach here.
            }
        } catch (Throwable e) {
            e.printStackTrace();
            rc = 1;
        }
        System.exit(rc);
    }

    private static ParsedArgs parseArgs(List<String> args) {
        List<String> specNames = new ArrayList<>();
        List<String> otherArgs = new ArrayList<>();
        for (int i = 0; i < args.size(); i++) {
            if ("-s".equals(args.get(i)) && i + 1 < args.size()) {
                specNames.add(args.get(++i));
            } else {
                otherArgs.add(args.get(i));
            }
        }
        return new ParsedArgs(specNames, otherArgs);
    }

    private static Object loadSpec(String className)
            throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
        String objectClassName = className.endsWith("$") ? className : className + "$";
        return Class.forName(objectClassName).getField("MODULE$").get(null);
    }

    private static Object combineSpecs(Class<?> specAbstractClass, List<Object> specs)
            throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        Class<?> traceClass = Class.forName("zio.Trace$");
        Object traceModule = traceClass.getField("MODULE$").get(null);
        Object traceEmpty = traceModule.getClass().getMethod("empty").invoke(traceModule);
        Method combineMethod = specAbstractClass.getMethod("$less$greater", specAbstractClass, Object.class);
        Object result = specs.get(0);
        for (int i = 1; i < specs.size(); i++) {
            result = combineMethod.invoke(result, specs.get(i), traceEmpty);
        }
        return result;
    }

    private static class ParsedArgs {
        final List<String> specNames;
        final List<String> otherArgs;

        ParsedArgs(List<String> specNames, List<String> otherArgs) {
            this.specNames = specNames;
            this.otherArgs = otherArgs;
        }
    }
}
