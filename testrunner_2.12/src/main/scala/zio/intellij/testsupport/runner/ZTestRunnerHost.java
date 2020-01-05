package zio.intellij.testsupport.runner;

public class ZTestRunnerHost {
    public static void main(String[] args) {
        ZTestRunner$.MODULE$.run(args);
    }
}