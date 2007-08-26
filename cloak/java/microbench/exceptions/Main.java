package microbench.exceptions;

public class Main {
  public static void main(String[] args) {
    main();
  }

  public static void main() {
    new ThrowCatch().main();
    new Unlocked().main();
    new SynchronizedMethod().main();
    new SynchronizedStmt().main();
    new TryFinally().main();
    new TryCatch().main();
  }
}
