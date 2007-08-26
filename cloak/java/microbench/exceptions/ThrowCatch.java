package microbench.exceptions;

import microbench.Benchmark;

public class ThrowCatch extends Benchmark {
  public static void main(String[] args) {
    new ThrowCatch().main();
  }

  public void run(int n) {
    for (int i=0; i <= n; i++) a();
  }

  public int a() {
    try {
      throw new Exception("foo"); 
    } catch (Exception x) {
      return 3;
    }
  }
}
