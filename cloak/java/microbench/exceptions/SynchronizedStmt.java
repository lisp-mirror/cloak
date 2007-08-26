package microbench.exceptions;

import microbench.Benchmark;

public class SynchronizedStmt extends Benchmark {
  public void run(int n) {
    for (int i=0; i <= n; i++) a();
  }

  public int a() {
    synchronized (this) {
      return drei();
    }
  }

  public int drei() {
    return 3;
  }
}
