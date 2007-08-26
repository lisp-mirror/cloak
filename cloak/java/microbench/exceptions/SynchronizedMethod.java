package microbench.exceptions;

import microbench.Benchmark;

public class SynchronizedMethod extends Benchmark {
  public void run(int n) {
    for (int i=0; i <= n; i++) a();
  }

  synchronized public int a() {
    return drei();
  }

  public int drei() {
    return 3;
  }
}
