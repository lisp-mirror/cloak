package microbench.exceptions;

import microbench.Benchmark;

public class Unlocked extends Benchmark {
  public void run(int n) {
    for (int i=0; i <= n; i++) a();
  }

  public int drei() {
    return 3;
  }

  public int a() {
    return drei();
  }
}
