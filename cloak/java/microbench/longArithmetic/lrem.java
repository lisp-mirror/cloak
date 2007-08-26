package microbench.longArithmetic;

import microbench.Benchmark;

public class lrem extends Benchmark {
  public void run(int n) {
    long total = 0L;
    long foo = 0x7fffffffffffffffL ^ n;
    for (int i=0; i <= n; i++) total %= foo;
  }
}
