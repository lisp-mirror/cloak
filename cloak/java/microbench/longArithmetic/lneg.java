package microbench.longArithmetic;

import microbench.Benchmark;

public class lneg extends Benchmark {
  public void run(int n) {
    long total = 0x7fffffffffffffffL ^ n;
    for (int i=0; i <= n; i++) {
      total = -(total ^ i);
    }
  }
}
