package microbench.longArithmetic;

import microbench.Benchmark;

public class lushr extends Benchmark {
  public void run(int n) {
    long total = 0x7fffffffffffffffL;
    for (int i=0; i <= n; i++) {
      total ^= i;
      total >>>= 5;
    }
  }
}
