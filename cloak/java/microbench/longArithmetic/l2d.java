package microbench.longArithmetic;

import microbench.Benchmark;

public class l2d extends Benchmark {
  public void run(int n) {
    double total = 0;
    long tmp = 0x7fffffffffffffffL;
    for (int i=0; i <= n; i++) {
      tmp ^= i;
      total += tmp;
    }
  }
}
