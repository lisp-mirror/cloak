package microbench.longArithmetic;

import microbench.Benchmark;

public class lmul extends Benchmark {
  public void run(int n) {
    long total = 0x7fffffffffffffffL;
    long eins = 1L;
    for (int i=0; i <= n; i++) total *= eins;
  }
}
