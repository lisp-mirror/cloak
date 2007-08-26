package microbench.longArithmetic;

import microbench.Benchmark;

public class laload extends Benchmark {
  public void run(int n) {
    long[] a = new long[1];
    a[0] = 0x7fffffffffffffffL;
    long total = 0;
    for (int i=0; i <= n; i++) total += a[0];
  }
}
