package microbench.longArithmetic;

import microbench.Benchmark;

public class lastore extends Benchmark {
  public void run(int n) {
    long[] a = new long[1];
    long tmp;
    for (int i=0; i <= n; i++) {
      tmp = i;
      a[0] = i + 0x7fffffffffffffffL;
    }
  }
}
