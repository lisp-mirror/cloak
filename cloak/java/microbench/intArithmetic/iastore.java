package microbench.intArithmetic;

import microbench.Benchmark;

public class iastore extends Benchmark {
  public void run(int n) {
    int[] a = new int[1];
    for (int i=0; i <= n; i++) a[0] = i + 0x7fffffff;
  }
}
