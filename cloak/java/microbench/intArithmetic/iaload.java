package microbench.intArithmetic;

import microbench.Benchmark;

public class iaload extends Benchmark {
  public void run(int n) {
    int[] a = new int[1];
    a[0] = 0x7fffffff;
    int total = 0;
    for (int i=0; i <= n; i++) total += a[0];
    a[0] ^= total;
  }
}
