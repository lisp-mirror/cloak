package microbench.longArithmetic;

import microbench.Benchmark;

public class lcmp1 extends Benchmark {
  public void run(int n) {
    int total = 0;
    long c = n ^ 0x7fffffffffffffffL;
    for (int i=0; i <= n; i++) {
      long j = i ^ 0x7fffffffffffffffL;
      if (j == c)
	total++;
      else
	total--;
    }
  }
}
