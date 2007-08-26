package microbench.longArithmetic;

import microbench.Benchmark;

public class ladd extends Benchmark {
  public void run(int n) {
    long total = 0L;
    for (int i=0; i <= n; i++) total += 0x7fffffffffffffffL ^ i;
  }
}
