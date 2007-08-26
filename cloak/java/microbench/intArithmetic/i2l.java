package microbench.intArithmetic;

import microbench.Benchmark;

public class i2l extends Benchmark {
  long workaround;

  public void run(int n) {
    long total = 0x7fffffffffffffffL;
    for (int i=0; i <= n; i++) total += i;
    workaround = total;
  }
}
