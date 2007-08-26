package microbench.intArithmetic;

import microbench.Benchmark;

public class i2b extends Benchmark {
  public void run(int n) {
    byte total = 0;
    for (int i=0; i <= n; i++) total += i;
  }
}
