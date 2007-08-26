package microbench.intArithmetic;

import microbench.Benchmark;

public class i2d extends Benchmark {
  public void run(int n) {
    double total = 0;
    for (int i=0; i <= n; i++) total += i;
  }
}
