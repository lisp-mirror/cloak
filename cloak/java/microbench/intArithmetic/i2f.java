package microbench.intArithmetic;

import microbench.Benchmark;

public class i2f extends Benchmark {
  public void run(int n) {
    float total = 0;
    for (int i=0; i <= n; i++) total += i;
  }
}
