package microbench.intArithmetic;

import microbench.Benchmark;

public class i2c extends Benchmark {
  public void run(int n) {
    char total = 0;
    for (int i=0; i <= n; i++) total += i;
  }
}
