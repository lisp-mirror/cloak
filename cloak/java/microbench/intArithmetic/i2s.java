package microbench.intArithmetic;

import microbench.Benchmark;

public class i2s extends Benchmark {
  public void run(int n) {
    short total = 0;
    for (int i=0; i <= n; i++) total += i;
  }
}
