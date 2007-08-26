package microbench.intArithmetic;

import microbench.Benchmark;

public class imul extends Benchmark {
  public void run(int n) {
    int total = 0x7fffffff;
    for (int i=0; i <= n; i++) total *= 1;
  }
}
