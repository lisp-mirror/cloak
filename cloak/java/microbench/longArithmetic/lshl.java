package microbench.longArithmetic;

import microbench.Benchmark;

public class lshl extends Benchmark {
  public void run(int n) {
    long total = 0;
    for (int i=0; i <= n; i++) total <<= 5;
  }
}
