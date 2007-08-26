package microbench.intArithmetic;

import microbench.Benchmark;

public class irem extends Benchmark {
  public void run(int n) {
    int total = 1;
    for (int i=0; i <= n; i++) total %= 0x7fffffff;
  }
}
