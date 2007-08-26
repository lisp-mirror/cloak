package microbench.intArithmetic;

import microbench.Benchmark;

public class iushr extends Benchmark {
  public void run(int n) {
    int total = 0;
    for (int i=0; i <= n; i++) {
      total ^= i;
      total >>>= 5;
    }
  }
}
