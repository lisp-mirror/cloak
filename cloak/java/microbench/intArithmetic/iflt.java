package microbench.intArithmetic;

import microbench.Benchmark;

public class iflt extends Benchmark {
  public void run(int n) {
    int total = 0;
    for (int i=0; i <= n; i++)
      if (i >= 0)
	total++;
      else
	total--;
  }
}
