package microbench.intArithmetic;

import microbench.Benchmark;

public class if_icmplt extends Benchmark {
  public void run(int n) {
    int total = 0;
    int c = n / 2;
    for (int i=0; i <= n; i++)
      if (i >= c)
	total++;
      else
	total--;
  }
}
