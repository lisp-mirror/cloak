package microbench.invoke;

import microbench.Benchmark;

public class NoCall extends Benchmark {
  int feld;

  public void run(int n) {
    for (int i=0; i <= n; i++) feld *= 1;
  }
}
