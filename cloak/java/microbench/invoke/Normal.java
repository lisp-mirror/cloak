package microbench.invoke;

import microbench.Benchmark;

public class Normal extends Benchmark {
  int feld;

  public void run(int n) {
    NormalHelper thing = new NormalHelper();
    for (int i=0; i <= n; i++) feld *= thing.a();
  }
}
