package microbench.invoke;

import microbench.Benchmark;

public class Private extends Benchmark {
  int feld;

  public void run(int n) {
    for (int i=0; i <= n; i++) feld *= a();
  }

  private int a()
  {
    return 3;
  }
}
