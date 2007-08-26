package microbench.invoke;

import microbench.Benchmark;

public class Same extends Benchmark {
  int feld;

  public void run(int n) {
    Same thing = new Same();
    for (int i=0; i <= n; i++) feld *= thing.a();
  }

  public int a()
  {
    return 3;
  }
}
