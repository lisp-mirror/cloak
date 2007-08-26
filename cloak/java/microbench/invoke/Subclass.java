package microbench.invoke;

import microbench.Benchmark;

public class Subclass extends Benchmark {
  int feld;

  public void run(int n) {
    Subclass thing = new SubclassHelper();
    for (int i=0; i <= n; i++) feld *= thing.a();
  }

  public int a()
  {
    return 3;
  }
}
