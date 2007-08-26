package microbench.invoke;

import microbench.Benchmark;

public class Interface extends Benchmark {
  int feld;

  public void run(int n) {
    doit(new HelperInterfaceImpl(), n);
  }

  void doit(HelperInterface thing, int n) {
    for (int i=0; i <= n; i++) feld *= thing.a();
  }
}
