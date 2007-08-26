package microbench.object;

import microbench.Benchmark;

public class putfield_int extends Benchmark {
  int field;

  public void run(int n) {
    for (int i=0; i <= n; i++) field = i + 0x7fffffff;
  }
}
