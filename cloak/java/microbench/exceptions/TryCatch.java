package microbench.exceptions;

import microbench.Benchmark;

public class TryCatch extends Benchmark {
  public void run(int n) {
    for (int i=0; i <= n; i++) a();
  }

  public int drei() {
    return 3;
  }

  public int a() {
    int a;
    try {
      a = drei();
    } catch (Exception x) {
      a = 0;
    }
    return a;
  }
}
