package microbench.object;

import microbench.Benchmark;

public class putfield_long extends Benchmark {
  long field;
  
  public void run(int n) {
    long tmp;
    for (int i=0; i <= n; i++) {
      tmp = i;
      field = i + 0x7fffffffffffffffL;
    }
  }
}
