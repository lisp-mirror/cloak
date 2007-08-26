package microbench.object;

import microbench.Benchmark;

public class getfield_long extends Benchmark {
  long field;
  
  public void run(int n) {
    field = 0x7fffffffffffffffL;
    long total = 0;
    for (long i=0; i <= n; i++) total += field;
    field ^= total;
  }
}
