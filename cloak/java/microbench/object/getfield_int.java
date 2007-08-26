package microbench.object;

import microbench.Benchmark;

public class getfield_int extends Benchmark {
  int field;
  
  public void run(int n) {
    field = 0x7fffffff;
    int total = 0;
    for (int i=0; i <= n; i++) total += field;
    field ^= total;
  }
}
