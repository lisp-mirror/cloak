package microbench.misc;

import microbench.Benchmark;

// Originally taken from http://waitaki.otago.ac.nz/~bryce/gcj/, which says:
// This is from the TYA distribution, an open-source JIT compiler.

public class Sieve extends Benchmark {
  static final int SIZE = 8190;

  boolean[] flags = new boolean[SIZE+1];
  
  public void run(int n) {
    int i, prime, k, iter, count = 0;
    boolean[] f = flags;

    for (int j = 0; j <= n; j++) {
      count=0;
      for(i=0; i<=SIZE; i++) f[i]=true;
      for (i=0; i<=SIZE; i++) {
	if(f[i]) {
	  prime=i+i+3;
	  for(k=i+prime; k<=SIZE; k+=prime)
	    f[k]=false;
	  count++;
	}
      }
    }

    if (count != 1899)
      throw new RuntimeException("Error: count <> 1899");
  }
}
