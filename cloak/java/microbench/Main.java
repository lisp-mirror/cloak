package microbench;

public class Main {
  static final int LOOP_COUNT = 10000000;
  static final int SMALL_LOOP_COUNT = 100000;
  static final int LEFT = 0;
  static final int RIGHT = 1;
  int feld;
  
  public static void main(String[] args) throws Exception
  {
    new Main().main();
  }
  
  void main() throws Exception
  {
    microbench.object.Main.main();
    microbench.invoke.Main.main();
    microbench.intArithmetic.Main.main();
    microbench.longArithmetic.Main.main();
    microbench.exceptions.Main.main();
    long n = new microbench.misc.Sieve().main(2000);
    System.out.println("Sieve score estimate: " + 1000000000L / n);
  }
}
