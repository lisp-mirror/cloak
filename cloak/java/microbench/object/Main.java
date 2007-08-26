package microbench.object;

public class Main {
  public static void main(String[] args) {
    main();
  }

  public static void main() {
    new getfield_int().main();
    new putfield_int().main();
    new getfield_long().main();
    new putfield_long().main();
  }
}
