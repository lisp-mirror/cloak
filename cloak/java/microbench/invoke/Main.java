package microbench.invoke;

public class Main {
  public static void main(String[] args) {
    main();
  }

  public static void main() {
    new NoCall().main();
    new Local().main();
    new Same().main();
    new Subclass().main();
    new Normal().main();
    new Interface().main();
  }
}
