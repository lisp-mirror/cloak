package microbench.longArithmetic;

/*
 * note: benchmarks need to take tare to not only declare variables as `long',
 * but to actually use values that don't fit into an `int', since otherwise
 * the lisp compiler will often figure out the actual types involved and
 * use 32 bit arithmetic.
 */
public class Main {
  public static void main(String[] args) {
    main();
  }

  // lookupswitch
  public static void main() {
    new l2d().main();
    new l2f().main();
    new l2i().main();
    new ladd().main();
    new laload().main();
    new land().main();
    new lastore().main();
    new lcmp1().main();
    new lcmp2().main();
    new lcmp3().main();
    // lconst
    // ldc, ldc2_w
    new ldiv().main();
    // lload
    new lmul().main();
    new lneg().main();
    new lor().main();
    new lrem().main();
    // lreturn
    new lshl().main();
    new lshr().main();
    // lstore
    new lsub().main();
    new lushr().main();
    new lxor().main();
  }
}
