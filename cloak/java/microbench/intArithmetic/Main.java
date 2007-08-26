package microbench.intArithmetic;

public class Main {
  public static void main(String[] args) {
    main();
  }

  // ifnonnull
  // ifnull
  // instanceof
  // invoke*
  // if_acmp
  public static void main() {
    new i2b().main();
    new i2c().main();
    new i2d().main();
    new i2f().main();
    new i2l().main();
    new i2s().main();
    new iadd().main();
    new iaload().main();
    new iand().main();
    new iastore().main();
    // iconst
    new idiv().main();
    new if_icmpeq().main();
    new if_icmpne().main();
    new if_icmplt().main();
    new if_icmple().main();
    new if_icmpgt().main();
    new if_icmpge().main();
    new ifeq().main();
    new ifne().main();
    new iflt().main();
    new ifle().main();
    new ifgt().main();
    new ifge().main();
    new iinc().main();
    // iload
    new imul().main();
    new ineg().main();
    new ior().main();
    new irem().main();
    // ireturn
    new ishl().main();
    new ishr().main();
    // istore
    new isub().main();
    new iushr().main();
    new ixor().main();
  }
}
