package gnu.classpath;

public final class VMStackWalker {
  public static native Class[] getClassContext();
  public static native Class getCallingClass();
  public static native ClassLoader getCallingClassLoader();
}
