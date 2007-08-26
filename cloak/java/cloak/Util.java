package cloak;

import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.jar.Attributes;
import java.util.jar.Attributes.Name;

/*
 * FIXME: security checks
 */
public class Util {
  public static native void _break();

  public static native void println(Object o);
  public static native void prin1(Object o);

  public static native void eval(String expr);

  public static native void format_t(String str);
  public static native void format_t(String str, Object a1);
  public static native void format_t(String str, Object a1, Object a2);
  public static native void format_t(String str, Object a1, Object a2, Object a3);
  public static native void format_t(String str, Object a1, Object a2, Object a3, Object a4);
  public static native void format_t(String str, Object a1, Object a2, Object a3, Object a4, Object a5);

  public static native String format_nil(String str);
  public static native String format_nil(String str, Object a1);
  public static native String format_nil(String str, Object a1, Object a2);
  public static native String format_nil(String str, Object a1, Object a2, Object a3);
  public static native String format_nil(String str, Object a1, Object a2, Object a3, Object a4);
  public static native String format_nil(String str, Object a1, Object a2, Object a3, Object a4, Object a5);

  public static native String note(String str);
  public static native String note(String str, Object a1);
  public static native String note(String str, Object a1, Object a2);
  public static native String note(String str, Object a1, Object a2, Object a3);
  public static native String note(String str, Object a1, Object a2, Object a3, Object a4);
  public static native String note(String str, Object a1, Object a2, Object a3, Object a4, Object a5);

  static Class getMainClass(String jarFileName) throws Exception {
    return getMainClass(jarFileName, ClassLoader.getSystemClassLoader());
  }

  static Class getMainClass(String jarFileName, ClassLoader cl)
    throws Exception
  {
    Manifest m = new JarFile(jarFileName).getManifest();
    if (m == null) 
      throw new Exception("No manifest found in: " + jarFileName);
    String name = m.getMainAttributes().getValue(Name.MAIN_CLASS);
    if (name == null)
      throw new Exception("No initial class found in: " + jarFileName);
    return Class.forName(name, true, cl);
  }

  public static void main(String[] args) {
    eval(args[0]);
  }
}
