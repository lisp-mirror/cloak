package java.lang;

final class VMThrowable {
  Object vmdata;

  private VMThrowable(Object vmdata) {
    this.vmdata = vmdata;
  }

  static native VMThrowable fillInStackTrace(Throwable t);
  native StackTraceElement[] getStackTrace(Throwable t);
}
