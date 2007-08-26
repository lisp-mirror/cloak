package microbench;

abstract public class Benchmark {
  static final int LEFT = 0;
  static final int RIGHT = 1;

  abstract public void run(int n);

  native long getBytesConsed();
//  long getBytesConsed() { return 0; }

  public static void main(String[] args)
    throws Exception
  {
    ((Benchmark) Class.forName(args[0]).newInstance()).main();
  }

  public long main()
  {
    return main(500);
  }

  public long main(int ms)
  {
    long startTime;
    int n = 0;

    System.out.print(pad(getClass().getName(), 40, LEFT));

    // run the benchmark for about `ms' milliseconds.
    // assume that this first run will be enough to fill caches and whatnot.
    startTime = System.currentTimeMillis();
    while ((System.currentTimeMillis() - startTime) < ms) {
      run(10000);
      n += 10000;
    }

    // run it again for the actual measurement
    long bytes = getBytesConsed();
    long time = System.currentTimeMillis();
    run(n);
    time = System.currentTimeMillis() - time;
    bytes = getBytesConsed() - bytes;

    // time per iteration
    double ns = (time * 1000000.0) / n;
    String s = Double.toString(ns);
    int dot = s.indexOf('.');
    if (dot != -1)
      s = s.substring(0, Math.min(dot + 3, s.length()));
    System.out.print(pad(s + " ns,", 12, RIGHT));

    // bytes consed per iteration
    System.out.println(pad(bytes / n + " bytes", 11, RIGHT));

    return (time * 1000000) / n;
  }

  static String pad(String s, int length, int justification)
  {
    if (s.length() < length)
      {
	char[] chars = new char[length];
	int start,end;
	if (justification == LEFT)
	  {
  	    s.getChars(0, s.length(), chars, 0);
	    start = s.length();
	    end = chars.length;
	  }
	else
	  {
	    int len = Math.min(chars.length, s.length());
    	    s.getChars(0, len, chars, chars.length - len);
	    start = 0;
	    end = chars.length - len;
          }
	for (int i=start; i<end; i++)
	  {
            chars[i] = ' ';
	  }
	return new String(chars);
      }
    else return s;
  }
}
