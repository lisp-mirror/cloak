/* Main.java -- implement com.sun.tools.javac.Main
   Copyright (C) 2004  Red Hat

This file is part of java-gcj-compat.

java-gcj-compat is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

java-gcj-compat is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with java-gcj-compat; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package com.sun.tools.javac;

import java.io.*;
import java.net.*;

public class Main
{
  public static int compile (String[] args, PrintWriter p) throws Exception
  {
    /*
     * This code depends on the patch in Comment #10 in this bug
     * report:
     *
     * https://bugs.eclipse.org/bugs/show_bug.cgi?id=88364
     */
    
    org.eclipse.jdt.internal.compiler.batch.Main main
      = new org.eclipse.jdt.internal.compiler.batch.Main(
	p,
	new PrintWriter (System.err),
	false);
    return main.compile(args) ? 0 : -1;
  }

  public static int compile (String[] args) throws Exception
  {
    return compile (args, new PrintWriter (System.out));
  }

  public static void main (String[] args) throws Exception
  {
    Runtime.getRuntime ().exit (Main.compile (args));
  }
}
