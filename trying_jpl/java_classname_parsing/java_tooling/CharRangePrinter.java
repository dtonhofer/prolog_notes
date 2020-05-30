import java.util.*;

/*
 * Print numeric ranges [LOW,HIGH] for the characters for which Java says that:
 * Character.isJavaIdentifierStart(i) is true ... if "--start" is passed to main.
 * Character.isJavaIdentifierPart(i)  is true ... if "--part"  is passed to main.
 * Those that are "part" but not "start" i.e. those that cannot be used as first chars if "--notstart"  is passed to main.
 */
 
class CharRangePrinter {

   // The "Range" expresses that a value may be
   // between [low,high]: low <= x <= high.
   // A range of length 1 is of course [limit,limit].

   private static class Range {

      final int low,high;

      public Range(int low,int high) {
         assert(low<=high);
         this.low = low;
         this.high = high;
      }

      public int length() {
         return high-low+1;
      }

      public String toString() {
         int length = high-low+1;
         return "[" + low + "," + high + "]"; 
      }
   }
 
   // simple option handling

   private final static String want_start_str    = "--start";
   private final static String want_part_str     = "--part";
   private final static String want_nostart_str  = "--nostart";

   // printing; 3 whitespaces at the start of each line
   // and a line contains max 78 characters (excluding the \n)

   private final static int indents     = 3;
   private final static int width_limit = 78;

   private static void printRanges(List<Range> ranges) {
      StringBuilder buf = new StringBuilder();
      for (int i=0;i<ranges.size();i++) {
         if (i>0) { buf.append(","); }
         String hold = buf.toString();
         Range range = ranges.get(i);
         buf.append(range);
         if (buf.length()+indents>width_limit) {
            outLine(hold);
            buf=new StringBuilder();
            buf.append(range);
         }
      }
      outLine(buf.toString());
   }

   private static void outLine(String x) {
      for (int i=0;i<indents;i++) { out(" "); }
      out(x);
      out("\n");
   }

   private static void out(String x) {
      System.out.print(x);
   }

   private static enum WantWhat {start,part,nostart};

   private static WantWhat handleArgv(String[] argv) {
      boolean good_argv = true;
      WantWhat want = null;
      if (argv.length!=1) {
         good_argv = false;
      }
      if (good_argv) {   
         String arg = argv[0];
         if (want_start_str.equals(arg)) {
            want = WantWhat.start;   
         }
         if (want_part_str.equals(arg)) {
            want = WantWhat.part;
         }
         if (want_nostart_str.equals(arg)) {
            want = WantWhat.nostart;
         }
         good_argv = (want != null);
      }
      if (!good_argv) {
         System.err.println("Pass " + want_start_str + " to print codepoints for Character.isJavaIdentifierStart()");
         System.err.println("Pass " + want_part_str  + " to print codepoints for Character.isJavaIdentifierPart()");
         System.err.println("Pass " + want_nostart_str  + " to print codepoints that can be 'part' but cannot be 'start'");
         System.exit(1);
      }      
      return want;
   }
   
   public static void main(String[] argv) {

      WantWhat want = handleArgv(argv); assert(want!=null);

      int start=0;
      int end=-1;
     
      List<Range> ranges = new LinkedList<Range>();

      for (int i=0;i<0xFFFF;i++) {
         boolean is;
         switch (want) {
         case start:
            is = Character.isJavaIdentifierStart(i);
            break;
         case part:
            is = Character.isJavaIdentifierPart(i);
            break;
         case nostart:
            is = Character.isJavaIdentifierPart(i) && !Character.isJavaIdentifierStart(i);
            break;
         default:
            throw new IllegalStateException("WTF");
         }
         if (is) {
            if (end < 0) {
               // not currently doing a run; start run
               start = i;
               end   = i;
            }  
            else {
               // extend current run 
               end = i;
            }
         }
         else {
            if (end < 0) {
               // not currently doing a run; do nothing
            }
            else {
               // close current run
               int length = end-start+1; 
               ranges.add(new Range(start,end)); 
               end=-1;
            }
         }
      }
      printRanges(ranges);
   }
}

