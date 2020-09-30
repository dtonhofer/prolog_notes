class PrintSomeEntityNames {
   public static final void main(String[] argv) {

      System.out.println(void.class.getName());        // void
      System.out.println(Void.TYPE.getName());         // void
      System.out.println(Void.class.getName());        // java.lang.Void

      System.out.println(char.class.getName());        // char 
      System.out.println(Character.TYPE.getName());    // char
      System.out.println(Character.class.getName());   // java.lang.Character
      System.out.println(Character.valueOf('x').getClass().getName());  // java.lang.Character

      System.out.println(int[].class.getName());                               // [I
      System.out.println((new int[4]).getClass().getName());                   // [I
      int[] a = {1,2,3}; System.out.println(a.getClass().getName());           // [I
 
      System.out.println(int[][].class.getName());                             // [[I
      System.out.println((new int[4][4]).getClass().getName());                // [[I
      int[][] aa = {{1},{2},{3}}; System.out.println(aa.getClass().getName()); // [[I

      System.out.println(Integer[][].class.getName());                             // [[Ljava.lang.Integer;
      System.out.println((new Integer[4][4]).getClass().getName());                // [[Ljava.lang.Integer;
      Integer[][] bb = {{1},{2},{3}}; System.out.println(bb.getClass().getName()); // [[Ljava.lang.Integer;

   }
}

