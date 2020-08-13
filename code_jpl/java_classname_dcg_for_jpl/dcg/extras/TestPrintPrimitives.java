class TestPrintPrimitives {
   
   public static void main(String[] argv) {
       System.out.println(boolean.class.getName());
       System.out.println(Boolean.class.getName());
       System.out.println(byte.class.getName());
       System.out.println(Byte.class.getName());
       System.out.println(char.class.getName());
       System.out.println(Character.class.getName());
       System.out.println(double.class.getName());
       System.out.println(Double.class.getName());
       System.out.println(float.class.getName());
       System.out.println(Float.class.getName());
       System.out.println(int.class.getName());
       // System.out.println(integer.class.getName()); does not exist but *could* be a user-defined class
       System.out.println(Integer.class.getName());
       System.out.println(long.class.getName());
       System.out.println(Long.class.getName());
       System.out.println(short.class.getName());
       System.out.println(Short.class.getName());
       System.out.println(void.class.getName());
       System.out.println(Void.class.getName());
    }
}

/* 

Which outputs:

 boolean
 java.lang.Boolean
 byte
 java.lang.Byte
 char
 java.lang.Character
 double
 java.lang.Double
 float
 java.lang.Float
 int
 java.lang.Integer
 long
 java.lang.Long
 short
 java.lang.Short
 void
 java.lang.Void

*/
