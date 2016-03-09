package test;

public class Example {
    
    public static void main (String[] args) {
        Example ex = new Example();
        ex.foo(10);
        //ex.twoifs(1, 100);
        ex.bar(1, 100);
        ex.lastIdx("foo");
        ex.strLoop("foobar???");
    }

    public String foo(int n) {
        String x = " a ";
        while (x.length() <n) {
            x = x + "b";
        }
        x = x.trim();
        return x;
    }

    // public int twoifs(int x, int y) {
    //     x++;
    //     if (y < 5) {
    //         y++;
    //         x++;
    //     }

    //     if (x == 0) {
    //         return y;
    //     } else {
    //         return x;
    //     }
    // }
    
    public int baz(int x, int y) {
        // while (x < y && 6 < y) {
        //     if (y % 13 == 0)
        //         break;
        //     y --;
        // }
        if (y < 0)
            return 0;
        return y+x;
    }
    
    public int bar(int x, int y){
        if (y < x)
            return 0;
        while (x < y) {
            y--; // System.out.println(y--);
        }
        return x-y;
    }

    public int lastIdx(String x) {
        if (x.length() > 1) {
            int n = x.length();
            return n - 1;
        }
        return -1;
    }

    public int strLoop(String x) {
        int a = 0;
        for (int i = 0; i < x.length(); ++i) {
            if (x.charAt(i) == '?')
                a++;
        }
        return a;
    }
}
