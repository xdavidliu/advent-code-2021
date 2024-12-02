public class Day25 {
    static final int ROW = 3010, COL = 3019;
    static long iterate(long x) {
        return x * 252533 % 33554393;
    }
    public static void main(String[] args) {
        int r = 1, c = 1;
        long x = 20151125;
        while (r != ROW || c != COL) {
            x = iterate(x);
            if (r == 1) { r += c; c = 1; }
            else { --r; ++c; }
        }
        System.out.println("part 1 = " + x);
    }   
}
