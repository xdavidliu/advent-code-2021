public class Day20 {
    static final int INPUT = 36000000;
    static final int LIM = 50;
    public static void main(String[] args) {
        int[] best = {Integer.MIN_VALUE, Integer.MIN_VALUE};
        boolean found1 = false, found2 = false;
        for (int x = 1; !found1 || !found2; ++x) {
            sumFactors(x, best);
            if (!found1 && best[0] * 10 >= INPUT) {
                found1 = true;
                System.out.println("part 1 = " + x);
            }
            if (!found2 && best[1] * 11 >= INPUT) {
                found2 = true;
                System.out.println("part 2 = " + x);
            }
        }
    }
    static void sumFactors(int x, int[] best) {
        boolean even = 0 == x % 2;
        int[] sum = {0, 0};
        int stride = even ? 1 : 2;
        int i;
        for (i = 1; i * i < x; i += stride) {
            if (0 == x % i) {
                sum[0] += i;
                if (x <= LIM * i) sum[1] += i;
                int compl = x / i;
                sum[0] += compl;
                if (x <= LIM * compl) sum[1] += compl;
            }
        }
        if (i * i == x) {
            sum[0] += i;
            if (x <= LIM * i) sum[1] += i;
        }
        best[0] = Math.max(best[0], sum[0]);
        best[1] = Math.max(best[1], sum[1]);
    }
    
}
