import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;

public class Day24 {
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        List<Integer> xs = read(path);
        Collections.sort(xs, (a, b) -> Integer.compare(b, a));
        int sum = 0;
        for (int x : xs) sum += x;
        assert 0 == sum % 3;
        List<List<Integer>> found = new ArrayList<>();
        Three three = new Three(xs);
        three.recurse(0, new ArrayList<>(), sum / 3, found);
        long best = Long.MAX_VALUE;
        for (List<Integer> f : found) {
            best = Math.min(best, Split.productList(f));
        }
        System.out.println("part 1 = " + best);
    }
    static List<Integer> read(String path) {
        try (Scanner sc = new Scanner(new File(path))) {
            List<Integer> xs = new ArrayList<>();
            while (sc.hasNextInt()) xs.add(sc.nextInt());
            return xs;
        } catch (FileNotFoundException e) { return null; }
    }
}

abstract class Split {
    List<Integer> arr;
    int[] restSum;
    abstract boolean precondition(List<Integer> keep);
    abstract void add(List<Integer> keep, List<List<Integer>> found);
    abstract boolean foundOne();
    Split(List<Integer> a) {
        arr = a;
        int n = arr.size();
        restSum = new int[n];
        for (int i = n-1, sum = 0; i >= 0; --i) {
            sum += arr.get(i);
            restSum[i] = sum;
        }
    }
    void recurse(int pos, List<Integer> keep, int target, List<List<Integer>> found) {
        if (!precondition(keep)) return;
        assert target > 0;
        if (pos == arr.size()) return;  // because of the unconditional recurse at the bottom
        if (restSum[pos] == target) {
            List<Integer> keepNext = new ArrayList<>(keep);
            for (int i = pos; i < arr.size(); ++i) keepNext.add(arr.get(i));
            add(keepNext, found);
        } else if (restSum[pos] > target) {
            int aPos = arr.get(pos);
            if (aPos == target) {
                add(oneMore(keep, aPos), found);
                // no return since aPos can also be skipped
            } else if (aPos < target) {
                recurse(pos+1, oneMore(keep, aPos), target - aPos, found);
                // no return since aPos can also be skipped
            }
            recurse(pos+1, keep, target, found);
        }
    }
    static List<Integer> oneMore(List<Integer> ls, int x) {
        List<Integer> next = new ArrayList<>(ls);
        next.add(x);
        return next;
    }
    static int sumList(List<Integer> xs) {
        int sum = 0;
        for (int x : xs) sum += x;
        return sum;
    }
    static long productList(List<Integer> xs) {
        long prod = 1;
        for (int x : xs) prod *= x;
        return prod;
    }
}

class Three extends Split {
    int lowSize = -1;
    Three(List<Integer> a) {
        super(a);
    }
    @Override boolean foundOne() { return lowSize != -1; }
    @Override boolean precondition(List<Integer> keep) {
        return lowSize == -1 || keep.size() < lowSize;
        // assumes beginning of recurse always has target > 0
    }
    @Override void add(List<Integer> keep, List<List<Integer>> found) {
        if (lowSize > 0 && keep.size() > lowSize) return;
        assert lowSize == -1 || keep.size() == lowSize;
        List<Integer> diff = setDifference(arr, keep);
        int sum = Split.sumList(diff);
        assert 0 == sum % 2; 
        // assume input sum divisible by 3, and initial target is a third, so remaining
        // diff must be even.
        Split two = new Two(diff);        
        two.recurse(0, Collections.emptyList(), sum / 2, null);
        if (two.foundOne()) {
            if (lowSize == -1) lowSize = keep.size();
            found.add(keep);
        }
    }
    static List<Integer> setDifference(List<Integer> big, List<Integer> small) {
        List<Integer> diff = new ArrayList<>(big);
        diff.removeAll(small);
        return diff;
    }
}

class Two extends Split {
    boolean fOne = false;
    Two(List<Integer> a) { super(a); }
    @Override boolean foundOne() { return fOne; }
    @Override boolean precondition(List<Integer> keep) { return !fOne; }
    @Override void add(List<Integer> keep, List<List<Integer>> found) { fOne = true; }
}
