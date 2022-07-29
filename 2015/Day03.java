import java.io.FileNotFoundException;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Set;
import java.util.TreeSet;
import java.util.Arrays;

public class Day03 {
    private static class Point implements Comparable<Point> {
        int[] vals;
        Point(int x, int y) {
            vals = new int[] {x, y};
        }
        @Override public int compareTo(Point rhs) {
            return Arrays.compare(vals, rhs.vals);
        }
    }
    private static Set<Point> points(String dirs, int beg, int stride) {
        int x = 0, y = 0;
        Set<Point> pts = new TreeSet<>();
        for (int i = beg; i < dirs.length(); i += stride) {
            switch(dirs.charAt(i)) {
                case 'v': --y; break;
                case '^': ++y; break;
                case '<': --x; break;
                case '>': ++x; break;
                default: break;
            }
            pts.add(new Point(x, y));
        }
        return pts;
    }
    public static void main(String[] args) {
        String path = "/usr/local/google/home/xdavidliu/Downloads/aoc-2015-03.txt";
        try(BufferedReader r = new BufferedReader(new FileReader(path))) {
            String dirs = r.readLine();
            Set<Point> pts = points(dirs, 0, 1);
            System.out.println("part 1 = " + pts.size());
            Set<Point> ptsAll= points(dirs, 0, 2);
            Set<Point> ptsOdd = points(dirs, 1, 2);
            ptsAll.addAll(ptsOdd);
            System.out.println("part 2 = " + ptsAll.size());
        } catch(FileNotFoundException e) {
        } catch(IOException e) {
        }
    }
}
