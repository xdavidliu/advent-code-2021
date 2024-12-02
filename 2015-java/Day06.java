import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day06 {
    static void update(int[][] grid, int part, String ins, int x0, int x1, int y0, int y1) {
        Function<Integer, Integer> op = null;
        if (part == 1) {
            switch (ins) {
                case "toggle": op = x -> 1 - x; break;
                case "turn on": op = x -> 1; break;
                case "turn off": op = x -> 0; break;
            }
        } else if (part == 2) {
            switch (ins) {
                case "toggle": op = x -> x + 2; break;
                case "turn on": op = x -> x + 1; break;
                case "turn off": op = x -> Math.max(0, x - 1); break;
            }
        }
        if (op != null) {
            apply(op, grid, x0, x1, y0, y1);
        }
    }
    static Pattern pat = Pattern.compile("([a-z ]+) (\\d+),(\\d+) through (\\d+),(\\d+)");
    static void update(int[][] grid, int part, String ln) {
        Matcher mat;
        if ((mat = pat.matcher(ln)).matches()) {
            String ins = mat.group(1);
            int[] r = new int[4];
            for (int i = 0; i < 4; ++i) {
                r[i] = Integer.parseInt(mat.group(i + 2));
            }
            update(grid, part, ins, r[0], r[2], r[1], r[3]);
        }
    }
    static void apply(Function<Integer, Integer> op, int[][] grid, int x0, int x1, int y0, int y1) {
        for (int i = x0; i <= x1; ++i) {
            for (int j = y0; j <= y1; ++j) {
                grid[i][j] = op.apply(grid[i][j]);
            }
        }
    }
    public static void main(String[] args) { 
        String path = "C:\\Users\\xdavi\\Documents\\temp\\input.txt";
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            String ln;
            int[][] gridPart1 = new int[1000][1000];
            int[][] gridPart2 = new int[1000][1000];
            while (null != (ln = rd.readLine())) {
                update(gridPart1, 1, ln);
                update(gridPart2, 2, ln);
            }
            int countPart1 = 0, countPart2 = 0;
            for (int i = 0; i < 1000; ++i) {
                for (int j = 0; j < 1000; ++j) {
                    countPart1 += gridPart1[i][j];
                    countPart2 += gridPart2[i][j];
                }
            }
            System.out.println("part 1 = " + countPart1);
            System.out.println("part 2 = " + countPart2);
        } catch(FileNotFoundException e){
        } catch(IOException e) {
        }
    }
}
