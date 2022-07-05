import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day02 {
    private static int[] dimensions(String ln) {
        Pattern pat = Pattern.compile("(\\d+)x(\\d+)x(\\d+)");
        Matcher mch = pat.matcher(ln);
        if (mch.matches() && mch.groupCount() == 3) {
            int a = Integer.decode(mch.group(1));
            int b = Integer.decode(mch.group(2));
            int c = Integer.decode(mch.group(3));
            return new int[]{a, b, c};
        } else {
            return null;
        }
    }
    private static int part1(int[] dim) {
        int ab = dim[0] * dim[1], bc = dim[1] * dim[2], ac = dim[0] * dim[2];
        int min = Integer.min(ab, Integer.min(bc, ac));
        return 2 * (ab + bc + ac) + min;
    }
    private static int part2(int[] dim) {
        int max = Integer.max(dim[0], Integer.max(dim[1], dim[2]));
        int prod = dim[0] * dim[1] * dim[2];
        return prod + 2 * (dim[0] + dim[1] + dim[2] - max);
    }
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\day02.txt";
        try {
            int p1 = 0, p2 = 0;
            BufferedReader rd = new BufferedReader(new FileReader(path));
            for (String ln = rd.readLine(); ln != null; ln = rd.readLine()) {
                int[] dim = dimensions(ln);
                if (dim == null) continue;
                p1 += part1(dim);
                p2 += part2(dim);
            }
            rd.close();
            System.out.printf("part 1 = %d\n", p1);
            System.out.printf("part 2 = %d\n", p2);
        } catch(FileNotFoundException e) {
        } catch(IOException e) {
        }
    }
}
