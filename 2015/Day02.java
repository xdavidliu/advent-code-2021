import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class Day02 {
    public static void main(String[] args) throws IOException {
        var dims = Files
                .lines(Path.of("/home/xdavidliu/Documents/temp/data02.txt"))
                .map(Day02::dimensions).toList();
        System.out.format("part 1 = %d, part 2 = %d\n",
                dims.stream().mapToInt(Day02::part1).sum(),
                dims.stream().mapToInt(Day02::part2).sum());
    }
    private static int[] dimensions(String ln) {
        var mch = Pattern.compile("(\\d+)x(\\d+)x(\\d+)").matcher(ln);
        if (!mch.matches()) throw new RuntimeException("invalid data: " + ln);
        return Stream.of(1,2,3).mapToInt(i -> Integer.decode(mch.group(i))).toArray();
    }
    private static int part1(int[] dim) {
        return Arrays.stream(dim).min().getAsInt() + 2 *
                Stream.of(0,1,2).mapToInt(i -> dim[i] * dim[(i+1)%3]).sum();
    }
    private static int part2(int[] dim) {
        int max = Arrays.stream(dim).max().getAsInt();
        int prod = Arrays.stream(dim).reduce((x, y) -> x * y).getAsInt();
        int sum = Arrays.stream(dim).sum();
        return prod + 2 * (sum - max);
    }
}
