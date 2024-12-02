import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class Day02 {
    public static void main(String[] args) throws IOException {
        final Pattern PAT = Pattern.compile("x");
        var dims = Files
                .lines(Path.of("/home/xdavidliu/Documents/temp/data02.txt"))
                .map(ln -> PAT.splitAsStream(ln)
                        .mapToInt(Integer::decode).toArray())
                .toList();
        System.out.format("part 1 = %d, part 2 = %d\n",
                dims.stream().mapToInt(Day02::part1).sum(),
                dims.stream().mapToInt(Day02::part2).sum());
    }
    private static int part1(int[] dim) {
        var areas = Stream.of(0,1,2)
                .mapToInt(i -> dim[i] * dim[(i+1)%3]).toArray();
        return Arrays.stream(areas).min().getAsInt()
                + 2 * Arrays.stream(areas).sum();
    }
    private static int part2(int[] dim) {
        return Arrays.stream(dim).reduce((x, y) -> x * y).getAsInt()
                + 2 * (Arrays.stream(dim).sum() - Arrays.stream(dim).max().getAsInt());
    }
}
