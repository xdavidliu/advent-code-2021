import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.stream.IntStream;

public class Day01 {
    private static int value(int cp) {
        switch(cp) {
            case '(': return 1;
            case ')': return -1;
            default: throw new RuntimeException("invalid char");
        }
    }
    public static void main(String[] args) throws IOException {
        var sums = Files.readString(Path.of(
                "/home/xdavidliu/Documents/temp/data01.txt"))
                .codePoints().map(Day01::value).toArray();
        Arrays.parallelPrefix(sums, Integer::sum);
        System.out.format("part 1 = %d, part 2 = %d\n",
                sums[sums.length - 1],
                1 + IntStream.range(0, sums.length)
                        .filter(i -> 0 > sums[i]).findFirst().getAsInt());
    }
}
