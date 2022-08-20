import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Day01 {
    private static int solve(String input) {
        int score = 0, pos = 0;
        boolean part2 = true;
        for (char c : input.toCharArray()) {
            switch(c) {
                case '(' -> ++score;
                case ')' -> --score;
            }
            ++pos;
            if (part2 && score == -1) {
                part2 = false;
                System.out.printf("part 2 = %d\n", pos);
            }
        }
        return score;
    }
    public static void main(String[] args) throws IOException {
        var ln = Files.readString(Path.of(
                "/Users", "xdavidliu", "Documents", "temp", "data01.txt"));
        System.out.printf("part 1 = %d\n", solve(ln));
    }
}
