import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Day01 {
    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of(
                "/home/xdavidliu/Documents/temp/data01.txt"));
        int score = 0;
        Integer firstNeg = null;
        for (int i = 0; i < input.length(); ++i) {
            switch(input.charAt(i)) { case '(' -> ++score; case ')' -> --score; }
            if (firstNeg == null && score == -1) firstNeg = i + 1;
        }
        System.out.format("part 1 = %d, part 2 = %d\n", score, firstNeg);
    }
}
