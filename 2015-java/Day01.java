import java.nio.file.*;
public class Day01 {
    static void solve() throws java.io.IOException {
        int p2 = -1, flr = 0, pos = 1;
        for (var b : Files.readAllBytes(Path.of("./input/day01.txt"))) {
            switch(b) { case '(' -> ++flr; case ')' -> --flr; }
            if (p2 == -1) { ++pos; if (flr < 0) p2 = pos; }
        } System.out.printf("01 %d %d\n", flr, p2);
    }
}