import java.nio.file.*; import java.util.*;
public class Day03 {
    static void solve() throws java.io.IOException {
        record Pt(int x, int y) {} var zero = List.of(new Pt(0, 0));
        Set<Pt> s1 = new HashSet<>(zero), s2 = new HashSet<>(zero);
        int i = 1; int[] xs = new int[3], ys = new int[3];
        for (var b : Files.readAllBytes(Path.of("./input/day03.txt"))) {
            switch (b) {
                case '^' -> { ++ys[0]; ++ys[i]; } case 'v' -> { --ys[0]; --ys[i]; }
                case '>' -> { ++xs[0]; ++xs[i]; } case '<' -> { --xs[0]; --xs[i]; }
            } s1.add(new Pt(xs[0], ys[0])); s2.add(new Pt(xs[i], ys[i])); i = 3 - i;
        } System.out.printf("03 %d %d\n", s1.size(), s2.size());
    }
}