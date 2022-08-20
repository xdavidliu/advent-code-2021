public class Day01 {
    public static void main(String[] args) throws java.io.IOException {
        String input = java.nio.file.Files.readString(java.nio.file.Path.of(
                "/Users/xdavidliu/Documents/temp/data01.txt"));
        int score = 0, firstNeg = -1;
        for (int i = 0; i < input.length(); ++i) {
            switch(input.charAt(i)) { case '(' -> ++score; case ')' -> --score; }
            if (firstNeg == -1 && score == -1) firstNeg = i + 1;
        }
        System.out.format("part 1 = %d, part 2 = %d\n", score, firstNeg);
    }
}
