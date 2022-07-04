import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;

public class Day01 {
    private static int solve(String input) {
        int score = 0, pos = 0;
        boolean part2 = true;
        for (char c : input.toCharArray()) {
            switch(c) {
                case '(':
                    ++score;
                    break;
                case ')':
                    --score;
                    break;
            }
            ++pos;
            if (part2 && score == -1) {
                part2 = false;
                System.out.printf("part 2 = %d\n", pos);
            }
        }
        return score;
    }
    public static void main(String[] args) {
        try {
            String path = "c:\\users\\xdavi\\documents\\temp\\data01.txt";
            BufferedReader read = new BufferedReader(new FileReader(path));
            String ln = read.readLine();
            System.out.printf("part 1 = %d\n", solve(ln));
        } catch(FileNotFoundException e) {
        } catch(IOException e) {
        }
    }
}
