import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Pattern;

public class Day05 {
    static Pattern threeVowel = Pattern.compile("[aeiou].*[aeiou].*[aeiou]");
    static Pattern forbidden = Pattern.compile("ab|cd|pq|xy");
    static Pattern twoInRow = Pattern.compile("(\\w)\\1");
    static Pattern repeat = Pattern.compile("(\\w\\w).*\\1");
    static Pattern sandwich = Pattern.compile("(\\w)\\w\\1");
    static boolean nicePart1(String s) {
        return !forbidden.matcher(s).find() &&
               threeVowel.matcher(s).find() &&
               twoInRow.matcher(s).find();
    }
    static boolean nicePart2(String s) {
        return repeat.matcher(s).find() && sandwich.matcher(s).find();
    }
    public static void main(String[] args) {
        String path = "/usr/local/google/home/xdavidliu/Downloads/aoc-2015-05.txt";
        try(BufferedReader rd = new BufferedReader(new FileReader(path))) {
            String ln;
            int count1 = 0, count2 = 0;
            while (null != (ln = rd.readLine())) {
                if (nicePart1(ln)) ++count1;
                if (nicePart2(ln)) ++count2;
            }
            System.out.println("part 1 = " + count1);
            System.out.println("part 2 = " + count2);
        } catch(FileNotFoundException e) {
        } catch(IOException e) {}
    }
}
