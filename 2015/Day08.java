import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Pattern;

public class Day08 {
    static Pattern pat = Pattern.compile("\\\\\"|\\\\x[0-9a-f]{2}|\\\\\\\\");
    // First one has five backslashes: the first four are a literal backslash, while the
    // fifth is to construct the double-quote character, which is an ordinary character
    // in Java.
    static int encodedLength(String s) {
        s = s.replaceAll("\"", "xx");
        s = s.replaceAll("\\\\", "xx");
        return s.length() + 2;  // for the two quotation marks.
    }
    public static void main(String[] args) {
        String path = "C:\\Users\\xdavi\\Documents\\temp\\input.txt";
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            String ln;
            int countMore = 0, countWayMore = 0, countLess = 0;
            while (null != (ln = rd.readLine())) {
                countMore += ln.length();
                countLess += pat.matcher(ln).replaceAll("x").length() - 2;
                // minus 2 for the quotes in the beginning and end.
                countWayMore += encodedLength(ln);
            }
            System.out.println("part 1 = " + (countMore - countLess));
            System.out.println("part 2 = " + (countWayMore - countMore));
        } catch (IOException e) {}
    }    
}
