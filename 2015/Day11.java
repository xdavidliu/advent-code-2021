import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day11 {
    static boolean twoDifferentPairs(String s) {
        Matcher mat1 = Pattern.compile("([a-z])\\1").matcher(s);
        if (!mat1.find()) return false;
        String regex2 = String.format("([^%s])\\1", mat1.group(1));
        return Pattern.compile(regex2).matcher(s).find();
    }
    static boolean legal(char[] arr) {
        String s = new String(arr);
        if (Pattern.compile("[iol]").matcher(s).find()) return false;
        if (!twoDifferentPairs(s)) return false;
        return increasingThree(arr);
    }
    static boolean increasingThree(char[] arr) {
        for (int i = 0; i < arr.length - 2; ++i) {
            if (1 == arr[i+1] - arr[i] && 1 == arr[i+2] - arr[i+1]) return true;
        }
        return false;
    }
    static void increment(char[] arr) {
        int i = arr.length - 1;
        while (i >= 0 && arr[i] == 'z') arr[i--] = 'a';
        if (i >= 0) ++arr[i];
    }
    public static void main(String[] args) {
        char[] arr = "hxbxwxba".toCharArray();
        do { increment(arr); } while (!legal(arr));
        System.out.println("part 1 = " + String.valueOf(arr));
        do { increment(arr); } while (!legal(arr));
        System.out.println("part 2 = " + String.valueOf(arr));
    }
    
}
