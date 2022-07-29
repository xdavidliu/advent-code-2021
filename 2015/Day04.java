import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Day04 {
    public static void main(String[] args) {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance("MD5");
        } catch(NoSuchAlgorithmException e) {
        }
        if (md == null) return;
        
        try{
            for (int i = 0; i < 10000000; ++i) {
                String s = "ckczppom" + Integer.toString(i);
                byte[] msg = s.getBytes("UTF-8");
                byte[] z = md.digest(msg);
                boolean a = z[0] == 0, b = z[1] == 0;
                // boolean c = (z[2] < 16 && z[2] >= 0);  // part 1
                boolean c = z[2] == 0;  // part 2
                if (a && b && c) {
                    System.out.println("found: " + i);
                    return;
                }
            }
        } catch(UnsupportedEncodingException e) {}
    }
}
