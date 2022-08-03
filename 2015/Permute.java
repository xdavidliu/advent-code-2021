public class Permute {
    private static void swap(int[] seq, int i, int k) {
        int temp = seq[i];
        seq[i] = seq[k];
        seq[k] = temp;
    }
    private static void reverseFrom(int[] seq, int i) {
        for (int k = seq.length - 1; i < k; ++i, --k) {
            swap(seq, i, k);
        }
    }
    public static boolean permute(int[] seq) {
        // Assumes distinct elements.
        int i = seq.length - 2;
        while (i >=0 && seq[i] > seq[i+1]) --i;
        if (i < 0) return false;
        int k = seq.length - 1;
        while (seq[i] > seq[k]) --k;
        swap(seq, i, k);
        reverseFrom(seq, i+1);
        return true;
    }    
}
