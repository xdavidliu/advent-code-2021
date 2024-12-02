import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;

public class Day10 {
    public static void main(String[] args) {
        Integer[] input = {1,3,2,1,1,3,1,1,1,2};
        Deque<Integer> st = new ArrayDeque<>(Arrays.asList(input));
        Deque<Integer> next = new ArrayDeque<>();
        for (int i = 0; i < 50; ++i) {
            while (!st.isEmpty()) {
                Integer x = st.poll();
                int count = 1;
                while (!st.isEmpty() && x == st.peek()) {
                    st.poll();
                    ++count;
                }
                next.addLast(count);
                next.addLast(x);
            }
            Deque<Integer> temp = st;
            st = next;
            next = temp;
            if (i + 1 == 40) System.out.println("part 1 = " + st.size());
        }
        System.out.println("part 2 = " + st.size());
    }
}
