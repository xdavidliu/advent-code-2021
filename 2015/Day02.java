public class Day02 {
    private static int[] dimensions(String ln) {
        var mch = java.util.regex.Pattern.compile(
                "(\\d+)x(\\d+)x(\\d+)").matcher(ln);
        if (mch.matches()) {
            return java.util.stream.Stream.of(1,2,3)
                    .map(i -> Integer.decode(mch.group(i))).mapToInt(i -> i).toArray();
        } else throw new RuntimeException("invalid data");
    }
    private static int part1(int[] dim) {
        int ab = dim[0] * dim[1], bc = dim[1] * dim[2], ac = dim[0] * dim[2];
        int min = Integer.min(ab, Integer.min(bc, ac));
        return 2 * (ab + bc + ac) + min;
    }
    private static int part2(int[] dim) {
        int max = Integer.max(dim[0], Integer.max(dim[1], dim[2]));
        int prod = dim[0] * dim[1] * dim[2];
        return prod + 2 * (dim[0] + dim[1] + dim[2] - max);
    }
    public static void main(String[] args) throws java.io.IOException {
        var lines = java.nio.file.Files.lines(
                java.nio.file.Path.of("/Users/xdavidliu/Documents/temp/data02.txt"));
        var dims = lines.map(Day02::dimensions).toList();
        System.out.format("part 1 = %d, part 2 = %d\n",
                dims.stream().map(Day02::part1).mapToInt(i -> i).sum(),
                dims.stream().map(Day02::part2).mapToInt(i -> i).sum());
    }
}
