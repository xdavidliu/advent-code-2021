import java.util.PriorityQueue;
import java.util.Queue;

public class Day21 {
    static final int BOSS_HP = 100, BOSS_DAMAGE = 8, BOSS_ARMOR = 2;
    static class Item {
        int cost, damage, armor;
        Item(int c, int d, int a) {
            cost = c; damage = d; armor = a;
        }
    }
    static Item combine(Item x, Item y) {
        return new Item(x.cost + y.cost, x.damage + y.damage, x.armor + y.armor);
    }
    public static void main(String[] args) {
        Item[] weapons = {new Item(8, 4, 0), new Item(10, 5, 0), new Item(25, 6, 0), 
                          new Item(40, 7, 0), new Item(74, 8, 0)};
        Item[] armor = {new Item(13, 0, 1), new Item(31, 0, 2), new Item(53, 0, 3), 
                        new Item(75, 0, 4), new Item(102, 0, 5), new Item(0, 0, 0)};
        Item[] rings = {new Item(25, 1, 0), new Item(50, 2, 0), new Item(100, 3, 0), 
                        new Item(20, 0, 1), new Item(40, 0, 2), new Item(80, 0, 3)};
        Queue<Item> qu = new PriorityQueue<>((a, b) -> Integer.compare(a.cost, b.cost));
        Queue<Item> quHigh = new PriorityQueue<>((a, b) -> Integer.compare(b.cost, a.cost));
        for (Item w : weapons) {
            for (Item a : armor) {
                Item wa = combine(w, a);
                qu.add(wa);  // 0 rings
                for (Item r : rings) qu.add(combine(wa, r));  // 1 ring
                for (int i = 0; i < rings.length; ++i) {  // 2 rings
                    for (int k = i + 1; k < rings.length; ++k) {
                        Item r = combine(rings[i], rings[k]);
                        Item war = combine(wa, r);
                        qu.add(war);
                        quHigh.add(war);
                    }
                }
            }
        }
        Item top;
        while (null != (top = qu.poll())) {
            if (winAgainstBoss(top)) {
                System.out.println("part 1 = " + top.cost);
                break;
            }
        }
        while (null != (top = quHigh.poll())) {
            if (!winAgainstBoss(top)) {
                System.out.println("part 2 = " + top.cost);
                break;
            }
        }
    }
    static boolean winAgainstBoss(Item x) {
        int bossHp = BOSS_HP, hp = 100;
        int damageToBoss = Math.max(1, x.damage - BOSS_ARMOR);
        int damageToMe = Math.max(1, BOSS_DAMAGE - x.armor);
        while (true) {
            bossHp -= damageToBoss;
            if (bossHp <= 0) return true;
            hp -= damageToMe;
            if (hp <= 0) return false;
        }
    }
}
