import java.util.PriorityQueue;
import java.util.Queue;

public class Day22 {
    static final boolean PART2 = true;
    public static void main(String[] args) {
        State st = new State();
        Queue<State> qu = new PriorityQueue<>((a,b) -> Integer.compare(a.used, b.used));
        st.playerTurn(qu);
        while (!qu.isEmpty()) {
            st = qu.poll();
            st.bossTurn();
            st.playerTurn(qu);
        }
    }
}

class State implements Cloneable {
    int recharge = 0, mana = 500, used = 0, hp = 50, bossHp = 71;
    int shield = 0, poison = 0, armor = 0;
    void damageBoss(int amount) {
        bossHp -= amount;
        if (bossHp <= 0) {
            System.out.println("part 1 = " + used);
            System.exit(0);
        }
    }
    void runEffects() {
        if (poison > 0) { --poison; damageBoss(3); }
        if (recharge > 0) { --recharge; mana += 101; }
        if (shield > 0) { armor = 7; --shield; }
        else armor = 0;
    }
    void bossTurn() {
        runEffects();
        hp -= Math.max(1, 10 - armor);
    }
    @Override public Object clone() {
        try { return super.clone(); }
        catch (CloneNotSupportedException e) {
            System.err.println(e.getStackTrace());
            return new State();
        }
    }
    State usedMana(int change) {
        if (this.mana < change) return null;
        State cp = (State) clone();
        cp.mana -= change;
        cp.used += change;
        return cp;
    }
    void playerTurn(Queue<State> qu) {
        if (Day22.PART2) hp -= 1;
        if (hp <= 0) return;
        runEffects();
        State nx;
        if (null != (nx = usedMana(53))) { nx.damageBoss(4); qu.add(nx); }
        if (null != (nx = usedMana(73))) { nx.damageBoss(2); nx.hp += 2; qu.add(nx); }
        if (shield == 0 && null != (nx = usedMana(113))) { nx.shield = 6; qu.add(nx); }
        if (poison == 0 && null != (nx = usedMana(173))) { nx.poison = 6; qu.add(nx); }
        if (recharge == 0 && null != (nx = usedMana(229))) { nx.recharge = 5; qu.add(nx); }
    }
}
