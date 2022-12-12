import queue
# TODO: replace this with collections.deque since you don't need threading stuff
# and queue.Queue.get blocks. I tried using queue.Queue on day12 and it hanged on
# the second BFS

prime_prod = 1

class Monkey:

    def _init_queue(self, line):
        self._q = queue.Queue()
        for x in line[16:].split(', '):
            self._q.put(int(x))

    def _init_op(self, line):
        op = line[21:]  # it's stripped!
        assert op[0] in '+*'
        if op == '+ old':
            self._op = lambda y : y + y
        elif op == '* old':
            self._op = lambda y : y * y
        else:
            op_val = int(op[2:])
            if op[0] == '+':
                self._op = lambda y : y + op_val
            else:
                self._op = lambda y : y * op_val

    def _init_test(self, line):
        self._test_val = int(line[19:])  # stripped

    def work(self, monkeys):
        while self._q.qsize() != 0:
            self._count += 1
            item = self._q.get()
            item = self._op(item)# % self._test_val
            if self._part == 2:
                item %= prime_prod
            if self._part == 1:
                item //= 3
            other = self._throw[0 == item % self._test_val]
            monkeys[other]._q.put(item)

    def __init__(self, lines, part):
        self._count = 0
        self._part = part
        self._init_queue(lines[1])
        self._init_test(lines[3])
        self._init_op(lines[2])
        self._throw = dict()
        self._throw[True] = int(lines[4][25:])
        self._throw[False] = int(lines[5][26:])


def prod_best_two(monkeys):
    best_two = sorted([m._count for m in monkeys], reverse=True)[:2]
    return best_two[0] * best_two[1]


with open('data.txt', 'r') as file:
    paragraphs = [p.strip() for p in file.read().split('\n\n')]
    monkeys1 = []
    monkeys2 = []
    for p in paragraphs:
        lines = [l.strip() for l in p.split('\n')]
        monkeys1.append(Monkey(lines, 1))
        monkeys2.append(Monkey(lines, 2))
    for i in range(20):
        for m in monkeys1:
            m.work(monkeys1)
    print(prod_best_two(monkeys1))
    # part 2
    # assumes all the _test_val are primes
    # not sure why this works, but it's something to do with
    # chinese remainder theorem
    for m in monkeys2:
        prime_prod *= m._test_val
    for i in range(10000):
        for m in monkeys2:
            m.work(monkeys2)
    print(prod_best_two(monkeys2))
