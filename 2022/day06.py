def bar(s, n):
    for i in range(n, len(s)):
        if len(set(s[i-n:i])) == n:
            return i

with open('data.txt', 'r') as f:
    s = f.read()
    print(bar(s, 4))
    print(bar(s, 14))
