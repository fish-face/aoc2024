from pprint import pprint

# p + (1, 0),
# p + (1, 1),
# p + (0, 1),
# p + (-1, 1),
# p + (-1, 0),
# p + (-1, -1),
# p + (0, -1),
# p + (1, -1),

E =  1 << 0
SE = 1 << 1
S =  1 << 2
SW = 1 << 3
W =  1 << 4
NW = 1 << 5
N =  1 << 6
NE = 1 << 7

def match_all(pattern, pos, neg = []):
	return all(pattern & d for d in pos) and all(not (pattern & d) for d in neg)

def is_convex(pattern):
	return sum([
		match_all(pattern, [], [E, S]),
		match_all(pattern, [], [S, W]),
		match_all(pattern, [], [W, N]),
		match_all(pattern, [], [N, E])
	])

def is_concave(pattern):
	return sum([
 		match_all(pattern, [E, S], [SE]),
		match_all(pattern, [S, W], [SW]),
		match_all(pattern, [W, N], [NW]),
		match_all(pattern, [N, E], [NE])
	])

def binexpand(n):
	return ''.join(str((n >> i) & 1) for i in range(8))

# pprint([binexpand(n) for n in [E, SE, S, SW, W, NW, N, NE]])
# pprint([
# 	(binexpand(n), is_convex(n), is_concave(n))
# 	for n in range(256)
# ])
for n in range(256):
	print(f"{is_convex(n) + is_concave(n)}")
