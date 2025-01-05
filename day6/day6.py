
def read_input(filename):
    with open(filename, "r") as file:
        return file.readlines()

GUARD_DIR = ["^", ">", "v", "<"]
class Guard:
    def __init__(self, pos, dir):
        self.pos = pos
        self.dir = dir

def parse(input):
    map = [[col for col in row][:-1] for row in input] # [:-1] to remove '\n'
    for i, _ in enumerate(map):
        for j, obj in enumerate(map[i]):
            if obj in GUARD_DIR:
                guard = Guard((i, j), obj)
    return (map, guard)

def run(map, guard):
    unique_positions = [(guard.pos)]
    # Only run while guard is inside map bounds
    while guard.pos[0] in range(0, len(map)) and guard.pos[1] in range(0, len(map[0])):
        i = guard.pos[0]
        j = guard.pos[1]
        # Calculate new position
        if guard.dir == "^":
            i -= 1
        elif guard.dir == ">":
            j += 1
        elif guard.dir == "v":
            i += 1
        elif guard.dir == "<":
            j -= 1
        # Check collision
        if (i < 0 or i >= len(map)) or (j < 0 or j >= len(map[0])):
            break
        if map[i][j] == "#":
            new_dir_index = (GUARD_DIR.index(guard.dir) + 1) % 4
            guard.dir = GUARD_DIR[new_dir_index]
        else:
            guard.pos = (i, j)
            if guard.pos not in unique_positions:
                unique_positions.append(guard.pos) 
    return len(unique_positions)
                
parsed_input = parse(read_input("input.txt"))
map = parsed_input[0]
guard = parsed_input[1]
print(run(map, guard))

