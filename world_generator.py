#!/usr/bin/env python
import subprocess
import itertools

from argparse import ArgumentParser

WORLD = (4, 4)

def to_cartesian(pos):
    "Returns the 2D cartesian position from an index"
    return (pos % WORLD[0] + 1, pos / WORLD[0] + 1)

TILES = [to_cartesian(i) for i in range(16) if i > 0] # All avaiable tiles (hence except 1x1).

if __name__ == "__main__":
    # Parse CLI option
    parser = ArgumentParser()
    parser.add_argument('-f', '--agent-file')
    parser.add_argument('-o', '--output')
    args = parser.parse_args()
    # ['[2,1]', '[3,1]', '[4,1]', '[1,2]', '[2,2]']
    # Generate all world combinations with 1 Gold, 1 Wumpus and 3 Pits
    print(itertools.permutations(TILES, 5))
    test = list(itertools.permutations(TILES, 5))
    for combination in itertools.permutations(TILES, 5):
        world = ["[%d,%d]" % i for i in combination]
        print(combination)
        # Execute this world
        # cmd = 'swipl -f {} -g src/world.pl -s src/main.pl -g'\
        #     .format(args.agent_file).split()
        # cmd.append("run(%s)."% ','.join(world))

        # p = subprocess.Popen(cmd,   stdout=subprocess.PIPE,
        #                             stderr=subprocess.PIPE)
        # out, err = p.communicate()
        # print(out)
        # print(err)
        break
