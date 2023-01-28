import sys
from typing import Tuple


def round_output(play: Tuple[str, str]) -> int:
    match play:
        case ("A", "Z") | ("B", "X") | ("C", "Y"):
            return 0
        case ("A", "X") | ("B", "Y") | ("C", "Z"):
            return 3
        case ("A", "Y") | ("B", "Z") | ("C", "X"):
            return 6


def round_score(play: Tuple[str, str]) -> int:
    shape_points = {"X": 1, "Y": 2, "Z": 3}
    return shape_points[play[1]] + round_output(play)


def main():
    input = sys.argv[1]

    with open(input) as file:
        print(sum(round_score(line.strip().split(" ")) for line in file.readlines()))


if __name__ == "__main__":
    main()
