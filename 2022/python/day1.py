import sys


def main():
    input = sys.argv[1]

    with open(input) as file:
        max_calories = 0
        current_calories = 0
        for line in file.readlines():
            if len(line) == 1:
                if max_calories < current_calories:
                    max_calories = current_calories
                current_calories = 0
                continue

            current_calories += int(line)

        print(max_calories)


if __name__ == "__main__":
    main()
