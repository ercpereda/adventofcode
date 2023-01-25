import sys


def get_repitedItem(rucksack: str) -> str:
    middle = len(rucksack) // 2
    first_compartiment: set = set(rucksack[:middle])
    second_compartiment: set = set(rucksack[middle:])

    return (first_compartiment & second_compartiment).pop()


def get_priority(item: str) -> int:
    if item.islower():
        return ord(item) - (ord("a") - 1)
    else:
        return ord(item) - (ord("A") - 1) + 26


def compute_incorrect_item_priority(rucksack: str) -> int:
    item = get_repitedItem(rucksack)
    return get_priority(item)


def main():
    input = sys.argv[1]

    with open(input) as file:
        print(
            sum(
                compute_incorrect_item_priority(line.strip())
                for line in file.readlines()
            )
        )


if __name__ == "__main__":
    main()
