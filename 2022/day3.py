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


def compute_rucksack_repeted_item(r1: str, r2: str, r3: str):
    s1 = set(r1)
    s2 = set(r2)
    s3 = set(r3)

    return (s1 & s2 & s3).pop()


def main():
    part = sys.argv[1]
    input = sys.argv[2]

    with open(input) as file:
        if part == "1":
            print(
                sum(
                    compute_incorrect_item_priority(line.strip())
                    for line in file.readlines()
                )
            )
            return
        elif part == "2":
            total = 0
            while True:
                r1 = file.readline().strip()
                r2 = file.readline().strip()
                r3 = file.readline().strip()
                if not (r1 and r2 and r3):
                    break
                item = compute_rucksack_repeted_item(r1, r2, r3)
                priority = get_priority(item)
                total += priority
            print(total)


if __name__ == "__main__":
    main()
