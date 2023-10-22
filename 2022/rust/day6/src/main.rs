use std::collections::HashSet;

fn find_marker(input: &str, seq_size: usize) -> Option<usize> {
    input
        .as_bytes()
        .windows(seq_size)
        .position(|window| window.iter().collect::<HashSet<_>>().len() == seq_size)
        .map(|pos| pos + seq_size)
}


fn main() {
    dbg!(find_marker(include_str!("day6-input.txt"), 4));
    dbg!(find_marker(include_str!("day6-input.txt"), 14));
}

#[cfg(test)]
mod tests {
    use crate::find_marker;
    use test_case::test_case;

    #[test_case(7, "mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4)]
    #[test_case(5, "bvwbjplbgvbhsrlpgdmjqwftvncz", 4)]
    #[test_case(6, "nppdvjthqldpwncqszvftbrmjlhg", 4)]
    #[test_case(10, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4)]
    #[test_case(11, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4)]
    #[test_case(19, "mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14)]
    #[test_case(23, "bvwbjplbgvbhsrlpgdmjqwftvncz", 14)]
    #[test_case(23, "nppdvjthqldpwncqszvftbrmjlhg", 14)]
    #[test_case(29, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14)]
    #[test_case(26, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14)]
    fn test_find_marker(index: usize, input: &str, seq_size: usize) {
        assert_eq!(Some(index), find_marker(input, seq_size));
    }
}
