use color_eyre::{eyre::bail, Result};

use std::{
    collections::BTreeMap,
    io::{BufRead, BufReader},
};

fn main() -> Result<()> {
    // let stone: Stone = 123456;
    // let exp = stone.ilog10() / 2 + 1;
    // dbg!(exp);
    // let pow = 10_u64.pow(exp);
    // let left = stone / pow;
    // let right = stone % pow;

    // println!("{}, {}", left, right);
    // return Ok(());

    let mut reader = BufReader::new(std::io::stdin());
    let input = match read_input(&mut reader) {
        Ok(input) => input,
        Err(err) => bail!("invalid input: {}", err),
    };
    let result = solve(&input);
    println!("answer: {}", result);

    Ok(())
}

type Stone = u64;
type Count = usize;
type CacheKey = (usize, Stone);

fn solve(input: &[Stone]) -> usize {
    let mut cache = BTreeMap::new();
    input
        .iter()
        .map(|&stone| blink(&mut cache, 75, stone))
        .sum()
}

fn blink(cache: &mut BTreeMap<CacheKey, Count>, n: usize, stone: Stone) -> Count {
    if n == 0 {
        return 1;
    }

    if let Some(&count) = cache.get(&(n, stone)) {
        return count;
    }

    let count = if stone == 0 {
        blink(cache, n - 1, 1)
    } else {
        let log = stone.ilog10();
        if log % 2 == 1 {
            let pow = 10_u64.pow(log / 2 + 1);
            let left = stone / pow;
            let right = stone % pow;
            blink(cache, n - 1, left) + blink(cache, n - 1, right)
        } else {
            blink(cache, n - 1, stone * 2024)
        }
    };
    cache.insert((n, stone), count);
    count
}

fn read_input(reader: &mut impl BufRead) -> Result<Vec<Stone>> {
    let line = read_line(reader)?;
    let stones = line
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<Vec<Stone>, _>>()?;
    Ok(stones)
}

fn read_line(reader: &mut impl BufRead) -> std::io::Result<String> {
    let mut buf = String::new();
    reader.read_line(&mut buf)?;
    Ok(buf)
}
