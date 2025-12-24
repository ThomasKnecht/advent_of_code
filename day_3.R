# Day 3

# https://adventofcode.com/2025/day/3

# Load Data
battery_banks <- readLines("puzzle_data//advent_of_code_3.txt")

# Part 1
joltages <- purrr::map_dbl(battery_banks, extract_joltage)

solution_part_3_1 <- sum(joltages)

# Part 2

joltages12 <- purrr::map_dbl(battery_banks, extract_joltage_12)

solution_part_3_2 <- sum(joltages12)
