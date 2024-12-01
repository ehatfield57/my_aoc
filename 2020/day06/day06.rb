#!/usr/bin/env ruby

require 'test/unit'

#
# Advent of Code - 2020
#
# Part One
#
# --- Day 6: Custom Customs ---
#
# As your flight approaches the regional airport where you'll switch to a much
# larger plane, customs declaration forms are distributed to the passengers.
#
# The form asks a series of 26 yes-or-no questions marked a through z. All you
# need to do is identify the questions for which anyone in your group answers
# "yes". Since your group is just you, this doesn't take very long.
#
# However, the person sitting next to you seems to be experiencing a language
# barrier and asks if you can help. For each of the people in their group, you
# write down the questions for which they answer "yes", one per line. For example:
#
# abcx
# abcy
# abcz
#
# In this group, there are 6 questions to which anyone answered "yes": a, b, c, x,
# y, and z. (Duplicate answers to the same question don't count extra; each question
# counts at most once.)
#
# Another group asks for your help, then another, and eventually you've collected
# answers from every group on the plane (your puzzle input). Each group's answers
# are separated by a blank line, and within each group, each person's answers are
# on a single line. For example:
#
# abc
#
# a
# b
# c
#
# ab
# ac
#
# a
# a
# a
# a
#
# b
#
# This list represents answers from five groups:
#
# * The first group contains one person who answered "yes" to 3 questions: a, b, and c.
# * The second group contains three people; combined, they answered "yes" to 3 questions: a, b, and c.
# * The third group contains two people; combined, they answered "yes" to 3 questions: a, b, and c.
# * The fourth group contains four people; combined, they answered "yes" to only 1 question, a.
# * The last group contains one person who answered "yes" to only 1 question, b.
#
# In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11.
#
# For each group, count the number of questions to which anyone answered "yes". What is
# the sum of those counts? (Answer: 6742)
#

def get_groups(filename)
  groups = []
  group = Hash.new(0)
  datem = ""

  lines = File.readlines(filename)
  lines.each do |line|
    datem = line.chomp

    if datem == ""
      groups << group
      group = Hash.new(0)
    else
      datem.chars.each {|c| group[c] = group[c] + 1}
    end
  end

  groups << group if group.count > 0
  groups
end

def part1(filename)
  groups = get_groups(filename)
  numbers = groups.map {|group| group.count}
  numbers.inject(:+)
end

puts "Part 1 Test: #{part1("test-input.txt")}"
puts "Part 1 Data: #{part1("input.txt")}"

#
# --- Part Two ---
#
# As you finish the last group's customs declaration, you notice that you
# misread one word in the instructions:
#
# You don't need to identify the questions to which anyone answered "yes";
# you need to identify the questions to which everyone answered "yes"!
#
# Using the same example as above:
#
# abc
#
# a
# b
# c
#
# ab
# ac
#
# a
# a
# a
# a
#
# b
#
# This list represents answers from five groups:
#
# * In the first group, everyone (all 1 person) answered "yes" to 3 questions: a, b, and c.
# * In the second group, there is no question to which everyone answered "yes".
# * In the third group, everyone answered yes to only 1 question, a. Since some people did not answer "yes" to b or c, they don't count.
# * In the fourth group, everyone answered yes to only 1 question, a.
# * In the fifth group, everyone (all 1 person) answered "yes" to 1 question, b.
#
# In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.
#
# For each group, count the number of questions to which everyone answered "yes".
# What is the sum of those counts? (Answer: 3447)
#

def get_yes(filename)
  groups = []
  group = Hash.new(0)
  people_count = 0
  datem = ""

  lines = File.readlines(filename)
  lines.each do |line|
    datem = line.chomp

    if datem == ""
      group["count"] = people_count
      groups << group
      group = Hash.new(0)
      people_count = 0
    else
      people_count += 1
      datem.chars.each {|c| group[c] = group[c] + 1}
    end
  end

  if people_count > 0
    group["count"] = people_count
    groups << group
  end

  groups
end

def part2(filename)
  total = 0
  groups = get_yes(filename) # (not (Count of char / number of people in group) < 1.0) == yes's
  groups.each do |group|
    group.each_key do |key|
      if key != "count" and (group[key] / group["count"]) == 1
        total += 1
      end
    end
  end
  total
end

puts "Part 2 Test: #{part2("test-input.txt")}"
puts "Part 2 Data: #{part2("input.txt")}"

