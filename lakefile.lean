import Lake
open Lake DSL

package "advent_of_lean" where
  -- add package configuration options here

lean_lib «AdventOfLean» where
  -- add library configuration options here

@[default_target]
lean_exe "advent_of_lean" where
  root := `Main
