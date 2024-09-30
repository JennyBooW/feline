import Lake
open Lake DSL

package "feline" where
  -- add package configuration options here

lean_lib «Feline» where
  -- add library configuration options here

@[default_target]
lean_exe "feline" where
  root := `Main
