#!/usr/bin/env nshell
# Example nshell script. Run it with:
#   nshell examples/greet.nsh World
#
# Demonstrates functions, $argv, command substitution, seq, and for loops.

function greet
    # $argv is the function's argument list; $argv[1] is the first argument.
    echo "Hello, $argv[1]!"
end

# Fall back to a default when no name was given on the command line.
set name $argv[1]
if test -z "$name"
    set name world
end

greet $name

echo "Number of arguments:" (count $argv)

echo "Counting to five:"
for i in (seq 1 5)
    echo "  $i"
end
