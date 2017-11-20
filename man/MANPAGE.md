% ac (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

ac - a speedier version of cat written in ATS

# SYNOPSIS

  ac [OPTION]... [FILE]...

# DESCRIPTION

**ac** concatenates files to standard output. Optionally filter ANSI escape codes.

When no file name is given, read from standard input.

# OPTIONS

**-h**, **-\-help** Output help information and exit

**-v**, **-\-version** Output version information and exit

**-s**, **-\-strip-ansi** Strip ANSI color codes

# EXAMPLES

ac f g

```
Output f's contents, then g's contents
```

curl -s wttr.in/LA | ac -s

```
Strip colors from output of command.
```

# BUG REPORTS

For bug reports and updates, go to https://github.com/vmchale/fastcat
