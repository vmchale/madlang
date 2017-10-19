% madlang (1)
% Vanessa McHale<vanessa.mchale@reconfigure.io>

# NAME

madlang - a text templating language for generative literature

# SYNOPSIS

  madlang run \<file\>

  madlang debug \<file\>

  madlang lint \<file\>

# DESCRIPTION

**madlang** is an interpreted language for generative literature and
computational creativity.

# OPTIONS

**-h** **--help**
:   Display help

**-r** **--rep**
:   Generate output more than once

**-i**
:   Set an input to the template

# CONFIGURATION

Place files in $HOME/.madlang for them to be available globally as libraries.
You can also download prebundled packages using

  madlang install


# EDITOR INTEGRATION

You can install a vim plugin for Madlang using

  madlang vim

Alternately, it is available from

https://github.com/vmchale/madlang-vim

# MODIFIERS

Strings in madlang can be followed by modifiers, for instance

  1.0 "evil".oulipo

Currently supported modifiers are:

  - to_lower
  - to_upper
  - capitalize
  - reverse
  - oulipo (removes all instances of the letter 'e')

# EXAMPLES

You can examine an example using the bundled libraries
at https://hub.darcs.net/vmchale/madlang-insults.
