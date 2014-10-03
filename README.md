# TMC - Tiny Music Compiler [![Build Status](https://travis-ci.org/emillon/tmc.svg)](https://travis-ci.org/emillon/tmc)

(the compiler is tiny, not the music)

## What is it

Hello guys and girls. I sometimes like to make small audio projects, such as
cutting and pasting audio, or even doing simple bootlegs. The existing tools to
do this usual have one of the following problems:

  - they're proprietary
  - they're not automatable
  - they're clunky
  - they don't support nondestructive editing

Or a combination of the above.

TMC is meant to (humbly) solve that. It's a DSL (Domain-Specific Language) that
describes a set of operations of audio files. It does not manipulate audio
itself. Instead, it calls existing tools such as SoX, "the Swiss Army knife of
sound processing programs". You can call that UNIX philosophy if that pleases
you, but with systemd becoming mainstream I'm not sure what UNIX is :)

There are several advantages to this approach.

  - Everything is nondestructive. That is, you can't lose work because all you
    write is a recipe for producing audio file. Source code.
  - This source code is text. So you can diff it, check it into git, whatever.
    Ever tried to put an Audacity project in git? Yeah me neither, but it sounds
    bad.
  - You can manipulate copyrighted stuff without distributing it. For example, I
    can publish "recipes" of bootlegs, without having to distribute the original
    songs. The sources are copyrighted, the result is too, but the recipe is
    not. Distributing a TMC file is like telling your DJ friend "loop the instru
    and drop the acapella at 16 bars". It's actually what the file contains, in
    a programmatic form.
  - It's well-typed! BPMs and Durations don't have the same type (even though
    they both are Doubles), so you can't swap parameters.

## How to use it

Little documentation yet. See `Music.TMC.Example` for, well, an example.

## Status

API will break. And there's no license yet so you're not really supposed to use
it anyway :)
