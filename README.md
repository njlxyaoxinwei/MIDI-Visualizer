MIDI Visualizer
================
a haskell program for visualizing midi files, final project of Yale University CPSC 431 (Introduction to Computer Music I: Algorithmic and Heuristic Composition)

## Dependencies

- [PortMidi v0.1.4](https://hackage.haskell.org/package/PortMidi)
- [Euterpea v1.0.0](https://github.com/Euterpea/Euterpea)
- [UISF v0.3.0.1](https://hackage.haskell.org/package/UISF)
- [HCodecs v0.5](https://hackage.haskell.org/package/HCodecs)

## Building from source

```
% cabal install --dependencies-only
% cabal configure
% cabal build
```

The executable lies in
```
% dist/build/visualize/visualize
```

If you run into the following error
```
/usr/bin/ld: /home/dydyd/.cabal/lib/x86_64-linux-ghc-7.8.3/PortMidi-0.1.3/libHSPortMidi-0.1.3.a(ptlinux.o): undefined reference to symbol 'pthread_create@@GLIBC_2.2.5'
//lib/x86_64-linux-gnu/libpthread.so.0: error adding symbols: DSO missing from command line
collect2: error: ld returned 1 exit status
```
Please run the following additional flag
```
% cabal build --ghc-options=-optl-pthread
```

## Usage

```
% visualize PATH_TO_MID_FILE
```


## Introduction

visualize is a MIDI player with lots of helpful visualizations to show what is going on when you hear MIDI music in real time. It sends all MIDI messages to the port specified but also displays helpful information based on those messages. Specifically, it handles the following types of MIDI messages

- NoteOn/NoteOff
- ProgramChange
- TempoChange
- TimeSignature
- KeySignature
- Text
- Lyrics
- ControlChange 7,11, and 123

For each MIDI channel, it displays a graph of the current volume for each key and the instrument used. For the Percussion Channel it displays the percussion sound too.

As a player, it allows basic controls such as play/pause/resume/stop as well as advanced controls such as playback speed and skipping ahead. A helpful timer is also provided for easy navigation within the music.

## Screenshots

![Startup](Screenshots/View.png)
![Playing](Screenshots/PlayView.png)
![Channel](Screenshots/ChannelView.png)
![Percussion](Screenshots/PercussionView.png)





