## media cutting plugin for [MPV](https://github.com/mpv-player/mpv)

Allows to cut pieces of media file specified with start/end timestamps via
keybindings.

CURRENTLY BROKEN BUILD (see [Issues](#issues)).

--------------------------------------------------------------------------------

### Usage example

1. start media _file.ext_ playback in MPV
2. set start/end timestamps by pressing 'A'/'B' keys
3. press 'C' key
4. check _file_.00\._ext_, _file_.01\._ext_ ... for the cutted parts

### Brief explanation of how it works
MPV captures **keystrokes**, calls corresponding functions from Lua script.
They in turn call Haskell functions through a C binding library. Haskell code
creates a **bash script** and accumulates there **pieces** composed of input
**timestamps**. Running the script produces all accumulated pieces of media
file using FFmpeg in **stream copy** mode.

### Installation
#### Software you need to have installed in order to use this plugin
* [MPV](https://mpv.io/installation/)
* [Lua](http://www.lua.org/download.html)
* [FFmpeg](https://ffmpeg.org/download.html)
* Bash
* [bc](http://www.gnu.org/software/bc)

Lua version must be compatible with MPV version used.

#### Build
$ git clone https://github.com/AleXoundOS/mpv-cut.git

$ cd mpv-cut

$ [stack](https://haskellstack.org) build

#### Install MPV plugin
copy cut.lua and libmpv-cut.so into ~/.mpv/scripts


### Key bindings
All commands relate to the current playback time position, which forms a
timestamp passed to them.

key binding      | action
---------------- | ------
a                | add timestamp as piece start
b                | add timestamp as piece end
Ctrl+x           | add timestamp which will act as both (start or end)
Ctrl+d           | delete existing timestamp
'                | navigate forward within existing timestamps
;                | navigate backward within existing timestamps
c                | cut pieces
C                | cut pieces overwriting existing ones
Alt+c            | cut pieces only a+v streams
Alt+C            | cut pieces only a+v streams with overwriting
Ctrl+Shift+LEFT  | switch to previous existing cutted piece
Ctrl+Shift+RIGHT | switch to next existing cutted piece

### Technical info
#### Output containers
By default output format (extension) for cutted pieces of:
* video - matroska (mkv)
* audio - kept same

#### Diagram
Data pass:
[MPV] --> [Lua] --> [C] --> [Haskell] --> Bash script --> [FFmpeg]

#### Pieces composition logic
Rough example:

1. f1 (A:B:ts) = Piece (A, B) : f1 ts
2. f2 ts = f1 ts ++ f2 (ts \\\\ (f1 ts))
3. f3 (A1:A2:B:ts) = Piece (A1, B) : Piece (A2, B) : f3 ts
4. f4 (B:A) = [Piece (Start, B), Piece (A, End)]

### Issues
#### Major build & installation problem #1
The resulting dynamic library depends on many other Haskell libraries.
So we have to either:

* bundle shared library with it's haskell dependencies statically
* link haskell dependencies dynamically and install all of them

Currently both fail most likely because of commercialhaskell/stack#2158.

##### static approach
Static build attempts fail at linking.
Supposedly the static .a libraries shipped with Haskell packages do not allow
further embedding into a shared dynamic library which is the case (built
without -fPIC).
Using "-shared" to build a library (as opposed to executable) immediately
throws errors like "relocation R_X86_64_32S against" ... "recompile with
-fPIC".

The goal is to collect statically all Haskell dependencies with RTS runtime
plus C binding into a single shared dynamic library
(in terms of GHC).

<details><summary>Building a static executable works.</summary>
For example, after adding main function and renaming module to Main in Haskell
library, this command succeeds:

$ ghc MPV_Cut.hs -o mpv-cut -Wall -static

It produces a ELF executable which has dependencies only on C libraries. Using:

$ ghc MPV_Cut.hs -o mpv-cut -Wall -static -optl-static

goes even further and creates a fully statically linked ELF executable without
any dependencies. However I don't know any means to include the C binding
object code into such file. And both variants don't export the needed functions
in the resulting binary.
</details>

##### dynamic approach
I.e. use of this plugin requires all dependencies installed somehow in your system
(or copied to a build time configured path like ~/.config/scripts/mpv-cut-delibs).
So we're left with something like linking with "-rpath=mpv-cut-deplibs".
And put all needed Haskell libraries into mpv-cut-deplibs dir
when installing the plugin.

Even this approach does not work. If using Stack, it simply does not
link shared library with RTS runtime.
Regardless of `ghc-options: -lHSrts-ghc8.0.1`.
It seems like ghc-options are respected well only for executables.

#### Inaccurate MPV seek with audio and some video formats #2
MPV cannot seek to exact requested time with them. This results in inability
to navigate to previously saved timestamp. In practice this even happened with
a few timestamps of mp4 file.

#### Possibly inaccurate FFmpeg seek #3
First of all by it's nature stream copy cannot guarantee frame accurate cut
since in many codecs frames depend on other frames. So it's not only FFmpeg's
fault in every case.

This results in wider pieces than requested. FFmpeg output seeking (as opposed
to input seeking) is in fact more accurate, but in practice misses the
preceding keyframe before the piece, making beginning of piece corrupted
(tested with FFmpeg 3.0.2). So for video input seeking is used. But for
audio-only media accurate output seeking is used.

#### Minor GHCi development inconvinience #4
GHCi 7.10.3 is unable to handle FFI exports nor ignore them. So
MPV_Cut.hs contains #ifdefs to ignore them when GHCi loads .ghci config with
corresponding definition.

### Supported media files formats
Any as long as they're both supported by MPV and FFmpeg.

### Usage hints
MPV provides a number of useful keybindings for navigation, what makes it a
very convenient tool for cutting.

key binding        | seek action
------------------ | ------------
,                  | backward 1 frame and pause
.                  | forward 1 frame and pause
Ctrl + left arrow  | backward 1 second
Ctrl + right arrow | forward 1 second
left arrow         | backward 5 seconds
right arrow        | forward 5 seconds
down arrow         | backward 10 seconds
up arrow           | forward 10 seconds
Shift + PgDn       | backward 10 minutes
Shift + PgUp       | forward 10 minutes


Also playback speed is adjustable:

key binding        | action
------------------ | ------------
[                  | decrease playback speed by 10%
]                  | increase playback speed by 10%
Backspace          | reset playback speed to normal

For more information about MPV usage refer to it's
[documentation](https://mpv.io/manual/stable/).

### Tested with
* MPV 0.17
* Lua 5.2
* FFmpeg 3.0.2
* Bash 4.3.046
* bc 1.06.95
* make 4.2.1
* stackage LTS Haskell 7.3

### License
GPLv3

### Feedback
Any feedback is appreciated! If you have solutions to [Issues](#issues) section,
please share them. And freely express your critique.
