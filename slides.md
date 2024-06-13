---
# try also 'default' to start simple
theme: default #apple-basic
# random image from a curated Unsplash collection by Anthony
# like them? see https://unsplash.com/collections/94734566/slidev
background: p&id_epr.png
# some information about your slides, markdown enabled
title: Ada Community Advocacy
info: |
  ## Ada Community Advocacy
  Fostering a healthy ecosystem for the future of Ada

# apply any unocss classes to the current slide
class: text-center
# https://sli.dev/custom/highlighters.html
highlighter: shiki
# https://sli.dev/guide/drawing
drawings:
  persist: false
# slide transition: https://sli.dev/guide/animations#slide-transitions
transition: slide-left
# enable MDC Syntax: https://sli.dev/guide/syntax#mdc-syntax
mdc: true

colorScheme: light

fonts:
  sans: 'Mona Sans'
  mono: 'Monaspace Neon'
  local: 'Mona Sans, Monaspace Neon'
---

# Ada Community Advocacy

### Fostering a healthy ecosystem for the future of Ada

Fernando Oleo Blanco - [Irvise](https://irvise.xyz/)

---
transition: fade-out
---

# Contents

What to expect

- Introduce myself

- Difference between formal & organised community and the _Outer Wilds_

- Being efficient, fostering and selling Ada

- _Bob Dylan - The Times They Are A-Changin'_

- Future? Hopefully a bright one!

## Community discussion, **feel free to participate & interrupt!**

The presentation style is going to be *very* informal

---
layout: image-right
image: ./Fer_ASDEX-1.jpg
---

# `whoami`

- Fer, the original since 1997 :)
   - Irvise in the net
- Industrial and mechanical engineer
   - Materials, thermals, fluids, FEM
- Currently working in the nuclear sector
   - Systems engineer (P&IDs, cycles, components...), no programming!
- Huge [Libre Software](https://www.gnu.org/philosophy/free-sw.en.html) aficionado
   - Full-time Linux since 18
- And what about Ada?
  - Hopefully my point of view enhances yours

---
layout: two-cols
layouClass: gap-16
---

# Programming history

- `Hello World!` in Ruby at... 16?
- Functions, loops, ifs and Fibonacci in Python 3
- University, bachellor
  - C (for embedded systems)
  - Matlab, Simulink
  - Operational Research...
  - ???
- Industrial Eng. Master
  - Function Blocks, Structured Text, Sequential Function Charts, Ladder (PLC)
  
::right::

<v-click>

# Fortran 66

[FRAPTRAN-2.0](https://fast.labworks.org/), nuclear safety analysis (LOCA)

Very well written, documented, commented and structured

```f77
      COMMON A/B
C     COMMENT
      X=KODE(J)
      IF (X.LT.0) GO TO 55
      IF (X.EQ.0) GO TO 55
      GO TO 40

55    CALL SOMETHING(INTPUT, OUTFLOAT)
```
</v-click>

<v-click>
<br>
Can we do better?
</v-click>

<v-click>

- **Fortran 2003-2023**
- C/C++?
- [Frama-C](https://frama-c.com/), [seL4](https://sel4.systems/)?

[*Real Programmers Don't Use PASCAL*](https://homepages.inf.ed.ac.uk/rni/papers/realprg.html)

</v-click>

---
layout: two-cols
layoutClass: gap-16
---


<div v-click.hide="3">

# Enter... Ada

- "Formally verified with [SPARK](https://learn.adacore.com/courses/intro-to-spark/index.html)"
- "Readable and easy to learn"
- "Efficient, high & low level, compiled and without GC" --- Blazingly f*cking fast
- "Used in the army, aeronautical and space"
- ...

<div v-click="[1, 3]">

## 🤔🧐
```ada
with Ada.Text_IO;

procedure My_Hello_World is
begin
   Ada.Text_IO.Put_Line ("Hello, World!");
end My_Hello_World;
```

Mmmm...

</div>
</div>

<div v-click="3" class="absolute left-30px top-10px">

![Poggers](/poggers.jpeg)

</div>

::right::

<div v-click="2">

```ada
-- 18.3 Memory Map
type UART_Type is record
   txdata : txdata_Type with Volatile_Full_Access => True;
   rxdata : rxdata_Type with Volatile_Full_Access => True;
   txctrl : txctrl_Type with Volatile_Full_Access => True;
   rxctrl : rxctrl_Type with Volatile_Full_Access => True;
   ie     : ie_Type     with Volatile_Full_Access => True;
   ip     : ip_Type     with Volatile_Full_Access => True;
   div    : div_Type    with Volatile_Full_Access => True;
end record
   with Size => 7 * 32;
for UART_Type use record
   txdata at 16#00# range 0 .. 31;
   rxdata at 16#04# range 0 .. 31;
   txctrl at 16#08# range 0 .. 31;
   rxctrl at 16#0C# range 0 .. 31;
   ie     at 16#10# range 0 .. 31;
   ip     at 16#14# range 0 .. 31;
   div    at 16#18# range 0 .. 31;
end record;
UART0_BASEADDRESS : constant := 16#1001_3000#;
UART0 : aliased UART_Type
   with Address    => System'To_Address (UART0_BASEADDRESS),
        Volatile   => True,
        Import     => True,
        Convention => Ada;
```

</div>

---
---

# My beginning

<Youtube id="YPD9U4Wuh5A" width="875" height="430"/>

---
transition: fade
---

# Work done in Ada

1. Started using [AVRAda](https://github.com/RREE/AVRAda_Examples), gave and received some feedback
2. Ada and RISC-V? Lets goooo [HiFive Rev B](https://www.sifive.com/boards/hifive1-rev-b)
   1. Helped get the Rev B up and running in [`Ada_Drivers_Library`](https://github.com/AdaCore/Ada_Drivers_Library/commit/c958bb1d7fdf941b14b96eea61a78edb77216a10)
3. [NetBSD](http://netbsd.org/) runs everywhere... and so does Ada in theory...
   1. With the huge help of [J. Marino](http://www.ravenports.com/), updated [GNAT 10](https://cdn.netbsd.org/pub/pkgsrc/current/pkgsrc/lang/gcc10-aux/index.html) in [pkgsrc](https://www.pkgsrc.org/) (`x86*` only)
   ---
4. [Dirk Craynest](https://archive.fosdem.org/2020/schedule/speaker/dirk_craeynest/) was stepping down from organising the Ada Devroom @ FOSDEM 
   1. No body stood up for 2022, so I did
      1. I really liked the Ada videos in FOSDEM, specially [J-P's Introduction to Ada](https://archive.fosdem.org/2018/schedule/event/ada_introduction/)
      2. I did not want it to die...
   2. (Virtual) Room in 2022
   3. Stand in 2023
   4. Nothing in 2024...
5. Ada was dying... something had to be done... [Ada Monthly Meeting](https://forum.ada-lang.io/t/ada-monthly-meeting/384) started in 2023

---
layout: center
class: text-center
---

# Focus on informal & amateur communities

---
layout: two-cols
layoutClass: gap-16
transition: fade
---

# The formal side of the Ada community

Ada has had a long and stablished professional community:

- [Ada-Europe](http://www.ada-europe.org/)
  - And the different national assemblies
- [SIGAda](https://sigada.org/) (RIP?)
- [WG-9](https://www.open-std.org/jtc1/sc22/wg9/), the ones building the ISO standard
- [AdaCore](https://www.adacore.com/)
- Army
- Aerospace
- ...

<v-click>
But I am not going to talk about them here
</v-click>

::right::

<v-click>

# The informal side of the Ada community

Hackers, newbies, tinkers... A mostly decentralised buch of great people

- IRC, `#ada` in [Libera.chat](https://libera.chat/)
- [C.L.A](https://usenet.ada-lang.io/comp.lang.ada/), USENET
- [Telegram](https://t.me/ada_lang)
- [Matrix/Gitter/Element](https://gitter.im/ada-lang/Lobby)
- [Discord](https://discord.gg/edM2czVKN4)
- [Reddit](https://www.reddit.com/r/ada/)
- [**Ada-Lang**](https://ada-lang.io/)
  - And its [forum](https://forum.ada-lang.io/)

</v-click>

<v-click>

This group will be my focus, as I feel this is of extreme importance for Ada's future

</v-click>

---
layout: center
class: text-center
---

# Success, a story told in two acts

---
---

# Ada is great, don't we all agree?!
Yes, it may not be perfect for every case but...

### Ada is obviously successful, isn't it?

- Great typing system!
  - The modern programming world favours types, see [**Rust**](https://www.rust-lang.org/), [**TypeScript**](https://www.typescriptlang.org/), [**Python 3**](https://docs.python.org/3/library/typing.html), [Gleam](https://gleam.run/index.html), [Elixir](https://elixir-lang.org/blog/2023/06/22/type-system-updates-research-dev/), Swift, Kotlin, Dart, Go, [even f*cking PHP](https://www.php.net/manual/en/language.types.declarations.php)...
- Great readability and clean syntax! Python, Lua-like (yes, Pascal)
- Ease of learning! So much so, that new people point out that it is *too verbose,* **what a compliment!**
- Great module system! Unlike some very well-known languages that are just as old...
- Amazing features!
  - [Tasking](https://learn.adacore.com/courses/intro-to-ada/chapters/tasking.html), [Contracts](https://learn.adacore.com/courses/intro-to-ada/chapters/contracts.html), [`C` interop](https://learn.adacore.com/courses/intro-to-ada/chapters/interfacing_with_c.html), [WASM](https://blog.adacore.com/use-of-gnat-llvm-to-translate-ada-applications-to-webassembly), nice standard library, great embedded support...
- Mature: stable, well-understood, documented, with high-quality learning resources...
- [SPARK](https://learn.adacore.com/courses/intro-to-spark/index.html), state-of-the-art formal verification
- People who (properly) learn it, [praise it](https://www.youtube.com/watch?v=MUISz2qA640) even [when they struggle](https://forum.ada-lang.io/t/making-a-game-in-ada-with-raylib/704)

---
---

# What is success for a language (and its community)

Sucess... is...

<v-click>

$Succ_{theory} = f(features, optimisations, libraries, ease\_of\_use, robustness, readiness...)$

</v-click>

<v-click>

Ada should be pretty much perfect except for:

- Prototyping or scripting: [Lua](https://github.com/Nikokrock/ada-lua), [Python 3](https://github.com/AdaCore/gnatcoll-bindings/tree/master/python3)
  - Check out [HAC](https://github.com/zertovitch/hac) and [SPARforte](https://sparforte.com/) for some Ada-based scripting!
- Dynamic, meta-programming: [Scheme](https://www.scheme.org/), [LISP](https://lisp-lang.org/), [Forth](https://forth-standard.org/)
<v-click>

- **Harder issues:** fast-paced world, time-to-market, prototyping, MVP...
   - <span class="text-sm"> [How to Build a Product that Scales into a Company](https://www.youtube.com/watch?v=r-98YRAF1dY&t=440s) *"You have to spend twice as much $ taking a product to market"* </span>

</v-click>

</v-click>

<v-click>

### Reality hits hard

<div class="text-center">

$Succ_{real} = Succ_{theory} \cdot \eta$

</div>

In this case, $\eta$ means efficiency, effectiveness to bring that theoretical potential to reality...

</v-click>

<v-click>

<div class="text-center">

For Ada, $\eta$ is... quite low

</div>

</v-click>

---
layout: two-cols
layoutClass:
---

# The community **is** the reflection of efficiency
Why do communities exist and what do the represent?

Communities are built around percieved value:
- Interesting?
- Fun or enjoyable?
- Profitable?
- Fulfilling or productive, maybe good-enough?
- Ease of access and/or ease of learning?
- Modern or trendy?
- Honor or tradition?
- Requirement, such as certification or debt?
- **Sense of belonging or community?**

::right::

<v-click>

<br>

# Ada's case
A different story in my opinion...

<br>
<br>
<br>

- Very interesting and capable
- Less fun? Focused on getting sh*t done
- Few jobs available
- Yes, if you care about quality
- Yes and no?
- ...
- Some?
- Yes, legacy and technical debt...
- **???**

</v-click>

---
---

# New and old problems to foster a healty, growing & sustainable community

<v-clicks depth="2">

- Companies want programmers, programmers want jobs...
  - Chicken and egg problem, and Ada is losing it
  - Every other new programming language **had to deal with the same issue!**
- Programmers want ease of use and a wealth of libraries
  - The community (professional and informal ones) do not have much to offer
  - [Alire](https://alire.ada.dev/) is closing the discovery gap
- Newbies and professionals want **lots of resources the way they want them!!!!**
  - Books ✅. Forums ✅ chats ✅ (though quite new and underutilised)
  - Videos (like Youtube)? Blogs? "Unrelated" forums, conferences chats? University?
    - **Times are changing old men**
      - Trends rule the modern day, success will in the end be based on merits
- **Are you proud of being part of the Ada community? Are we  accessible to newcomers?**

</v-clicks>

---
---

# A sense of belonging, visibility and sustainability I
Ada runs on a shoe-string budget

### Help other Ada users... and yourselves!
<br>

- [Apple / OSX / Mac](https://github.com/simonjwright/building-gcc-macos-native) supported by only one person, [Simon J. Wright](https://github.com/simonjwright)
  - Also quite a lot of ARM!
- SweetAda is developed solely by [Gabriele Galeotti](https://www.linkedin.com/in/gabriele-galeotti-92921019?trk=public_post_feed-actor-name&original_referer=https%3A%2F%2Fwww.linkedin.com%2F)
  - Single showcase of multiarch support in Ada
- [AdaCore](https://www.adacore.com/) is still the driving force
  - While there has been friction in the past, lets try to cooperate
- Some younger folks would be welcomed!
  - The average age of the tipical Ada programmer is... a bit high

<v-click>

## Efficiency is key here! If you have something cool, promote it!

</v-click>

---
---

# A sense of belonging, visibility and sustainability II
The community is quite small, but incredibily knowledgeable

### Show the world!
- Show the rest of the community and the wider programming world what we are capable of!
  - The topics in this conferece are a testament on how *cool* we are!
  - Be proud of and talk about them!

<v-click>

### Help lower the barrier of entry

- Updated learning methods, more dynamic and accesible ones... **Documentation is key!**
  - [Learn.AdaCore](https://learn.adacore.com/) & [Ada-Lang](https://ada-lang.io/) are great, but not enough
  - Libraries and tools also need their docs, just look at Rust!
- Help with questions, share best practices, tricks
  - A bigger community greatly helps here
- Better tooling and libs
  - [Alire](https://alire.ada.dev/) & [GetAda](https://www.getada.dev/)
  - Need to improve OS support and basic libraries/tools

</v-click>

---
layout: image-right
image: ./steveballmersweat.gif
---

# Utilise the resources around us

Developers, develpers, developers, (& marketing)

- "This would be better written in Ada"
  - Be efficient, reuse C libs, showcase value-added Ada

<v-click>

- Participate in events!
  - Google Summer of Code (GSoC)
    - Even [GNU Cobol](https://summerofcode.withgoogle.com/programs/2024/projects) is there!
    - [Fortran-Lang](https://summerofcode.withgoogle.com/programs/2024/organizations/fortran-lang) too!
  - Language conferences
    - General ones and **formal verification**

</v-click>

---
---

# Closing remarks
<br>

### Ada excels at pretty much any technical level, it is impressive

<v-click>

- But we need to promote it, market it and showcase it to the world
  - Use the same marketing techniques and utilise the current trends. Let the world help you

<br>
</v-click>

<v-click>

### Welcome new people and encourage collaboration and modern practices
IMHO, there are more people interested in Ada than ever before

</v-click>

<v-click>

### Maintain a healthy relationship between the formal and informal communities

<br>
</v-click>

<v-click>

### Become more efficient, dynamic, bolder... younger?

More proactive, less complacent?

</v-click>


---
layout: center
class: text-center
---

# Thank you!

## Comments? Critics?

Fernando Oleo Blanco -/- [irvise@irvise.xyz](mailto:irvise@irvise.xyz)
