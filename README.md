## ZIO companion plugin for IntelliJ IDEA

[![Project stage][Stage]][Stage-Page]

Provides inspections, refactorings, and other improvements to the ZIO library ecosystem users in IntelliJ IDEA.

### Installation

The plugin is now available on the [JetBrains Plugin Marketplace](https://plugins.jetbrains.com/plugin/13820-zio-for-intellij).

<img src="https://user-images.githubusercontent.com/601206/79748708-89a48080-8316-11ea-95a3-3542dab04684.png" />

**Supported versions**: IntelliJ IDEA 2022.x, 2021.x (Community edition or better)

A note about support: this plugin is maintained for the last two IntelliJ IDEA versions. When a new IDEA is released, the oldest supported version will stop getting new features (except critical bug fixes).

To continue enjoying new features, please upgrade to the latest available IntellJ version!

**Currently supported**:
 * 2022.3 (EAP)
 * 2022.x (current release)
 * 2021.x

Previous versions are still available on the Marketplace.

### Features

The plugin has several powerful features for ZIO:

#### Test runner support

You can run individual ZIO tests or the whole specs from IDEA:

<img src="https://user-images.githubusercontent.com/601206/79748798-af318a00-8316-11ea-818d-5f266aa52ffe.png" />

And view the results in the [integrated runner](https://plugins.jetbrains.com/plugin/13820-zio-for-intellij/zio-test-runner):

<img src="https://user-images.githubusercontent.com/601206/79748960-fb7cca00-8316-11ea-8e4a-e080de4bdf1c.png" />

#### Refactorings and inspections

Don't want to type out the full type signature? Choose a more specific one:

![specific-type](https://user-images.githubusercontent.com/601206/74927065-a0def480-53df-11ea-934a-f74aebaf2c54.gif)

Suggestions to simplify complex expressions using built-in ZIO combinators:

![](https://user-images.githubusercontent.com/601206/74927181-d08dfc80-53df-11ea-922d-15bbe471f736.gif)

**And many more features!**

### Contributing

Like what you see? Help improve this plugin by submitting issues and ideas!

Have questions? We're waiting for you at `#zio-intellij` on the ZIO Discord!

### Acknowledgments

This project is using the YourKit Java Profiler to bring you bug-free and performant experience!  
![](https://www.yourkit.com/images/yklogo.png) 

YourKit supports open source projects with innovative and intelligent tools for monitoring and profiling Java and .NET applications.
YourKit is the creator of [YourKit Java Profiler](https://www.yourkit.com/java/profiler/), [YourKit .NET Profiler](https://www.yourkit.com/.net/profiler/),
and [YourKit YouMonitor](https://www.yourkit.com/youmonitor/).

[Stage]: https://img.shields.io/badge/Project%20Stage-Production%20Ready-brightgreen.svg
[Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages
