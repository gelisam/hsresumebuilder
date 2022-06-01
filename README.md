# λ hsResumeBuilder

![Haskell](https://img.shields.io/badge/Haskell-purple?style=for-the-badge&logo=haskell&logoColor=white)
![Cabal](https://img.shields.io/badge/Cabal-blue?style=for-the-badge&logo=haskell&logoColor=white)

hsResumeBuilder is an attempt at a Curriculum Vitae / Resume generator written entirely in Haskell.

See [my resume as an example](https://averageflow.github.io/hsresumebuilder/), made with this very tool.

The concept is a highly-customizable theme-based resume generator, currently supporting one theme.

I personally use it myself for my own CV, so that if I ever get tired of how it looks, i don't need to change all data to change the looks of it. My data stays safely in a YAML file, while i can go happily about tweaking colors and HTML structure, even CSS files. 

Any new theme is welcome in a pull request, and we gotta figure out the best way to make theme selection customizable from YAML :) 

### Usage

This CLI tool will read a YAML file named `hsresumebuilder.yaml` in the current directory and generate a resume from the preferences in the file.

All keys in the YAML file are required.
See the YAML that I use for creating my own CV [here](hsresumebuilder.yaml).

Please note that on fields where a list of items is expected, it's also possible to provide `[]` as a value, in case you have nothing to place there.
Failing to provide at least `[]` will result in program failure.

It will create or overwrite in the current directory a file named `output.html`. 

After running the program successfully you can open this HTML file in your browser and print it as PDF. 

Your browser will allow you to adjust the scale at which you want to print and all other things.

Of course you can always edit the HTML after generation if you feel like it. My goal is to futurely allow the usage of several themes and add customization options in each theme.

#### Running the binary

Please head over to the [releases page](https://github.com/averageflow/hsresumebuilder/releases), and download the latest release for your system. Currently supporting Linux systems (use ubuntu-latest binary) and macOS systems.

#### Running from source

You can run this tool if you have Cabal installed, from the root of the project, with:

```
cabal run hsresumebuilder
```


### Visit the [Wiki](https://github.com/averageflow/hsresumebuilder/wiki) for more
