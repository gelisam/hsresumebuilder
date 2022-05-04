# hsresumebuilder

This CLI tool will read a YAML file named `.hsresumebuilder.yaml` in the current directory and generate a resume, also known as CV (Curriculum Vitae) from the preferences there.

It will create or overwrite in the current directory a file named `output.html`. After running the program successfully you can open this HTML file in your browser and print it as PDF. Your browser will allow you to adjust the scale at which you want to print and all other things.

Of course you can always edit the HTML after generation if you feel like it. My goal is to futurely allow the usage of several themes and add customization options in each theme.

You can run this tool if you have Cabal installed, from the root of the project, with:

```
cabal run hsresumebuilder
```

## Language levels
For the language levels, please categorize them from 0, lowest, to 4, highest levels of proficiency.

0 : limited proficiency
1 : limited proficiency +
2 : high proficiency
3 : almost native
4 : native


## Create your own

All keys in the YAML file are required.
See the YAML that I use for creating my own CV [here](.hsresumebuilder.yaml).

Please note that on fields where a list of items is expected, it's also possible to provide `[]` as a value, in case you have nothing to place there.
Failing to provide at least `[]` will result in program failure.

## Preview

To get an idea of the type of resume this tool builds, open the `output.html` file in your browser.
