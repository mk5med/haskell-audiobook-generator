# haskell-audiobook-generator

Easily transform books into audiobooks.

Features
- [x] Accept books as txt files
- [x] Split books by chapter
- [x] Split chapters to a max length of 1000 lines
- [ ] Use TTS model on each chapter
- [ ] Zip all audio files

## CLI Use

```
Usage: audiobook-generator [OPTION...]
  -i FILE     --input=FILE      (REQUIRED) Path to input file
  -o DIR      --output=DIR      (REQUIRED) Path to output directory
  -s[NUMBER]  --split[=NUMBER]  Split the input file by NUMBER.
                                If omitted the program will use a heuristic chapter processor
```

Convert a book with the max-lines chapter processor. This is useful when the chapter delimiters in the book are not detected by the heuristic processor.

```bash
./audiobook-generator -i input.txt -o chapters -s
# or
./audiobook-generator -i input.txt -o chapters -s 500
```

Convert a book with the heuristic chapter processor.

```bash
./audiobook-generator -i input.txt -o chapters
```

## Getting started with development

1. Install stack. https://docs.haskellstack.org/en/stable/#how-to-install-stack
2. Clone the repository.
3. Run `stack run -- [args]`. Replace `[args]` with the CLI arguments from [above](#cli-use).
