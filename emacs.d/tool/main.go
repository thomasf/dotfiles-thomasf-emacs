package main

import (
	"os"
)

func main() {
	for _, file := range initFiles {
		emacs, err := loadEmacsInit(file)
		if err != nil {
			panic(err)
		}
		Must(emacs.Sort())
		Must(emacs.FixLineSpacing())
		Must(emacs.Diff(os.Stdout))
		Must(emacs.Save())
	}
}

func Must(err error) {
	if err != nil {
		panic(err)
	}
}
