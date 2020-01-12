package main

import (
	"bytes"
	"strings"
	"time"

	"github.com/sergi/go-diff/diffmatchpatch"
)

func NewDiffs(text1, text2 string) []diffmatchpatch.Diff {
	dmp := diffmatchpatch.New()
	dmp.DiffTimeout = 2 * time.Second
	dmp.DiffEditCost = 1
	dmp.MatchThreshold = 0.5
	dmp.MatchDistance = 1000
	dmp.PatchDeleteThreshold = 0.5
	dmp.PatchMargin = 4
	dmp.MatchMaxBits = 32

	diffs := dmp.DiffMain(text1, text2, true)
	diffs = dmp.DiffCleanupSemantic(diffs)
	return diffs
}

func DiffPrettyText(diffs []diffmatchpatch.Diff) string {
	var buff bytes.Buffer
	if len(diffs) == 1 && diffs[0].Type == diffmatchpatch.DiffEqual {
		return ""
	}
	for i, diff := range diffs {
		text := diff.Text
		_ = i
		switch diff.Type {
		case diffmatchpatch.DiffInsert:
			_, _ = buff.WriteString("\x1b[32m")
			_, _ = buff.WriteString(text)
			_, _ = buff.WriteString("\x1b[0m")
		case diffmatchpatch.DiffDelete:
			_, _ = buff.WriteString("\x1b[31m")
			_, _ = buff.WriteString(text)
			_, _ = buff.WriteString("\x1b[0m")
		case diffmatchpatch.DiffEqual:
			lines := strings.SplitAfter(text, "\n")
			const contextLines = 5
			if len(lines) > contextLines*2+2 {
				for _, l := range lines[:contextLines] {
					buff.WriteString(l)
				}
				_, _ = buff.WriteString("\x1b[36m")
				buff.WriteString("━━━━\n")
				_, _ = buff.WriteString("\x1b[0m")

				for _, l := range lines[len(lines)-contextLines:] {
					buff.WriteString(l)
				}
				continue
			}
			_, _ = buff.WriteString(text)
		}
	}

	return buff.String()
}
