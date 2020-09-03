package compiler

import (
	"error"
	"io/ioutil"
	"os"
	. "parser"
	"path"
	Path "path/filepath"
	"strconv"
)

var exPath, _ = os.Executable()
var libPath = Path.Join(Path.Dir(exPath), "../lib")

var wd, _ = os.Getwd()

var dfPath = path.Join(libPath, "internal/default.h")
var DefaultC = []byte(`
int main() {
	return v0_main();
}`)

var ProjectDir string

func ImportFile(dir string, base string, isMain bool, num2 int, pack bool) *SymbolTable {
	n := strconv.Itoa(num)
	n2 := strconv.Itoa(num2)

	if isMain {
		ProjectDir = dir
	}

	path := Path.Join(dir, base)
	rel, err := Path.Rel(ProjectDir, path)

	if err != nil {
		rel, _ = Path.Rel(libPath, path)
	}

	OutPath := Path.Join(ProjectDir, "_build", Path.Dir(rel), n2+Path.Base(base))

	if isMain {
		OutPath += ".c"
	} else if Path.Ext(OutPath) != ".h" {
		OutPath += ".h"
	}

	Code, err := ioutil.ReadFile(path)

	if err != nil {
		path = Path.Join(libPath, base)
		Code, err = ioutil.ReadFile(path)
	}
	if err != nil {
		error.NewGenError("error finding import: " + err.Error())
	}

	buildDir := Path.Dir(OutPath)

	os.MkdirAll(buildDir, os.ModeDir)
	os.Chmod(buildDir, 0777)

	f, err := os.Create(OutPath)

	if err != nil {
		error.NewGenError("error creating files: " + err.Error())
	}

	if Path.Ext(path) == ".h" {
		f.Write(Code)
	} else {
		ast := ParseFile(&Lexer{Buffer: Code, Line: 1, Column: 1, Path: path})
		symbols, imports, prefixes, exports, numm := AnalyzeFile(ast, path)
		newAst := FormatFile(ast, symbols, imports, prefixes, numm)

		if !isMain {
			f.Write([]byte("#ifndef H_" + n + "\n#define H_" + n + "\n"))
		}
		f.Write([]byte("#include \"internal/default.h\"\n"))
		f.Write(CompileOnlyDeclarations(newAst, pack))

		f.Write(CompileOnlyInitializations(newAst, pack))

		if isMain {
			f.Write(DefaultC)
		} else {
			f.Write([]byte("\n#endif"))
		}
		f.Close()

		return exports
	}
	return &SymbolTable{}
}
