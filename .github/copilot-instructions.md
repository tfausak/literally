# Literally Haskell Library

Literally is a minimal Haskell library that converts type literals into values. It provides a single module with type-safe literal conversion functionality.

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

## Working Effectively

### Setup and Building
- **Bootstrap the environment**:
  - The repository uses Cabal with GHC 9.12.2
  - All required Haskell tools are pre-installed: `cabal`, `ghc`, `hlint`, `cabal-gild`
  - Ormolu requires installation (takes 10+ minutes - see CI section below)

- **Core build commands**:
  - `cabal build` -- builds the library in ~4 seconds. NEVER CANCEL.
  - `cabal clean && cabal build` -- clean rebuild in ~4 seconds. NEVER CANCEL.
  - `cabal check` -- validates cabal file (~0.01 seconds)
  - `cabal sdist` -- creates source distribution (~0.01 seconds)

- **Interactive development**:
  - `cabal repl` -- starts GHCi REPL (takes ~1m 47s first time, includes compilation). NEVER CANCEL. Set timeout to 200+ seconds.
  - Test core functionality in REPL: `literal 42 :: Integer` returns `42`
  - Test string literals: `literal "hello" :: String` returns `"hello"`
  - Exit REPL with `:quit`

### Documentation and CI Tools
- **Documentation generation**:
  - `cabal haddock` -- generates Haddock documentation (~5 seconds). NEVER CANCEL.
  - Output goes to `dist-newstyle/build/x86_64-linux/ghc-9.12.2/literally-0.2025.9.9/doc/html/literally`

- **Linting and formatting** (run before committing):
  - `hlint .` -- runs HLint linter (~0.3 seconds)
  - `cabal-gild --input literally.cabal --mode check` -- checks cabal file formatting (<0.01 seconds)
  - **Ormolu installation**: `cabal install ormolu --install-method=copy --installdir=/tmp/bin` (takes 10+ minutes, NEVER CANCEL. Set timeout to 1200+ seconds)
  - **Ormolu formatting**: `/tmp/bin/ormolu --mode check Literally.hs` -- checks Haskell code formatting (~0.07 seconds)

### CI Validation (Always run before committing)
Run these commands in order to match the GitHub Actions CI pipeline:
1. `cabal check`
2. `cabal-gild --input literally.cabal --mode check`  
3. `hlint .`
4. Install ormolu if not present: `cabal install ormolu --install-method=copy --installdir=/tmp/bin` (NEVER CANCEL - takes 10+ minutes)
5. `/tmp/bin/ormolu --mode check Literally.hs`
6. `cabal build`

## Validation

### Manual Testing Requirements
- **ALWAYS test library functionality after changes**:
  - Start `cabal repl` (allow 200+ seconds)
  - Test numeric literal: `literal 42 :: Integer` should return `42`
  - Test string literal: `literal "hello" :: String` should return `"hello"`
  - Test character literal: `literal 'x' :: Char` should return `'x'`
  - Exit with `:quit`

- **ALWAYS run complete CI validation** before completing work:
  - All linting tools must pass (hlint, cabal-gild, ormolu)
  - Build must succeed
  - Documentation generation must work
  - REPL functionality must work

### Expected Timings (NEVER CANCEL - wait for completion)
- `cabal build`: ~4 seconds
- `cabal repl` (first time): ~1m 47s - Set timeout to 200+ seconds
- `cabal haddock`: ~5 seconds  
- `hlint .`: ~0.3 seconds
- `cabal-gild check`: <0.01 seconds
- `ormolu check`: ~0.07 seconds
- **Ormolu installation**: 10+ minutes - Set timeout to 1200+ seconds, NEVER CANCEL

## Repository Structure

### Key Files and Directories
```
.
├── .github/
│   └── workflows/
│       ├── ci.yml                    # Main CI pipeline
│       └── copilot-setup-steps.yml   # Copilot setup workflow
├── .gitignore                        # Git ignore rules
├── Literally.hs                     # Main library module (71 lines)
├── cabal.project                     # Cabal project config
├── cabal.project.local              # Local cabal settings (tests enabled)
├── dist-newstyle/                   # Build output directory
└── literally.cabal                  # Package definition and metadata
```

### Source Code
- **Single module**: `Literally.hs` contains the entire library
- **Core functionality**: `literal` function converts type literals to values
- **Type classes**: `FromType` and `KnownType` provide the conversion mechanism
- **Supported types**: Natural numbers, characters, strings, tuples, and unit type

### Build Artifacts
- `dist-newstyle/` contains all build outputs (ignored by git)
- Documentation generated in `dist-newstyle/build/.../doc/html/literally`
- No test suite defined in this project

## Common Tasks

### Working with the Library
```bash
# Quick build and test cycle
cabal build
cabal repl
# In REPL: literal 42 :: Integer
# Exit: :quit

# Full validation (CI equivalent)
cabal check
cabal-gild --input literally.cabal --mode check
hlint .
/tmp/bin/ormolu --mode check Literally.hs  # Install first if needed
cabal build
cabal haddock
```

### Package Information
- **Name**: literally  
- **Version**: 0.2025.9.9
- **Synopsis**: Convert type literals into values
- **License**: 0BSD
- **Dependencies**: Only base ^>={4.20, 4.21}
- **GHC Requirements**: 9.10, 9.12 (tested on multiple platforms)

This is a minimal, focused library with fast build times and comprehensive CI validation. The main complexity is in the type-level programming, not in the build system.