# HaskQLibrary

HaskQLibrary is a command-line library management system built in Haskell, backed by SQLite.

---

## Installation Guide

### Prerequisites

#### 1. Install GHCup
GHCup is the recommended way to install GHC and Stack on all platforms.

**Windows (run in PowerShell):**
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force
Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing | Invoke-Expression
```

**macOS / Linux:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

During setup, say **Yes** to installing HLS (Haskell Language Server) and **Yes** to Stack.

#### 2. Install GHC and Stack via GHCup
```bash
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3
ghcup install stack
ghcup install hls
```

#### 3. Verify Installation
```bash
ghc --version
stack --version
```

---

## Running the Project

#### 1. Clone the repository
```bash
git clone https://github.com/your-org/haskell-opac.git
cd haskell-opac
```

#### 2. Build the project
```bash
stack build
```
First build will take a few minutes — Stack downloads GHC and all dependencies automatically.

#### 3. Run the app
```bash
stack run
```

---

## Editor Setup (VS Code)

1. Install the **Haskell** extension by Haskell.org
2. Open the project folder
3. Generate the HLS config file:
```bash
stack install implicit-hie
stack exec -- gen-hie > hie.yaml
```
4. Reload the window — `Ctrl+Shift+P` → `Developer: Reload Window`

---

## Dependencies

- `sqlite-simple`
- `bcrypt`
- `haskeline`
- `mtl`
- `transformers`
- `text`
- `time`

---

## Project Architecture

The app is organized into four layers, each building on the one below it.

**Models** define the core data types — `User`, `Book`, `BorrowRecord` — as flat records that map directly to SQLite tables.

**DB** handles the database — opening connections, running transactions, and executing migrations on startup to keep the schema up to date.

**Queries** sit above the DB layer and provide typed functions for reading and writing each model. Nothing here contains business logic — just SQL wrapped in `AppM`.

**Services** contain the actual business logic — password hashing and login validation in `Auth`, borrow/return rules and fine calculation in `Borrow`, hold queue management in `Reservation`.

**CLI** is the top layer — a `haskeline`-powered REPL loop that dispatches to screens for search, borrowing, reservations, and admin functions. All screens share common `Prompt` and `Display` utilities.

Everything runs inside `AppM`, a `ReaderT Env IO` monad that carries the database connection through the entire app without passing it explicitly to every function.

---

## Contributors
| Name | Role |
| :---: | :---: | 
| Masaharu Kayama | Project Lead |
| Lester Manapul | Documentation Lead |
| Kirk Vallete | Haskell Developer |
| Kline Calaguas | Haskell Developer |
| Rashley Serioza | Database Engineer |
| Sean Ong | Database Engineer |
