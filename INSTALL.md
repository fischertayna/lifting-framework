# How to Build & Run the Project

These steps were tested on **Ubuntu 22.04** and **Fedora 38**.  
Minor tweaks may be required on other Linux distributions.

## 1  Install the Haskell toolchain

This project needs **GHC**, **Cabal**, and related tools.  
The simplest way to install them is via **GHCup**:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

GHCup will prompt you to install extra components (e.g., ghc, cabal).
Accept those prompts so Cabal can build the project later.

This project was tested using ghc 9.2.8 and cabal 3.10.2.1

```bash
ghcup install ghc 9.2.8 && \
ghcup set ghc 9.2.8 && \
ghcup install cabal 3.10.2.1 && \
ghcup set cabal 3.10.2.1
```

Verify the versions

```bash
ghc --version && cabal --version
```

## 2 Install build-time utilities (alex, happy, bnfc)

The lexer/parser generators listed below are mandatory:

```bash
cabal install alex
cabal install happy
cabal install BNFC         # Tested with BNFC 2.9.4.1
```

\* Even if bnfc is already present, reinstall it to guarantee a recent version.

## 3 Build & Install CUDD

The project depends on the CUDD BDD library.

```bash
git clone https://github.com/ivmai/cudd
cd cudd

./configure --enable-obj --enable-dddmp --enable-shared
make
make check
sudo make install
```

Some header files are not installed by default; copy them manually:

```bash
sudo cp config.h /usr/local/include/
sudo cp util/util.h /usr/local/include/
sudo cp mtr/mtr.h /usr/local/include/
```

Next, expose CUDD to Cabal (note the --lib flag):

```bash
C_INCLUDE_PATH=/usr/local/include \
LIBRARY_PATH=/usr/local/lib \
cabal install --lib cudd
```

## 4 Build This Project

Return to the project root and compile:

```bash
C_INCLUDE_PATH=/usr/local/include \
LIBRARY_PATH=/usr/local/lib \
cabal build
```

## 5 Run

You can now run the sample analysis pipeline (replace the input file as needed):

```bash
cabal run < src/Language/Analysis/Concat.lng
```

Open app/Main.hs to choose which interpreter (e.g., Typecheck, Base, Var, Memo, …) is executed and inspect the corresponding Driver.hs to see which input file is supplied.

---

## Troubleshooting

- **make don't work for CUDD** - If there is an error with aclocal.m4 you can use the tag cudd-3.0.0 and touch the file.

Change the first block of commands of CUDD instructions for:

```bash
git clone --branch cudd-3.0.0 https://github.com/ivmai/cudd
cd cudd
touch aclocal.m4 configure Makefile.in
```

The following commands stays the same.

- **Missing headers or libraries** – verify that `/usr/local/include` and `/usr/local/lib` contain the CUDD artifacts and that the environment variables above are set.

It may be necessary to export some variables:

```
export PATH="/root/.cabal/bin:/root/.ghcup/bin:${PATH}" \
    C_INCLUDE_PATH=/usr/local/include \
    LIBRARY_PATH=/usr/local/lib \
    LD_LIBRARY_PATH=/usr/local/lib
```

- **bnfc version mismatch** – reinstall with cabal install BNFC to fetch the latest release.


---

## Docker image

A ready-to-use Docker image containing the full build environment is published on the GitHub Container Registry.

| Tag                                             | Description                        |
| ----------------------------------------------- | ---------------------------------- |
| `ghcr.io/fischertayna/lifting-framework:sblp25` | Release used in the SBLP ’25 paper |

1. **Pull the image**

```bash
docker pull ghcr.io/fischertayna/lifting-framework:sblp25
```

2. **Start an interactive shell (optional)**

```bash
docker run --rm -it ghcr.io/fischertayna/lifting-framework:sblp25 bash
```

Note: No authentication is required as the image is public.