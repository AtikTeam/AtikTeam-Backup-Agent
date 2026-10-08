# atikteam-backup-agent

Connects to the AtikTeam backup API, maintains a local database of the files
(identified by their hash), downloads the ones that changed, expires old
versions, and provides a web interface to browse the backups.

## Usage

    atikteam-backup-agent [-p PORT] [--host ADDRESS] [-l info|notice|error]
                          [--data-dir DIRECTORY] [--static-dir DIRECTORY]
                          [--interval SECONDS]

The interface is at http://127.0.0.1:5500 by default. The database is in the
`AtikTeam/backup` directory of the user data directory (`~/.AtikTeam/backup` on
Linux).

The web interface is in English by default. The language (English or French)
can be changed with the buttons in the page header; the choice is stored in the
`language` file of the database directory and applies to everyone using this
database.

## Building

    cabal build                                        # development
    cabal test --enable-tests                          # tests of the pure functions
    cabal build --project-file=cabal.project.release   # static binary (Alpine/musl)

The files of `static/` (style sheet, script, logo) are embedded in the
executable at compile time. To work on them without recompiling, run the
program with `--static-dir static`: the files are then read from that directory
on every request.

Example of a Alpine/musl docker environment with cabal cache persistence.

    docker run -it --rm   -v "$PWD:/mnt" -w /mnt   -v cabal-cache:/root/.cache/cabal   -v cabal-store:/root/.local/state/cabal/store --entrypoint bash   docker.io/benz0li/ghc-musl:9.10.3-int-native


## Certificates

HTTPS connections use the root certificates of the operating system (the
certificate store on Windows; `/etc/ssl/certs` or the `SSL_CERT_FILE` /
`SSL_CERT_DIR` variables on Linux). On Linux, the `ca-certificates` package
must be installed where the static binary runs.

