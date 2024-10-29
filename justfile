@default:
    @just --list

@clean:
    rm -rf build

@test:
    scheme --script test.scm
