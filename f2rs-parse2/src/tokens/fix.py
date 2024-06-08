
FILE = "rules.rs"

# replace all "(source: S) -> impl Parser<S, Token = SOME_TOKEN<S::Span>>"
# with: (source: S) -> impl PResult<SOME_TOKEN<S::Span>, S>

with open(FILE, "r") as f:
    lines = f.readlines()

with open(FILE, "w") as f:
    for line in lines:
        TRIGGER = "(source: S) -> impl Parser<S, Token = "
        pos = line.find(TRIGGER)

        if pos == -1:
            f.write(line)
            continue

        line = line.replace(TRIGGER, "(source: S) -> impl PResult<").replace("> {\n", ", S> {\n")

        f.write(line)
