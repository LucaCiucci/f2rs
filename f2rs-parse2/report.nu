

# original command
# cargo test -p f2rs-parse2 | complete | get stdout | lines | filter { ($in | str starts-with "test ") and ($in | str contains "...") } | sort
# example line: test tests::test_foo_F18V007r1_401_F18V007r1_402_F18V007r1_403 ... ok


let lines = ^cargo test -p f2rs-parse2
    | complete
    | get stdout
    | lines
    | filter { ($in | str starts-with "test ") and ($in | str contains "...") }
    | sort

let rules = open ($env.FILE_PWD | path join "../resources/18-007r1.rules.yaml")

let file_name = $"($env.FILE_PWD | path join "report.md")"
mut report_top = open $file_name | lines | split list "# Table" | get 0 | append "# Table" | append "\n" | str join "\n"


mut rules_table = ""
$rules_table += "| number | name | status |\n"
$rules_table += "| --- | --- | --- |\n"
mut ok = 0
for $rule in $rules {
    let standard = $rule.standard
    let ident = $rule.ident
    let number = $rule.number
    let matching_lines = $lines | filter { $in | str contains $"($standard)_($number)" }
    let emoji = if ($matching_lines | is-empty) {
        "🚧"
    } else {
        if ($matching_lines | all { $in | str ends-with "ok" }) {
            $ok += 1
            "✅"
        } else {
            "🟥"
        }
    }
    $rules_table += $"| ($number) | ($ident) | ($emoji) |\n"
}

let n = $rules | length
$report_top += $"($ok) out of ($n) tested ok, ($ok / $n * 100 | into int)%\n\n"

$report_top + $rules_table | save -f $file_name