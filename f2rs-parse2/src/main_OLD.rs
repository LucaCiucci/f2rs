


fn inspect_free_source() {
    let raw_source = include_str!("congruent.f90").repeat(1);

    let start_time = std::time::Instant::now();
    let lines = raw_source
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .map(|chars| (TokenizedFreeLine::parse_chars(&chars).unwrap(), chars))
        .enumerate()
        .map(|(number, (content, chars))| Line { number, chars, content })
        .collect::<Vec<_>>();
    let elapsed = start_time.elapsed();
    println!("Number of lines: {} tokenized in {elapsed:?}", lines.len());

    let groups = {
        let mut groups = Vec::new();
        let mut index = 0;

        while index < lines.len() {
            let lines = &lines[index..];
            let count = TokenizedFreeLine::group(lines.iter().map(|line| &line.content));
            assert!(count > 0);
            groups.push(index..index + count);
            index += count;
        }

        groups
    };

    let number_length = lines.len().to_string().len();

    for group in groups {
        let lines = &lines[group];
        for (i, line) in lines.iter().enumerate() {
            let number = line.number.to_string();
            let number = " ".repeat(number_length - number.len()) + &number;

            let sign = if lines.len() > 1 {
                if i == 0 {
                    "┌".bright_black()
                } else if i == lines.len() - 1 {
                    "└".bright_black()
                } else {
                    "│".bright_black()
                }
            } else {
                " ".normal()
            };

            let number = format!("{number}{sign}| ",);
            let content = line.content.dump(
                &line.chars,
                Some(Color::Magenta),
                Some(Color::Green),
                Some(Color::BrightYellow),
                Some(Color::BrightBlue),
                Some(Color::BrightBlack),
                Some(Color::BrightCyan),
                Some(Color::Red),
            );
            println!("{}{}", number, content);
        }
    }

    println!("Number of lines: {} tokenized in {elapsed:?}", lines.len());

    #[derive(Debug, Clone)]
    struct Group {
        lines: Vec<Line>,
    }

    let groups = {
        let mut groups = Vec::new();
        let mut index = 0;

        while index < lines.len() {
            let lines = &lines[index..];
            let count = TokenizedFreeLine::group(lines.iter().map(|line| &line.content));
            assert!(count > 0);
            groups.push(Group {
                lines: lines[..count].to_vec(),
            });
            index += count;
        }

        groups
    };

    println!("Number of groups: {}", groups.len());

    let start_time = std::time::Instant::now();
    for g in groups {
        let line_number = g.lines[0].number;

        let tokens = g.lines
            .into_iter()
            .map(|line| line.content.tokens.map_span(&|span| MultilineSpan::from_line_span(line.number, span)))
            .flatten()
            .collect::<Vec<_>>();

        if let Some(s) = StatementValue::parse(&tokens[..], &Cfg::f2018()) {
            //println!("parsed! line {line_number}");
            assert!(s.as_import().unwrap().data.is_none());
            let s = s.as_import().unwrap().kw.span();
            //println!("module name: {s:?}");
        }
    }
    let elapsed = start_time.elapsed();
    println!("classified_statements in {elapsed:?}");
}
