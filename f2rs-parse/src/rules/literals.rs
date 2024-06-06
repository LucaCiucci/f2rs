

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_digit_string() {
        for cfg in test_configs() {
            let parser = digit_string(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" 1"), false);
            assert_eq!(parser.parses("a"), false);
            assert_eq!(parser.parse("1").unwrap().0.value(), "1");
            assert_eq!(parser.parse("123").unwrap().0.value(), "123");
            assert_eq!(parser.parse("123a").unwrap().0.value(), "123");
            assert_eq!(parser.parse("123 ").unwrap().0.value(), "123");
        }
    }

    #[test]
    fn test_sign() {
        for cfg in test_configs() {
            let parser = sign(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" +"), false);
            assert_eq!(parser.parses(" -"), false);
            assert!(parser.parse("+").unwrap().0.is_plus());
            assert_eq!(parser.parse("+ ").unwrap().0.is_plus(), true);
            assert_eq!(parser.parse("-").unwrap().0.is_minus(), true);
            assert_eq!(parser.parse("- ").unwrap().0.is_minus(), true);
            assert_eq!(parser.parse("+1").unwrap().0.is_plus(), true);
            assert_eq!(parser.parse("-1").unwrap().0.is_minus(), true);
        }
    }

    #[test]
    fn test_signed_digit_string() {
        for cfg in test_configs() {
            let parser = signed_digit_string(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" +"), false);
            assert_eq!(parser.parses(" -"), false);
            assert_eq!(parser.parses(" 1"), false);
            assert_eq!(parser.parses("+ "), false);
            assert_eq!(parser.parses("+ "), false);
            assert_eq!(parser.parses("- "), false);
            assert_eq!(parser.parses("- "), false);
            assert!(parser.parse("+1 ").unwrap().0.sign.unwrap().is_plus());
            assert_eq!(parser.parse("+1 ").unwrap().0.digits.value(), "1");
            assert!(parser.parse("-1 ").unwrap().0.sign.unwrap().is_minus());
            assert_eq!(parser.parse("-1 ").unwrap().0.digits.value(), "1");
            assert!(parser.parse("+1234 ").unwrap().0.sign.unwrap().is_plus());
            assert_eq!(parser.parse("+1234 ").unwrap().0.digits.value(), "1234");
        }
    }

    #[test]
    fn test_kind_param() {
        for cfg in test_configs() {
            let parser = kind_param(&cfg, false);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" 1"), false);
            assert_eq!(parser.parses(" a"), false);
            assert_eq!(parser.parse("1").unwrap().0.as_digit_string().unwrap().value(), "1");
            assert_eq!(parser.parse("123").unwrap().0.as_digit_string().unwrap().value(), "123");
            assert_eq!(parser.parse("123a").unwrap().0.as_digit_string().unwrap().value(), "123");
            assert_eq!(parser.parse("123 ").unwrap().0.as_digit_string().unwrap().value(), "123");
            assert_eq!(parser.parses("+"), false);
            assert_eq!(parser.parse("a").unwrap().0.as_scalar_int_constant_name().unwrap().0.value(), "a");
            assert_eq!(parser.parse("a ").unwrap().0.as_scalar_int_constant_name().unwrap().0.value(), "a");
            assert_eq!(parser.parse("a1").unwrap().0.as_scalar_int_constant_name().unwrap().0.value(), "a1");
            assert_eq!(parser.parse("a1 ").unwrap().0.as_scalar_int_constant_name().unwrap().0.value(), "a1");
        }
    }

    #[test]
    fn test_int_literal_constant() {
        for cfg in test_configs() {
            let parser = int_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses(" 1"), false);
            assert_eq!(parser.parses(" a"), false);
            assert_eq!(parser.parses("a"), false);
            assert_eq!(parser.parses("_"), false);
            assert_eq!(parser.parses("_a"), false);
            assert_eq!(parser.parses("_1"), false);
            assert_eq!(parser.parse("1_").unwrap().0.digits.value(), "1");
            assert!(parser.parse("1_").unwrap().0.kind_param.is_none());
            assert_eq!(parser.parse("1_2").unwrap().0.digits.value(), "1");
            assert_eq!(parser.parse("1_2").unwrap().0.kind_param.unwrap().as_digit_string().unwrap().value(), "2");
            assert_eq!(parser.parse("1_a").unwrap().0.digits.value(), "1");
            assert_eq!(parser.parse("1_a").unwrap().0.kind_param.unwrap().as_scalar_int_constant_name().unwrap().0.value(), "a");
        }
    }

    #[test]
    fn test_signed_int_literal_constant() {
        for cfg in test_configs() {
            let parser = signed_int_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse("1").unwrap().0.sign, None);
            assert!(parser.parse("+1").unwrap().0.sign.unwrap().is_plus());
            assert!(parser.parse("-1").unwrap().0.sign.unwrap().is_minus());
            assert_eq!(parser.parses("+1_a"), true);
        }
    }

    #[test]
    fn examples_in_18_007r1_section_7_4_3_1() {
        for cfg in test_configs() {
            let parser = signed_int_literal_constant(&cfg);
            assert_eq!(parser.parse("473 ").unwrap().1, " ");
            assert_eq!(parser.parse("+56 ").unwrap().1, " ");
            assert_eq!(parser.parse("-101 ").unwrap().1, " ");
            assert_eq!(parser.parse("21_2 ").unwrap().1, " ");
            assert_eq!(parser.parse("21_SHORT ").unwrap().1, " ");
            assert_eq!(parser.parse("1976354279568241_8 ").unwrap().1, " ");
        }
    }

    #[test]
    fn test_exponent_letter() {
        for cfg in test_configs() {
            let parser = exponent_letter(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("a"), false);
            assert_eq!(parser.parses("1"), false);
            assert!(parser.parse("d").unwrap().0.is_d());
            assert!(parser.parse("D").unwrap().0.is_d());
            assert!(parser.parse("e").unwrap().0.is_e());
            assert!(parser.parse("E").unwrap().0.is_e());
        }
    }

    #[test]
    fn test_exponent() {
        for cfg in test_configs() {
            let parser = exponent(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("a"), false);
            assert_eq!(parser.parses("1"), true);
            assert_eq!(parser.parses("d"), false);
            assert_eq!(parser.parses("D"), false);
            assert_eq!(parser.parses("e"), false);
            assert_eq!(parser.parses("E"), false);
            assert_eq!(parser.parse("+123_").unwrap().1, "_");
        }
    }

    #[test]
    fn test_significand() {
        for cfg in test_configs() {
            let parser = significand(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("+1"), false);
            assert_eq!(parser.parses("1"), false);
            assert_eq!(parser.parses("1."), true);
            assert_eq!(parser.parses("+1."), false);
            assert_eq!(parser.parses("1.1"), true);
            assert_eq!(parser.parses(".1"), true);
            assert_eq!(parser.parses("+.1"), false);
        }
    }

    #[test]
    fn test_real_literal_constant() {
        for cfg in test_configs() {
            let parser = real_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("1"), false);
            assert_eq!(parser.parses("1."), true);
            assert_eq!(parser.parses("1.1"), true);
            assert_eq!(parser.parses("1.1e3"), true);
            assert_eq!(parser.parses("1.e3"), true);
            assert_eq!(parser.parses("1.e+3"), true);
            assert_eq!(parser.parses("1.e-3"), true);
            assert_eq!(parser.parses("1.d-3"), true);
            assert_eq!(parser.parses("1.D-3"), true);
            assert_eq!(parser.parses("+1.D-3"), false);
        }
    }

    #[test]
    fn test_signed_real_literal_constant() {
        for cfg in test_configs() {
            let parser = signed_real_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("1"), false);
            assert_eq!(parser.parses("1."), true);
            assert_eq!(parser.parses("1.1"), true);
            assert_eq!(parser.parses("1.1e3"), true);
            assert_eq!(parser.parses("1.e3"), true);
            assert_eq!(parser.parses("1.e+3"), true);
            assert_eq!(parser.parses("1.e-3"), true);
            assert_eq!(parser.parses("1.d-3"), true);
            assert_eq!(parser.parses("1.D-3"), true);
            assert_eq!(parser.parses("+1.D-3"), true);
        }
    }

    #[test]
    fn examples_in_18_007r1_section_7_4_3_2() {
        for cfg in test_configs() {
            let parser = signed_real_literal_constant(&cfg);
            assert_eq!(parser.parse("-12.78 ").unwrap().1, " ");
            assert_eq!(parser.parse("+1.6E3 ").unwrap().1, " ");
            assert_eq!(parser.parse("2.1 ").unwrap().1, " ");
            assert_eq!(parser.parse("-16.E4_8 ").unwrap().1, " ");
            assert_eq!(parser.parse("0.45D-4 ").unwrap().1, " ");
            assert_eq!(parser.parse("10.93E7_QUAD ").unwrap().1, " ");
            assert_eq!(parser.parse(".123 ").unwrap().1, " ");
            assert_eq!(parser.parse("3E4 ").unwrap().1, " ");
        }
    }

    #[test]
    fn test_complex_part() {
        for cfg in test_configs() {
            let parser = complex_part(&cfg);
            assert_eq!(parser.parses(""), false);
            assert!(parser.parse("1").unwrap().0.is_int());
            assert!(parser.parse("+1").unwrap().0.is_int());
            assert!(parser.parse("1_a").unwrap().0.is_int());
            assert!(parser.parse("1.").unwrap().0.is_real());
            assert!(parser.parse("1e3").unwrap().0.is_real());
            assert!(parser.parse("1.1").unwrap().0.is_real());
            assert!(parser.parse("+1.1e3_a").unwrap().0.is_real());
            assert!(parser.parse("a").unwrap().0.is_name());
            assert!(parser.parse("abc").unwrap().0.is_name());
        }
    }

    #[test]
    fn test_complex_literal_constant() {
        for cfg in test_configs() {
            let parser = complex_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("1"), false);
            assert_eq!(parser.parses("()"), false);
            assert_eq!(parser.parses("(a)"), false);
            assert_eq!(parser.parses("(a,b)"), true);
            assert_eq!(parser.parses("(a,b,c)"), false);
            assert_eq!(parser.parses("( a , b )"), true);
            assert_eq!(parser.parses("( +1.3 , b )"), true);
            assert_eq!(parser.parses("( +1.3e-2 , b )"), true);
        }
    }

    #[test]
    fn examples_in_18_007r1_section_7_4_3_3() {
        for cfg in test_configs() {
            let parser = complex_literal_constant(&cfg);
            assert_eq!(parser.parse("(1.0, -1.0) ").unwrap().1, " ");
            assert_eq!(parser.parse("(3, 3.1E6) ").unwrap().1, " ");
            assert_eq!(parser.parse("(4.0_4, 3.6E7_8) ").unwrap().1, " ");
            assert_eq!(parser.parse("( 0., PI) ").unwrap().1, " ");
        }
    }

    #[test]
    fn test_char_literal_constant() {
        for cfg in test_configs() {
            let parser = char_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(r##""ciao" "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##""ci'ao" "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##""ci""ao" "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"'ciao' "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"'ci"ao' "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"'ci''ao' "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"10_"ciao" "##).unwrap().1, " ");
            assert_eq!(parser.parse(r##"abc_"ciao" "##).unwrap().1, " ");
        }
    }

    #[test]
    fn test_logical_literal_constant() {
        for cfg in test_configs() {
            let parser = logical_literal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parse(".true. ").unwrap().1, " ");
            assert_eq!(parser.parse(".true.").unwrap().0.value, true);
            assert_eq!(parser.parse(".TRUE.").unwrap().0.value, true);
            assert_eq!(parser.parse(".TrUe.").unwrap().0.value, true);
            assert_eq!(parser.parse(".false.").unwrap().0.value, false);
            assert_eq!(parser.parse(".FALSE.").unwrap().0.value, false);
            assert_eq!(parser.parse(".FaLsE.").unwrap().0.value, false);
        }
    }

    #[test]
    fn test_hex_digit() {
        for cfg in test_configs() {
            let parser = hex_digit(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("g"), false);
            assert_eq!(parser.parses("G"), false);
            assert_eq!(parser.parses("a"), true);
            assert_eq!(parser.parses("A"), true);
            assert_eq!(parser.parses("f"), true);
            assert_eq!(parser.parses("F"), true);
            assert_eq!(parser.parses("0"), true);
            assert_eq!(parser.parses("9"), true);
        }
    }

    #[test]
    fn test_hex_constant() {
        for cfg in test_configs() {
            let parser = hex_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("z"), false);
            assert_eq!(parser.parses("'1'"), false);
            assert_eq!(parser.parse("z'123abcdef'").unwrap().0.value(), "123abcdef");
            assert_eq!(parser.parse("z\"123abcdef\"").unwrap().0.value(), "123abcdef");
            assert_eq!(parser.parse("Z'123abcDef'").unwrap().0.value(), "123abcDef");
        }
    }

    #[test]
    fn test_octal_constant() {
        for cfg in test_configs() {
            let parser = octal_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("o"), false);
            assert_eq!(parser.parses("'1'"), false);
            assert_eq!(parser.parse("o'1234567'").unwrap().0.value(), "1234567");
            assert_eq!(parser.parse("o\"1234567\"").unwrap().0.value(), "1234567");
            assert_eq!(parser.parse("O'1234567'").unwrap().0.value(), "1234567");
        }
    }

    #[test]
    fn test_binary_constant() {
        for cfg in test_configs() {
            let parser = binary_constant(&cfg);
            assert_eq!(parser.parses(""), false);
            assert_eq!(parser.parses("b"), false);
            assert_eq!(parser.parses("'1'"), false);
            assert_eq!(parser.parse("b'1010101'").unwrap().0.value(), "1010101");
            assert_eq!(parser.parse("b\"1010101\"").unwrap().0.value(), "1010101");
            assert_eq!(parser.parse("B'1010101'").unwrap().0.value(), "1010101");
        }
    }

    #[test]
    fn test_boz_literal_constant() {
        for cfg in test_configs() {
            let parser = boz_literal_constant(&cfg);
            assert!(parser.parse("b'1010101'").unwrap().0.is_binary());
            assert!(parser.parse("o'1234567'").unwrap().0.is_octal());
            assert!(parser.parse("z'123abcdef'").unwrap().0.is_hex());
            assert!(parser.parse("B\"1010101\"").unwrap().0.is_binary());
            assert!(parser.parse("O\"1234567\"").unwrap().0.is_octal());
            assert!(parser.parse("Z\"123abcdef\"").unwrap().0.is_hex());
        }
    }
}