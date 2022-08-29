use itertools::Itertools;
use num_traits::PrimInt;

use std::fmt;
use std::str::FromStr;

/// U4Pair is a standard u8 that contains up to two digits (0-9).
/// The first digit is stored in the high-order 4 bytes; the second
/// is stored in the lower 4 bytes. If there is no second digit,
/// a sentinel value (0xf) is substituted.
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Default)]
struct U4Pair(u8);

impl U4Pair {
    const SENTINEL: u8 = 0xf;
    /// unsafe_new does no sanity checking on its inputs.
    fn unsafe_new(n1: u8, n2: u8) -> Self {
        U4Pair((n1 << 4) + n2)
    }

    #[allow(dead_code)]
    fn new(n1: u8, n2: Option<u8>) -> Self {
        let sentinel = n2.is_none();
        let n2 = n2.unwrap_or(U4Pair::SENTINEL);

        if n1 > 9 || n2 > 9 && !sentinel {
            panic!("both numbers must be single digit")
        }
        U4Pair::unsafe_new(n1, n2)
    }

    /// return the values packed inside the U4Pair as two u8s, the second being an option.
    const fn deconstruct(self) -> (u8, Option<u8>) {
        let n1 = self.0 >> 4;
        let n2 = match self.0 & U4Pair::SENTINEL {
            U4Pair::SENTINEL => None,
            v => Some(v),
        };
        (n1, n2)
    }

    /// Returns the number of digits held in the U4Pair.
    const fn n_digits(self) -> usize {
        match self.0 & U4Pair::SENTINEL {
            U4Pair::SENTINEL => 1,
            _ => 2,
        }
    }

    /// Returns a new U4Pair with the second value removed.
    const fn remove_second(self) -> Self {
        U4Pair(self.0 | U4Pair::SENTINEL)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct Float<W: PrimInt> {
    whole: W,
    frac: Vec<U4Pair>,
    extra_padding: usize,
}

/// convenience function that converts a string of digits to a vec of U4Pairs.
fn encode_u4s(s: &str) -> Result<Vec<U4Pair>, String> {
    s.chars()
        .chunks(2)
        .into_iter()
        .map(|mut ch| {
            let n1c = ch.next().ok_or("Truncated input")?;
            let n2c = ch.next();
            let n1 = n1c.to_digit(10).ok_or("Invalid digit")? as u8;
            let n2 = match n2c {
                Some(n) => n.to_digit(10).ok_or("Invalid digit")? as u8,
                None => U4Pair::SENTINEL,
            };

            Ok(U4Pair::unsafe_new(n1, n2))
        })
        .collect::<Result<Vec<_>, _>>()
}

/// convenience function that converts a vec of U4Pairs to a string of digits.
fn decode_u4s(u: &[U4Pair]) -> String {
    u.iter()
        .map(|pair| {
            let (n1, n2) = pair.deconstruct();
            let mut n1s = n1.to_string();
            if let Some(n2s) = n2 {
                n1s.push_str(&n2s.to_string());
            }
            n1s
        })
        .collect::<String>()
}

impl<W: PrimInt + FromStr> TryFrom<&str> for Float<W> {
    type Error = String;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let splits: Vec<&str> = s.split('.').collect();
        let whole = splits[0].parse().or(Err("Parsing error"))?;
        if splits.len() == 1 {
            let frac = Vec::new();
            return Ok(Float {
                whole,
                frac,
                extra_padding: 0,
            });
        }
        if splits.len() > 2 {
            return Err(String::from("Parsing error"));
        }
        let frac_s = splits[1].trim_end_matches('0');
        let extra_padding = splits[1].len() - frac_s.len();
        let frac = encode_u4s(frac_s)?;
        Ok(Float {
            whole,
            frac,
            extra_padding,
        })
    }
}

impl<W: PrimInt> Float<W> {
    /// Returns the precision of a Float object based on the length of its fractional component.
    pub fn precision(&self) -> usize {
        let l = self.frac.len();
        if l == 0 {
            return 0;
        }
        let ndigits = self.frac[l - 1].n_digits();
        l * 2 - (2 - ndigits) + self.extra_padding
    }

    /// Returns a new Float padded or truncated to the given precision.
    pub fn to_precision(&self, precision: usize) -> Self {
        let p = self.precision();
        if p == precision {
            return self.clone();
        }

        let (new_vec, extra_padding) = {
            if p < precision {
                (self.frac.clone(), precision - p)
            } else {
                let (precision_div, precision_mod) = (precision / 2, precision % 2);
                let newsize = precision_div + precision_mod;
                let mut new_vec = vec![U4Pair::default(); newsize];
                new_vec.clone_from_slice(&self.frac[..newsize]);
                if precision_mod != 0 {
                    new_vec[newsize - 1] = new_vec[newsize - 1].remove_second();
                }
                (new_vec, 0)
            }
        };
        Float {
            whole: self.whole,
            frac: new_vec,
            extra_padding,
        }
    }
}

impl<W: PrimInt + fmt::Display> fmt::Display for Float<W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}.{}{}",
            self.whole,
            decode_u4s(&self.frac),
            "0".repeat(self.extra_padding)
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{Float, U4Pair};

    #[test]
    fn u4new() {
        let u1 = U4Pair::new(3, None);
        assert_eq!(u1.0, (3 << 4) + U4Pair::SENTINEL);
        let u2 = U4Pair::new(3, Some(2));
        assert_eq!(u2.0, (3 << 4) + 2);
    }
    #[test]
    #[should_panic]
    fn invalidu41() {
        let _u1 = U4Pair::new(10, None);
    }
    #[test]
    #[should_panic]
    fn invalidu42() {
        let _u1 = U4Pair::new(3, Some(10));
    }
    #[test]
    fn create_from_string() {
        let f: Float<u32> = "3.14150".try_into().unwrap();
        assert_eq!(f.precision(), 5);
        assert_eq!(f.whole, 3);
        let f: Float<u32> = "3".try_into().unwrap();
        assert_eq!(f.precision(), 0);
        assert_eq!(f.whole, 3);
    }
    #[test]
    fn invalid_string() {
        let f: Result<Float<u32>, String> = "3.14a".try_into();
        assert_eq!(f, Err(String::from("Invalid digit")));
        let f: Result<Float<u32>, String> = "a".try_into();
        assert_eq!(f, Err(String::from("Parsing error")));
        let f: Result<Float<u32>, String> = "5.123.456".try_into();
        assert_eq!(f, Err(String::from("Parsing error")));
    }
    #[test]
    fn modify_precision() {
        let f: Float<u8> = "3.1410000".try_into().unwrap();
        let g: Float<u8> = "3.141".try_into().unwrap();
        let h: Float<u8> = "3.14159".try_into().unwrap();
        assert_ne!(f, g);
        assert_ne!(f, h);
        let f1 = f.to_precision(g.precision());
        assert_eq!(f1, g);
        let g1 = g.to_precision(f.precision());
        assert_eq!(f, g1);
        let h1 = h.to_precision(g.precision());
        assert_eq!(g, h1);
        let f2 = f.to_precision(f.precision());
        assert_eq!(f2, f);
    }
    #[test]
    fn format() {
        let f: Float<u8> = Float::try_from("3.1415").unwrap();
        let s = format!("{}", f);
        assert_eq!(s, String::from("3.1415"));
        let f = f.to_precision(8);
        let s = format!("{}", f);
        assert_eq!(s, String::from("3.14150000"));
        let f = f.to_precision(0);
        let s = format!("{}", f);
        assert_eq!(s, String::from("3."));
    }

    #[test]
    fn deconstruct() {
        let f = U4Pair::new(3, Some(4));
        let (x, y) = f.deconstruct();
        assert_eq!(x, 3);
        assert_eq!(y, Some(4));
        let g = f.remove_second();
        let (x, y) = g.deconstruct();
        assert_eq!(x, 3);
        assert_eq!(y, None);

        let f = U4Pair::new(3, None);
        let (x, y) = f.deconstruct();
        assert_eq!(x, 3);
        assert_eq!(y, None)
    }
}
