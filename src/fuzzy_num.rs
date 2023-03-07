#![allow(non_snake_case, dead_code)]
use crate::fuzzy_num::fuzzy_unit::FuzzyUnit;
use std::cmp::Ordering::Equal;
use std::fmt;
use std::ops::{Add, Div, Mul, Sub};

pub(crate) mod fuzzy_unit;

/// This structure represents fuzzy number. Representation is of form:
///         Σ μ(x)/x ∀ x ∈ X
///
/// # Fields
///
/// * num - a vector of FuzzyUnits. Each FuzzyUnit is an ordered-pair of
/// membership function value and real data.
///
/// # Examples
/// ```
/// use fuzzylogic::FuzzyNum;
/// let n = FuzzyNum::new(vec![(0.5, 1.0), (1.0, 2.0), (0.5, 4.0)]);
/// ```
#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub struct FuzzyNum<T>
where
    T: PartialOrd,
{
    /// A vector of all individual units. Each unit is a structural pair of
    /// membership function value and real value.
    pub(crate) num: Vec<FuzzyUnit<T>>,
}

impl<T> FuzzyNum<T>
where
    T: PartialOrd + Copy,
{
    /// Returns a new fuzzy number.
    ///
    /// # Arguments
    ///
    /// * `v` - A vector of ordered-pairs of membership function value (μ(x))
    /// and real (x) value.
    ///
    /// # Examples
    ///
    /// ```
    /// use fuzzylogic::FuzzyNum;
    /// // Each tuple is a pair of μ(x) and x.
    /// let number = FuzzyNum::new(vec![
    ///     (0.1, 1.0),
    ///     (0.8, 4.0),
    ///     (0.3, 6.0),
    ///     (0.5, 8.0),
    /// ]);
    ///
    /// ```
    pub fn new(v: Vec<(f32, T)>) -> Self {
        let mut n: Vec<FuzzyUnit<T>> = vec![];
        for (mu, x) in v {
            match FuzzyUnit::new(mu, x) {
                Some(x) => n.push(x),
                None => unreachable!(),
            }
            // n.push(FuzzyUnit::new(mu, x));
        }
        FuzzyNum { num: n }
    }

    /// Returns the length of subvector for which each mu is > 0.
    fn positive_mu_len(&self) -> usize {
        let mut len = 0;
        for s in &self.num {
            if s.mu > 0.0 {
                len = len + 1;
            }
        }
        len
    }

    /// Combines same value fuzzy numbers by taking the maximum of membership
    /// function value for each of them.
    fn rearrange(&mut self) -> Self {
        let mut res: Self = FuzzyNum::new(vec![]);
        self.num.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Equal));

        let mut current_x: T = self.num[0].x;
        let mut new_mu: f32 = 0.0;
        for s in &self.num {
            if current_x != s.x {
                match FuzzyUnit::new(new_mu, current_x) {
                    Some(x) => res.num.push(x),
                    None => unreachable!(),
                };
                // res.num.push(FuzzyUnit::new(new_mu, current_x));
                new_mu = 0.0;
            }
            new_mu = new_mu.max(s.mu);
            current_x = s.x;
        }

        if new_mu != 0.0 {
            match FuzzyUnit::new(new_mu, current_x) {
                Some(x) => res.num.push(x),
                None => unreachable!(),
            };
            // res.num.push(FuzzyUnit::new(new_mu, current_x));
        }

        res
    }
}

impl<T> Add for FuzzyNum<T>
where
    T: PartialOrd + Copy + Add + Add<Output = T>,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut res: Self = Self::new(vec![]);
        for f1 in &self.num {
            for f2 in &rhs.num {
                let mut mu_pair = vec![f1.mu, f2.mu];
                mu_pair.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Equal));
                let mu = mu_pair[0];
                let x = f1.x + f2.x;

                // res.num.push(FuzzyUnit::new(mu, x));
                match FuzzyUnit::new(mu, x) {
                    Some(x) => res.num.push(x),
                    None => unreachable!(),
                };
            }
        }
        res.rearrange()
    }
}

impl<T> Sub for FuzzyNum<T>
where
    T: PartialOrd + Copy + Sub + Sub<Output = T>,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut res: Self = Self::new(vec![]);
        for f1 in &self.num {
            for f2 in &rhs.num {
                let mut mu_pair = vec![f1.mu, f2.mu];
                mu_pair.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Equal));
                let mu = mu_pair[0];
                let x = f1.x - f2.x;

                // res.num.push(FuzzyUnit::new(mu, x));
                match FuzzyUnit::new(mu, x) {
                    Some(x) => res.num.push(x),
                    None => unreachable!(),
                };
            }
        }
        res.rearrange()
    }
}

impl<T> Mul for FuzzyNum<T>
where
    T: PartialOrd + Copy + Mul + Mul<Output = T>,
{
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut res: Self = Self::new(vec![]);
        for f1 in &self.num {
            for f2 in &rhs.num {
                let mut mu_pair = vec![f1.mu, f2.mu];
                mu_pair.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Equal));
                let mu = mu_pair[0];
                let x = f1.x * f2.x;

                // res.num.push(FuzzyUnit::new(mu, x));
                match FuzzyUnit::new(mu, x) {
                    Some(x) => res.num.push(x),
                    None => unreachable!(),
                };
            }
        }
        res.rearrange()
    }
}

impl<T> Div for FuzzyNum<T>
where
    T: PartialOrd + Copy + Div + Div<Output = T>,
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let mut res: Self = Self::new(vec![]);
        for f1 in &self.num {
            for f2 in &rhs.num {
                let mut mu_pair = vec![f1.mu, f2.mu];
                mu_pair.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Equal));
                let mu = mu_pair[0];
                let x = f1.x / f2.x;

                // res.num.push(FuzzyUnit::new(mu, x));
                match FuzzyUnit::new(mu, x) {
                    Some(x) => res.num.push(x),
                    None => unreachable!(),
                };
            }
        }
        res.rearrange()
    }
}

impl<T> fmt::Display for FuzzyNum<T>
where T: PartialOrd + Copy + fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let positive_len = self.positive_mu_len() - 1;
        let mut idx = 0;
        let sep = " + ";

        for s in &self.num {
            if s.to_string().as_str() != "" {
                f.write_str(s.to_string().as_str())?;
                if idx == positive_len {
                    continue;
                }
                f.write_str(sep)?;
                idx = idx + 1;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fill_up_A() -> FuzzyNum<f32> {
        let mut n: FuzzyNum<f32> = FuzzyNum::new(vec![]);
        for i in -10..11 {
            let mu;
            match i {
                1 => mu = 0.1,
                2 => mu = 0.2,
                3 => mu = 0.5,
                5 => mu = 1.0,
                _ => mu = 0.0,
            }
            let singleton = FuzzyUnit::new(mu, i as f32);
            match singleton {
                Some(x) => n.num.push(x),
                None => unreachable!(),
            };
            // n.num.push(singleton);
        }
        n
    }

    fn fill_up_B() -> FuzzyNum<f32> {
        let mut n: FuzzyNum<f32> = FuzzyNum::new(vec![]);
        for i in -10..11 {
            let mu;
            match i {
                2 => mu = 0.1,
                3 => mu = 0.3,
                5 => mu = 0.5,
                6 => mu = 1.0,
                _ => mu = 0.0,
            }
            let singleton = FuzzyUnit::new(mu, i as f32);
            match singleton {
                Some(x) => n.num.push(x),
                None => unreachable!(),
            };
            // n.num.push(singleton);
        }
        n
    }

    #[test]
    fn test_fuzzy_num_add() {
        // 0.1/1 + 0.2/2 + 0.5/3 + 1.0/5
        let A = fill_up_A();
        // 0.1/2 + 0.3/3 + 0.5/5 + 1.0/6
        let B = fill_up_B();

        let C = A + B;
        assert_eq!(
            C.to_string(),
            "0.1/3 + 0.1/4 + 0.2/5 + 0.3/6 + 0.2/7 + 0.5/8 + 0.5/9 + 0.5/10 + 1/11"
        );

        let A = fill_up_A();
        let B = fill_up_B();
        assert_eq!(A.clone() + B.clone(), B.clone() + A.clone());
    }

    #[test]
    fn test_fuzzy_num_sub() {
        let A = fill_up_A();
        let B = fill_up_B();

        let D = A - B;
        assert_eq!(
            D.to_string(),
            "0.1/-5 + 0.2/-4 + 0.5/-3 + 0.5/-2 + 1/-1 + 0.5/0 + 0.1/1 + 0.3/2 + 0.1/3"
        );

        let A = FuzzyNum::new(vec![
            (0.3, 1.0),
            (0.6, 2.0),
            (1.0, 3.0),
            (0.7, 4.0),
            (0.2, 5.0),
        ]);
        let B = FuzzyNum::new(vec![(0.5, 10.0), (1.0, 11.0), (0.5, 12.0)]);

        let E = B - A;
        assert_eq!(
            E.to_string(),
            "0.2/5 + 0.5/6 + 0.7/7 + 1/8 + 0.6/9 + 0.5/10 + 0.3/11"
        );

        let A = FuzzyNum::new(vec![
            (0.3, 1.0),
            (0.6, 2.0),
            (1.0, 3.0),
            (0.7, 4.0),
            (0.2, 5.0),
        ]);
        let B = FuzzyNum::new(vec![(0.5, 10.0), (1.0, 11.0), (0.5, 12.0)]);

        assert_ne!(A.clone() - B.clone(), B.clone() - A.clone());
    }

    #[test]
    fn test_fuzzy_num_mul() {
        let A = FuzzyNum::new(vec![
            (0.3, 1.0),
            (0.6, 2.0),
            (1.0, 3.0),
            (0.7, 4.0),
            (0.2, 5.0),
        ]);
        let B = FuzzyNum::new(vec![(0.5, 10.0), (1.0, 11.0), (0.5, 12.0)]);

        let E = A * B;
        assert_eq!(
            E.to_string(),
            "0.3/10 + 0.3/11 + 0.3/12 + 0.5/20 + 0.6/22 + 0.5/24 + 0.5/30 + 1/33 + 0.5/36 + 0.5/40 + 0.7/44 + 0.5/48 + 0.2/50 + 0.2/55 + 0.2/60"
        );

        let A = FuzzyNum::new(vec![
            (0.3, 1.0),
            (0.6, 2.0),
            (1.0, 3.0),
            (0.7, 4.0),
            (0.2, 5.0),
        ]);
        let B = FuzzyNum::new(vec![(0.5, 10.0), (1.0, 11.0), (0.5, 12.0)]);

        assert_eq!(A.clone() * B.clone(), B.clone() * A.clone());
    }

    #[test]
    fn test_fuzzy_num_div() {
        let A = FuzzyNum::new(vec![(0.3, 2.0), (0.6, 4.0), (1.0, 6.0)]);
        let B = FuzzyNum::new(vec![(0.5, 1.0), (1.0, 2.0), (0.5, 4.0)]);

        let F = A / B;
        assert_eq!(
            F.to_string(),
            "0.3/0.5 + 0.5/1 + 0.5/1.5 + 0.6/2 + 1/3 + 0.5/4 + 0.5/6"
        );

        let A = FuzzyNum::new(vec![(0.3, 2.0), (0.6, 4.0), (1.0, 6.0)]);
        let B = FuzzyNum::new(vec![(0.5, 1.0), (1.0, 2.0), (0.5, 4.0)]);

        let G = B / A;
        assert_ne!(F, G);
    }

    #[test]
    fn test_verify() {
        let A = FuzzyNum::new(vec![(0.3, 2.0), (0.6, 4.0), (1.0, 6.0)]);
        let B = FuzzyNum::new(vec![(0.5, 1.0), (1.0, 2.0), (0.5, 4.0)]);

        let X = A.clone() + B.clone();
        let XSquare = X.clone() * X.clone();
        let A2 = A.clone() * A.clone();
        let B2 = B.clone() * B.clone();
        let AB = A.clone() * B.clone();
        let Fuzzy2 = FuzzyNum::new(vec![(1.0, 2.0)]);
        let DoubleAB = Fuzzy2.clone() * AB.clone();

        assert_ne!(XSquare, A2.clone() + B2.clone() + DoubleAB.clone());
    }
}
