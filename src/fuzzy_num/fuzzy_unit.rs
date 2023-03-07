// use num::FromPrimitive;
use std::fmt;

/// Unit represents the individual term in a fuzzy number.
///
/// # Fields
///
/// * x - It is a real value stored in the set.
/// * mu - This is a membership function float value in the range [0, 1].
///
/// # Examples
/// ```
/// // use fuzzylogic::FuzzyUnit;
/// // let unit = FuzzyUnit::new(0.1, 2.0); // Some(FuzzyUnit { 2.0, 0.1 })
/// // let try_unit = FuzzyUnit::new(1.1, 2.0); // None
/// ```
#[derive(PartialEq, PartialOrd, Copy, Clone, Debug)]
pub(crate) struct FuzzyUnit<T>
where
    T: PartialOrd,
{
    pub(crate) x: T,
    pub(crate) mu: f32,
}

impl<T> FuzzyUnit<T>
where
    T: PartialOrd + Copy,
{
    pub(crate) fn new(mu: f32, x: T) -> Option<Self> {
        assert!(
            0.0 <= mu && mu <= 1.0,
            "μ should be between 0.0 and 1.0"
        );
        Some(FuzzyUnit { x, mu })
    }

    pub(crate) fn get_mu(&self) -> f32 {
        self.mu
    }

    pub(crate) fn get_val(&self) -> T {
        self.x
    }
}

impl<T> fmt::Display for FuzzyUnit<T>
where
    T: PartialOrd + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.mu > 0.0 {
             write!(f, "{}/{}", self.mu, self.x)
        } else {
            write!(f, "")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_unit() {
        if let Some(unit) = FuzzyUnit::<i32>::new(0.3, 5) {
            assert_eq!(unit.get_val(), 5);
            assert_eq!(unit.get_mu(), 0.3);
            assert_eq!(unit.to_string(), "0.3/5");
        }
    }
}
