use std::fmt;

/// Unit represents the individual term in a fuzzy number.
#[derive(PartialEq, PartialOrd, Copy, Clone, Debug)]
pub(crate) struct FuzzyUnit {
    pub(crate) x: f32,
    pub(crate) mu: f32,
}

impl FuzzyUnit {
    pub(crate) fn new(mu: f32, x: f32) -> Self {
        FuzzyUnit { x, mu }
    }

    pub(crate) fn get_mu(&self) -> f32 {
        self.mu
    }

    pub(crate) fn get_val(&self) -> f32 {
        self.x
    }
}

impl fmt::Display for FuzzyUnit {
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
        let unit = FuzzyUnit::new(0.3, 5.0);
        assert_eq!(unit.get_val(), 5.0);
        assert_eq!(unit.get_mu(), 0.3);
        assert_eq!(unit.to_string(), "0.3/5");
    }
}
