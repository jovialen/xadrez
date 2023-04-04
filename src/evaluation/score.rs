//! Score of the evaluation.

use crate::piece::Side;

/// Evaluation for a chess position.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Score {
    /// Which side the evaluation is relative to.
    pub relative_to: Side,
    /// The score for the current position, relative to
    /// [`Score::relative_to`], in approximate centi-pawns.
    pub score: i32,
    /// How the position will develop.
    pub prediction: Option<PositionPrediction>,
}

/// Prediction on how the position will develop.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionPrediction {
    /// Will set the opponent in checkmate in the given amout of moves.
    MateIn(usize),
    /// Will be put in checkmate in the given amount of moves.
    MatedIn(usize),
    /// Is, or will be, a draw.
    Draw,
    /// Checkmate.
    Checkmate,
}

impl Score {
    /// Get a numeric representation of the evaluation in centi-pawns.
    ///
    /// The result will be relative to [`Score::relative_to`].
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    #[must_use]
    pub const fn centi_pawns(&self) -> i32 {
        match self.prediction {
            Some(PositionPrediction::Checkmate) => -i32::MAX,
            Some(PositionPrediction::Draw) => 0,
            Some(PositionPrediction::MateIn(moves)) => i32::MAX - moves as i32,
            Some(PositionPrediction::MatedIn(moves)) => -i32::MAX + moves as i32,
            None => self.score,
        }
    }

    /// Get a numeric representation of the evaluation relative to
    /// [`Side::White`] in centi-pawns.
    #[must_use]
    pub const fn absolute_centi_pawns(&self) -> i32 {
        match self.relative_to {
            Side::White => self.centi_pawns(),
            Side::Black => -self.centi_pawns(),
        }
    }

    /// Get a numeric representation of the evalution in pawns.
    ///
    /// The result will be relative to [`Score::relative_to`].
    #[must_use]
    pub fn pawns(&self) -> f64 {
        f64::from(self.centi_pawns()) / 100.0
    }

    /// Get a numeric representation of the evaluation relative to
    /// [`Side::White`] in pawns.
    #[must_use]
    pub fn absolute_pawns(&self) -> f64 {
        f64::from(self.absolute_centi_pawns()) / 100.0
    }

    /// Check if the evaluation results in checkmate.
    #[must_use]
    pub const fn is_mate(&self) -> bool {
        if let Some(prediction) = self.prediction {
            matches!(
                prediction,
                PositionPrediction::Checkmate
                    | PositionPrediction::MateIn(_)
                    | PositionPrediction::MatedIn(_)
            )
        } else {
            false
        }
    }

    #[must_use]
    pub(crate) const fn max(relative_to: Side) -> Self {
        Self {
            relative_to,
            score: i32::MAX,
            prediction: Some(PositionPrediction::MateIn(1)),
        }
    }

    #[must_use]
    pub(crate) const fn min(relative_to: Side) -> Self {
        Self {
            relative_to,
            score: -i32::MAX,
            prediction: Some(PositionPrediction::Checkmate),
        }
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    pub(crate) const fn mate_in(relative_to: Side, moves: usize) -> Self {
        Self {
            relative_to,
            score: i32::MAX - moves as i32,
            prediction: Some(PositionPrediction::MateIn(moves)),
        }
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    pub(crate) const fn mated_in(relative_to: Side, moves: usize) -> Self {
        Self {
            relative_to,
            score: -i32::MAX + moves as i32,
            prediction: Some(PositionPrediction::MatedIn(moves)),
        }
    }

    pub(crate) const fn equal() -> Self {
        Self {
            relative_to: Side::White,
            score: 0,
            prediction: None,
        }
    }
}

impl Ord for Score {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        assert_eq!(self.relative_to, rhs.relative_to);
        self.centi_pawns().cmp(&rhs.centi_pawns())
    }
}

impl PartialOrd for Score {
    fn partial_cmp(&self, rhs: &Self) -> std::option::Option<std::cmp::Ordering> {
        assert_eq!(self.relative_to, rhs.relative_to);
        self.centi_pawns().partial_cmp(&rhs.centi_pawns())
    }
}

impl std::ops::Neg for Score {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            relative_to: !self.relative_to,
            score: -self.score,
            prediction: self.prediction.map(|v| -v),
        }
    }
}

impl std::ops::Add<i32> for Score {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        Self {
            score: self.score + rhs,
            prediction: self.prediction.and_then(|v| v + rhs),
            ..self
        }
    }
}

impl std::ops::Sub<i32> for Score {
    type Output = Self;

    fn sub(self, rhs: i32) -> Self::Output {
        Self {
            score: self.score - rhs,
            prediction: self.prediction.and_then(|v| v - rhs),
            ..self
        }
    }
}

impl Default for Score {
    fn default() -> Self {
        Self {
            relative_to: Side::White,
            score: 0,
            prediction: None,
        }
    }
}

impl std::fmt::Display for Score {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self.prediction {
            Some(PositionPrediction::Checkmate) => write!(f, "checkmate"),
            Some(PositionPrediction::Draw) => write!(f, "draw"),
            Some(PositionPrediction::MateIn(moves)) => write!(f, "win in {moves}"),
            Some(PositionPrediction::MatedIn(moves)) => write!(f, "lose in {moves}"),
            None => write!(f, "{}", self.absolute_pawns()),
        }
    }
}

impl std::ops::Neg for PositionPrediction {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::MateIn(0) => Self::Checkmate,
            Self::MateIn(moves) => Self::MatedIn(moves),
            Self::MatedIn(moves) => Self::MateIn(moves),
            Self::Draw => Self::Draw,
            Self::Checkmate => Self::MateIn(0),
        }
    }
}

impl std::ops::Add<i32> for PositionPrediction {
    type Output = Option<Self>;

    #[allow(clippy::cast_sign_loss)]
    fn add(self, rhs: i32) -> Self::Output {
        if rhs < 0 {
            self - rhs
        } else {
            match self {
                Self::MateIn(moves) => {
                    Some(Self::MateIn(moves.checked_sub(rhs as usize).unwrap_or(1)))
                }
                Self::MatedIn(moves) => Some(Self::MatedIn(moves + rhs as usize)),
                Self::Checkmate if rhs > 0 => Some(Self::MatedIn(rhs as usize)),
                Self::Checkmate => Some(Self::Checkmate),
                Self::Draw => None,
            }
        }
    }
}

impl std::ops::Sub<i32> for PositionPrediction {
    type Output = Option<Self>;

    #[allow(
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap
    )]
    fn sub(self, rhs: i32) -> Self::Output {
        if rhs < 0 {
            self + rhs
        } else {
            match self {
                Self::MateIn(moves) => Some(Self::MateIn(moves + rhs as usize)),
                Self::MatedIn(moves) if moves as i32 > rhs => {
                    Some(Self::MatedIn((moves as i32 - rhs) as usize))
                }
                Self::MatedIn(_) | PositionPrediction::Checkmate => Some(Self::Checkmate),
                Self::Draw => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_score_numeric() {
        let s = Score {
            relative_to: Side::White,
            score: 1000,
            prediction: None,
        };
        assert_eq!(s.centi_pawns(), 1000);
        assert_eq!(s.centi_pawns(), 1000);

        let s = Score {
            relative_to: Side::Black,
            score: 1000,
            prediction: None,
        };
        assert_eq!(s.centi_pawns(), 1000);
        assert_eq!(s.absolute_centi_pawns(), -1000);

        let s = Score {
            relative_to: Side::Black,
            score: 1000,
            prediction: Some(PositionPrediction::Checkmate),
        };
        assert_eq!(s.centi_pawns(), -i32::MAX);
        assert_eq!(s.absolute_centi_pawns(), i32::MAX);

        let s = Score {
            relative_to: Side::Black,
            score: 1000,
            prediction: Some(PositionPrediction::MateIn(10)),
        };
        assert_eq!(s.centi_pawns(), i32::MAX - 10);
        assert_eq!(s.absolute_centi_pawns(), -i32::MAX + 10);
    }

    #[test]
    fn test_score_ordering() {
        let s1 = Score {
            relative_to: Side::White,
            score: 1000,
            prediction: None,
        };
        let s2 = Score {
            relative_to: Side::White,
            score: 500,
            prediction: None,
        };
        assert!(s1 > s2);
        assert!(s2 < s1);

        let s1 = Score {
            relative_to: Side::White,
            score: 90_000,
            prediction: None,
        };
        let s2 = Score {
            relative_to: Side::White,
            score: 0,
            prediction: Some(PositionPrediction::MateIn(10)),
        };
        assert!(s2 > s1);
        assert!(s1 < s2);

        let s1 = Score {
            relative_to: Side::Black,
            score: 90_000,
            prediction: Some(PositionPrediction::MateIn(5)),
        };
        let s2 = Score {
            relative_to: Side::Black,
            score: 0,
            prediction: Some(PositionPrediction::MateIn(10)),
        };
        assert!(s1 > s2);
        assert!(s2 < s1);

        let s1 = Score {
            relative_to: Side::Black,
            score: -12898,
            prediction: None,
        };
        let s2 = Score {
            relative_to: Side::Black,
            score: 90_000,
            prediction: Some(PositionPrediction::MatedIn(100)),
        };
        assert!(s1 > s2);
        assert!(s2 < s1);

        let s1 = Score {
            relative_to: Side::White,
            score: 0,
            prediction: Some(PositionPrediction::MatedIn(2)),
        };
        let s2 = Score {
            relative_to: Side::White,
            score: 0,
            prediction: Some(PositionPrediction::MatedIn(1)),
        };
        assert!(s1 > s2);
        assert!(s2 < s1);
        assert!(-s1 < -s2);
        assert!(-s2 > -s1);
    }
}
