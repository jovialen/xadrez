//! Score of the evaluation.

use crate::piece::Side;

/// Evaluation for a chess position.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Evaluation {
    /// Which side the evaluation is relative to.
    pub relative_to: Side,
    /// The score for the current position, relative to
    /// [`Evaluation::relative_to`], in approximate centi-pawns.
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

impl Evaluation {
    /// Get a numeric representation of the evaluation in centi-pawns.
    ///
    /// The result will be relative to [`Evaluation::relative_to`].
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
    /// The result will be relative to [`Evaluation::relative_to`].
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
    pub fn is_mate(&self) -> bool {
        self.prediction.map_or(false, |v| {
            matches!(
                v,
                PositionPrediction::Checkmate
                    | PositionPrediction::MateIn(_)
                    | PositionPrediction::MatedIn(_)
            )
        })
    }

    /// Get the best possible score for the given side.
    #[must_use]
    pub fn max(relative_to: Side) -> Self {
        Self {
            relative_to,
            score: i32::MAX,
            prediction: Some(PositionPrediction::MateIn(1)),
        }
    }

    /// Get the worst possible score for the given side.
    #[must_use]
    pub const fn min(relative_to: Side) -> Self {
        Self {
            relative_to,
            score: -i32::MAX,
            prediction: Some(PositionPrediction::Checkmate),
        }
    }

    pub(crate) fn last(&self) -> Self {
        Self {
            relative_to: !self.relative_to,
            score: -self.score,
            prediction: self.prediction.map(PositionPrediction::last),
        }
    }
}

impl Ord for Evaluation {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        self.centi_pawns().cmp(&rhs.centi_pawns())
    }
}

impl PartialOrd for Evaluation {
    fn partial_cmp(&self, rhs: &Self) -> std::option::Option<std::cmp::Ordering> {
        self.centi_pawns().partial_cmp(&rhs.centi_pawns())
    }
}

impl std::ops::Neg for Evaluation {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.last()
    }
}

impl Default for Evaluation {
    fn default() -> Self {
        Self {
            relative_to: Side::White,
            score: 0,
            prediction: None,
        }
    }
}

impl std::fmt::Display for Evaluation {
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

impl PositionPrediction {
    const fn last(self) -> Self {
        match self {
            Self::MateIn(moves) => Self::MatedIn(moves + 1),
            Self::MatedIn(moves) => Self::MateIn(moves + 1),
            Self::Draw => Self::Draw,
            Self::Checkmate => Self::MateIn(1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_evaluation_last() {
        let eval = Evaluation {
            relative_to: Side::White,
            score: -2000,
            prediction: Some(PositionPrediction::Checkmate),
        };

        let eval = eval.last();
        assert_eq!(
            eval,
            Evaluation {
                relative_to: Side::Black,
                score: 2000,
                prediction: Some(PositionPrediction::MateIn(1)),
            }
        );

        let eval = eval.last();
        assert_eq!(
            eval,
            Evaluation {
                relative_to: Side::White,
                score: -2000,
                prediction: Some(PositionPrediction::MatedIn(2)),
            }
        );

        let eval = eval.last();
        assert_eq!(
            eval,
            Evaluation {
                relative_to: Side::Black,
                score: 2000,
                prediction: Some(PositionPrediction::MateIn(3)),
            }
        );

        let eval = Evaluation {
            relative_to: Side::Black,
            score: 1234,
            prediction: None,
        };

        let eval = eval.last();
        assert_eq!(
            eval,
            Evaluation {
                relative_to: Side::White,
                score: -1234,
                prediction: None,
            }
        );
    }

    #[test]
    fn test_evaluation_numeric() {
        let eval = Evaluation {
            relative_to: Side::White,
            score: 1000,
            prediction: None,
        };
        assert_eq!(eval.centi_pawns(), 1000);
        assert_eq!(eval.centi_pawns(), 1000);

        let eval = Evaluation {
            relative_to: Side::Black,
            score: 1000,
            prediction: None,
        };
        assert_eq!(eval.centi_pawns(), 1000);
        assert_eq!(eval.absolute_centi_pawns(), -1000);

        let eval = Evaluation {
            relative_to: Side::Black,
            score: 1000,
            prediction: Some(PositionPrediction::Checkmate),
        };
        assert_eq!(eval.centi_pawns(), -i32::MAX);
        assert_eq!(eval.absolute_centi_pawns(), i32::MAX);

        let eval = Evaluation {
            relative_to: Side::Black,
            score: 1000,
            prediction: Some(PositionPrediction::MateIn(10)),
        };
        assert_eq!(eval.centi_pawns(), i32::MAX - 10);
        assert_eq!(eval.absolute_centi_pawns(), -i32::MAX + 10);
    }

    #[test]
    fn test_evaluation_ordering() {
        let e1 = Evaluation {
            relative_to: Side::White,
            score: 1000,
            prediction: None,
        };
        let e2 = Evaluation {
            relative_to: Side::White,
            score: 500,
            prediction: None,
        };
        assert!(e1 > e2);
        assert!(e2 < e1);

        let e1 = Evaluation {
            relative_to: Side::White,
            score: 90_000,
            prediction: None,
        };
        let e2 = Evaluation {
            relative_to: Side::White,
            score: 0,
            prediction: Some(PositionPrediction::MateIn(10)),
        };
        assert!(e2 > e1);
        assert!(e1 < e2);

        let e1 = Evaluation {
            relative_to: Side::Black,
            score: 90_000,
            prediction: Some(PositionPrediction::MateIn(5)),
        };
        let e2 = Evaluation {
            relative_to: Side::White,
            score: 0,
            prediction: Some(PositionPrediction::MateIn(10)),
        };
        assert!(e1 > e2);
        assert!(e2 < e1);

        let e1 = Evaluation {
            relative_to: Side::Black,
            score: -12898,
            prediction: None,
        };
        let e2 = Evaluation {
            relative_to: Side::White,
            score: 90_000,
            prediction: Some(PositionPrediction::MatedIn(100)),
        };
        assert!(e1 > e2);
        assert!(e2 < e1);

        let e1 = Evaluation {
            relative_to: Side::Black,
            score: 0,
            prediction: Some(PositionPrediction::MatedIn(2)),
        };
        let e2 = Evaluation {
            relative_to: Side::White,
            score: 0,
            prediction: Some(PositionPrediction::MatedIn(1)),
        };
        assert!(e1 > e2);
        assert!(e2 < e1);
    }
}
