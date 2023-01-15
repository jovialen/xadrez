//! Chess move structures.
//!
//! Provides the structures relating to the changes on the chessboard.

use crate::{board::Square, error::ParseLANError, piece::PieceKind};
use std::{fmt, str::FromStr};

/// Structure representing a single move on the chessboard.
///
/// There is no guarantee that the move is legal, so the move should be
/// validated before it is used.
#[derive(Clone, Copy, Debug)]
pub struct Move {
    /// The origin square of the move.
    pub from: Square,
    /// The destination square of the move.
    pub to: Square,
    /// The kind of move that is being made.
    pub(crate) kind: MoveKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum MoveKind {
    // This variant is only used when the type of move is not clear.
    Any,
    Quiet,
    Capture,
    EnPassant,
    // Tecnically, a pawn push is any move where the pawn moves forward. Here, it is used
    // for when the pawn makes the initial double push as its first move.
    PawnPush,
    Promotion(PieceKind),
}

impl Move {
    pub(crate) fn new(from: Square, to: Square, kind: MoveKind) -> Self {
        Self { from, to, kind }
    }
}

impl FromStr for Move {
    type Err = ParseLANError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();

        if s.is_empty() {
            return Err(ParseLANError::Empty);
        }

        if s.len() < 4 || s.len() > 5 {
            return Err(ParseLANError::InvalidSize);
        }

        let (from, to) = s.split_at(2);

        if s.len() == 4 {
            Ok(Self {
                from: Square::from_str(from).or(Err(ParseLANError::InvalidSquare))?,
                to: Square::from_str(to).or(Err(ParseLANError::InvalidSquare))?,
                kind: MoveKind::Any,
            })
        } else {
            Ok(Self {
                from: Square::from_str(from).or(Err(ParseLANError::InvalidSquare))?,
                to: Square::from_str(to).or(Err(ParseLANError::InvalidSquare))?,
                kind: MoveKind::Promotion(
                    PieceKind::try_from(s.chars().nth(4).unwrap())
                        .or(Err(ParseLANError::InvalidPromotion))?,
                ),
            })
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(format!("{}{}", self.from, self.to).to_lowercase().as_str())?;

        match self.kind {
            MoveKind::Promotion(to) => f.write_str(format!("{to}").to_uppercase().as_str()),
            _ => Ok(()),
        }
    }
}

impl PartialEq for Move {
    fn eq(&self, other: &Self) -> bool {
        *self == other.kind && self.from == other.from && self.to == other.to
    }
}

impl PartialEq<MoveKind> for Move {
    #[allow(clippy::match_same_arms)]
    fn eq(&self, other: &MoveKind) -> bool {
        use MoveKind::{Any, Promotion};

        match (self.kind, *other) {
            (Any, Promotion(_)) => false,
            (Promotion(_), Any) => false,
            (Any, _) => true,
            (_, Any) => true,
            (k0, k1) => k0 == k1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod lan {
        use super::*;

        #[test]
        fn normal_move() {
            assert_eq!(
                Move::from_str("e2e4").ok(),
                Some(Move {
                    from: Square::E2,
                    to: Square::E4,
                    kind: MoveKind::Any
                })
            );
        }

        #[test]
        fn untrimmed() {
            assert_eq!(
                Move::from_str("      e2e4 ").ok(),
                Some(Move {
                    from: Square::E2,
                    to: Square::E4,
                    kind: MoveKind::Any
                })
            );
        }

        #[test]
        fn promotion() {
            assert_eq!(
                Move::from_str("h7h8Q").ok(),
                Some(Move {
                    from: Square::H7,
                    to: Square::H8,
                    kind: MoveKind::Promotion(PieceKind::Queen),
                })
            );
        }

        #[test]
        fn to_lan() {
            assert_eq!(
                Move::from_str("e2e4")
                    .expect("Failed to parse LAN")
                    .to_string(),
                "e2e4"
            );
            assert_eq!(
                Move::from_str("g4h4")
                    .expect("Failed to parse LAN")
                    .to_string(),
                "g4h4"
            );
            assert_eq!(
                Move::from_str("b1b5")
                    .expect("Failed to parse LAN")
                    .to_string(),
                "b1b5"
            );
            assert_eq!(
                Move::from_str("c3a1")
                    .expect("Failed to parse LAN")
                    .to_string(),
                "c3a1"
            );

            assert_eq!(
                Move::from_str("c3a1Q")
                    .expect("Failed to parse LAN")
                    .to_string(),
                "c3a1Q"
            );
        }
    }

    mod eq {
        #[allow(clippy::enum_glob_use)]
        use super::{MoveKind::*, *};
        use crate::{
            board::Square::{A1, A2, A3, B1},
            piece::PieceKind::{Bishop, Knight, Queen, Rook},
        };

        #[test]
        fn any_is_all_except_promotion() {
            assert_eq!(Move::new(A1, A2, Any), Move::new(A1, A2, Any));
            assert_eq!(Move::new(A1, A2, Any), Move::new(A1, A2, Quiet));
            assert_eq!(Move::new(A1, A2, Any), Move::new(A1, A2, Capture));
            assert_eq!(Move::new(A1, A2, Any), Move::new(A1, A2, PawnPush));
            assert_eq!(Move::new(A1, A2, Any), Move::new(A1, A2, EnPassant));

            assert_ne!(Move::new(A1, A2, Any), Move::new(A1, A2, Promotion(Queen)));
            assert_ne!(Move::new(A1, A2, Any), Move::new(A1, A2, Promotion(Bishop)));
            assert_ne!(Move::new(A1, A2, Any), Move::new(A1, A2, Promotion(Rook)));
            assert_ne!(Move::new(A1, A2, Any), Move::new(A1, A2, Promotion(Knight)));

            assert_eq!(Move::new(A1, A2, Quiet), Move::new(A1, A2, Any));
            assert_eq!(Move::new(A1, A2, Capture), Move::new(A1, A2, Any));
            assert_eq!(Move::new(A1, A2, PawnPush), Move::new(A1, A2, Any));
            assert_eq!(Move::new(A1, A2, EnPassant), Move::new(A1, A2, Any));

            assert_ne!(Move::new(A1, A2, Promotion(Queen)), Move::new(A1, A2, Any));
            assert_ne!(Move::new(A1, A2, Promotion(Bishop)), Move::new(A1, A2, Any));
            assert_ne!(Move::new(A1, A2, Promotion(Rook)), Move::new(A1, A2, Any));
            assert_ne!(Move::new(A1, A2, Promotion(Knight)), Move::new(A1, A2, Any));
        }

        #[test]
        fn movement_must_match() {
            assert_ne!(Move::new(A1, A2, Any), Move::new(A2, A3, Quiet));
            assert_ne!(Move::new(A1, A2, Any), Move::new(B1, A3, Quiet));
        }

        #[test]
        fn kind_must_match() {
            assert_ne!(Move::new(A1, A2, Quiet), Move::new(A1, A2, Capture));
            assert_ne!(
                Move::new(A1, A2, Quiet),
                Move::new(A1, A2, Promotion(Queen))
            );
            assert_ne!(
                Move::new(A1, A2, Capture),
                Move::new(A1, A2, Promotion(Queen))
            );
            assert_ne!(
                Move::new(A1, A2, Promotion(Knight)),
                Move::new(A1, A2, Promotion(Queen))
            );
        }
    }
}
