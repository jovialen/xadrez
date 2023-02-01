use crate::board::Square;
use std::{fmt, ops};

pub(crate) mod constants {
    // Allow for completeness
    #![allow(unused)]

    use super::Bitboard;

    pub(crate) const BITBOARD_ALL: Bitboard = Bitboard(0xFFFF_FFFF_FFFF_FFFF);

    pub(crate) const BITBOARD_FILE_A: Bitboard = Bitboard(0x0101_0101_0101_0101);
    pub(crate) const BITBOARD_FILE_B: Bitboard = Bitboard(0x0202_0202_0202_0202);
    pub(crate) const BITBOARD_FILE_C: Bitboard = Bitboard(0x0404_0404_0404_0404);
    pub(crate) const BITBOARD_FILE_D: Bitboard = Bitboard(0x0808_0808_0808_0808);
    pub(crate) const BITBOARD_FILE_E: Bitboard = Bitboard(0x1010_1010_1010_1010);
    pub(crate) const BITBOARD_FILE_F: Bitboard = Bitboard(0x2020_2020_2020_2020);
    pub(crate) const BITBOARD_FILE_G: Bitboard = Bitboard(0x4040_4040_4040_4040);
    pub(crate) const BITBOARD_FILE_H: Bitboard = Bitboard(0x8080_8080_8080_8080);

    pub(crate) const BITBOARD_RANK_1: Bitboard = Bitboard(0x0000_0000_0000_00FF);
    pub(crate) const BITBOARD_RANK_2: Bitboard = Bitboard(0x0000_0000_0000_FF00);
    pub(crate) const BITBOARD_RANK_3: Bitboard = Bitboard(0x0000_0000_00FF_0000);
    pub(crate) const BITBOARD_RANK_4: Bitboard = Bitboard(0x0000_0000_FF00_0000);
    pub(crate) const BITBOARD_RANK_5: Bitboard = Bitboard(0x0000_00FF_0000_0000);
    pub(crate) const BITBOARD_RANK_6: Bitboard = Bitboard(0x0000_FF00_0000_0000);
    pub(crate) const BITBOARD_RANK_7: Bitboard = Bitboard(0x00FF_0000_0000_0000);
    pub(crate) const BITBOARD_RANK_8: Bitboard = Bitboard(0xFF00_0000_0000_0000);

    pub(crate) const BITBOARD_EDGES: Bitboard = Bitboard(0xFF81_8181_8181_81FF);
    pub(crate) const BITBOARD_INNER: Bitboard = Bitboard(!0xFF81_8181_8181_81FF);
}

#[derive(Clone, Copy, Default, PartialEq)]
pub(crate) struct Bitboard(pub u64);

impl Bitboard {
    #[inline]
    pub fn get<T>(self, index: T) -> bool
    where
        T: Into<usize>,
    {
        (self.0 >> index.into()) & 0b1 != 0
    }

    #[inline]
    pub fn on<T>(&mut self, index: T)
    where
        T: Into<usize>,
    {
        self.0 |= 0b1 << index.into();
    }

    #[inline]
    pub fn off<T>(&mut self, index: T)
    where
        T: Into<usize>,
    {
        self.0 &= !(0b1 << index.into());
    }

    #[allow(clippy::if_not_else)]
    pub fn lsb(self) -> Option<usize> {
        // Using this method of finding the least signifigant bit:
        // https://www.chessprogramming.org/BitScan#With_separated_LS1B

        const DEBRUIJN_BITSCAN_TABLE: [usize; 64] = [
            0, 47, 1, 56, 48, 27, 2, 60, 57, 49, 41, 37, 28, 16, 3, 61, 54, 58, 35, 52, 50, 42, 21,
            44, 38, 32, 29, 23, 17, 11, 4, 62, 46, 55, 26, 59, 40, 36, 15, 53, 34, 51, 20, 43, 31,
            22, 10, 45, 25, 39, 14, 33, 19, 30, 9, 24, 13, 18, 8, 12, 7, 6, 5, 63,
        ];
        const DEBRUIJN: u64 = 0x03f7_9d71_b4cb_0a89;

        if self.0 != 0 {
            let i = (((self.0 ^ (self.0 - 1)).wrapping_mul(DEBRUIJN)) >> 58) as usize;
            let bit_position = *DEBRUIJN_BITSCAN_TABLE.get(i)?;

            Some(bit_position)
        } else {
            None
        }
    }

    pub fn lsb_square(self) -> Option<Square> {
        self.lsb().and_then(|i| Square::try_from(i).ok())
    }

    pub fn pop_lsb(&mut self) -> Option<usize> {
        let lsb = self.lsb()?;
        self.off(lsb);
        Some(lsb)
    }

    pub fn pop_lsb_square(&mut self) -> Option<Square> {
        self.pop_lsb().and_then(|i| Square::try_from(i).ok())
    }

    pub fn pop_count(self) -> usize {
        self.0.count_ones() as usize
    }
}

impl PartialEq<u64> for Bitboard {
    fn eq(&self, other: &u64) -> bool {
        self.0 == *other
    }
}

impl ops::Not for Bitboard {
    type Output = Bitboard;

    fn not(self) -> Self::Output {
        Bitboard(!self.0)
    }
}

impl ops::BitAnd for Bitboard {
    type Output = Bitboard;

    fn bitand(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 & rhs.0)
    }
}

impl<T> ops::BitAnd<T> for Bitboard
where
    u64: ops::BitAnd<T, Output = u64>,
{
    type Output = Bitboard;

    fn bitand(self, rhs: T) -> Self::Output {
        Bitboard(self.0 & rhs)
    }
}

impl<T> ops::BitAndAssign<T> for Bitboard
where
    Self: ops::BitAnd<T, Output = Bitboard>,
{
    fn bitand_assign(&mut self, rhs: T) {
        *self = *self & rhs;
    }
}

impl ops::BitOr for Bitboard {
    type Output = Bitboard;

    fn bitor(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 | rhs.0)
    }
}

impl ops::BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl ops::BitXor for Bitboard {
    type Output = Bitboard;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 ^ rhs.0)
    }
}

impl ops::BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl<T> ops::Shl<T> for Bitboard
where
    u64: ops::Shl<T, Output = u64>,
{
    type Output = Bitboard;

    fn shl(self, rhs: T) -> Self::Output {
        Bitboard(self.0 << rhs)
    }
}

impl<T> ops::ShlAssign<T> for Bitboard
where
    u64: ops::ShlAssign<T>,
{
    fn shl_assign(&mut self, rhs: T) {
        self.0 <<= rhs;
    }
}

impl<T> ops::Shr<T> for Bitboard
where
    u64: ops::Shr<T, Output = u64>,
{
    type Output = Bitboard;

    fn shr(self, rhs: T) -> Self::Output {
        Bitboard(self.0 >> rhs)
    }
}

impl<T> ops::ShrAssign<T> for Bitboard
where
    u64: ops::ShrAssign<T>,
{
    fn shr_assign(&mut self, rhs: T) {
        self.0 >>= rhs;
    }
}

impl ops::Add for Bitboard {
    type Output = Bitboard;

    fn add(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 + rhs.0)
    }
}

impl ops::Sub for Bitboard {
    type Output = Bitboard;

    fn sub(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 - rhs.0)
    }
}

impl ops::SubAssign for Bitboard {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

impl ops::Mul for Bitboard {
    type Output = Bitboard;

    fn mul(self, rhs: Self) -> Self::Output {
        Bitboard(self.0.wrapping_mul(rhs.0))
    }
}

impl ops::MulAssign for Bitboard {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl ops::Div for Bitboard {
    type Output = Bitboard;

    fn div(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 / rhs.0)
    }
}

impl ops::DivAssign for Bitboard {
    fn div_assign(&mut self, rhs: Self) {
        self.0 /= rhs.0;
    }
}

impl fmt::Display for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bytes = self.0.to_be_bytes();
        for byte in bytes {
            writeln!(
                f,
                "{}",
                format!("{byte:#010b}")
                    .chars()
                    .rev()
                    .take(8)
                    .collect::<String>()
            )?;
        }
        Ok(())
    }
}

impl fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bitboard({:#b})", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::constants::BITBOARD_ALL;
    use super::*;
    use crate::board::Square;

    #[test]
    fn set_bits() {
        let mut bitboard = Bitboard(0);
        assert_eq!(bitboard, Bitboard(0));

        bitboard.on(Square::A1);
        assert_eq!(bitboard, Bitboard(0b1));

        bitboard.on(Square::B1);
        assert_eq!(bitboard, Bitboard(0b11));

        bitboard.off(Square::B1);
        assert_eq!(bitboard, Bitboard(0b1));

        bitboard.off(Square::A1);
        bitboard.on(Square::H8);
        assert_eq!(bitboard, Bitboard(0b1u64.rotate_right(1)));
    }

    #[allow(clippy::bool_assert_comparison)]
    #[test]
    #[allow(clippy::bool_assert_comparison)]
    fn get_bits() {
        let bitboard = Bitboard(0b10_0010_0101);

        assert_eq!(bitboard.get(Square::A1), true);
        assert_eq!(bitboard.get(Square::B1), false);
        assert_eq!(bitboard.get(Square::C1), true);
        assert_eq!(bitboard.get(Square::D1), false);
        assert_eq!(bitboard.get(Square::E1), false);
        assert_eq!(bitboard.get(Square::F1), true);
        assert_eq!(bitboard.get(Square::G1), false);
        assert_eq!(bitboard.get(Square::H1), false);
        assert_eq!(bitboard.get(Square::A2), false);
        assert_eq!(bitboard.get(Square::B2), true);
    }

    #[test]
    fn pop_lsb() {
        let mut bitboard = Bitboard(0b1_1100_0100);

        assert_eq!(bitboard.pop_lsb(), Some(2));
        assert_eq!(bitboard, Bitboard(0b1_1100_0000));

        assert_eq!(bitboard.pop_lsb(), Some(6));
        assert_eq!(bitboard, Bitboard(0b1_1000_0000));

        assert_eq!(bitboard.pop_lsb(), Some(7));
        assert_eq!(bitboard, Bitboard(0b1_0000_0000));

        assert_eq!(bitboard.pop_lsb(), Some(8));
        assert_eq!(bitboard, Bitboard(0));

        assert_eq!(bitboard.pop_lsb(), None);

        let mut bitboard = BITBOARD_ALL;
        for i in 0..64 {
            assert_eq!(bitboard.pop_lsb(), Some(i));
        }
        assert_eq!(bitboard.pop_lsb(), None);
    }
}
