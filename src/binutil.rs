use std::ops::Shl;

pub fn mask64<N>(n: N) -> u64 where u64: Shl<N, Output = u64> { !(!0 << n) }
pub fn mask32<N>(n: N) -> u32 where u32: Shl<N, Output = u32> { !(!0 << n) }
pub fn mask16<N>(n: N) -> u16 where u16: Shl<N, Output = u16> { !(!0 << n) }
pub fn bit32(b: bool) -> u32 { if b { 1 } else { 0 } }
