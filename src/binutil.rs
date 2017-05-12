pub fn mask(n: u32) -> u32 { !(!0 << n) }
pub fn bit(b: bool) -> u32 { if b { 1 } else { 0 } }
