use std::{ops, convert};

pub fn sign_extend<T>(value: T, bit_size: T) -> T
    where T: Clone + ops::Sub<Output=T> + ops::Shl<T, Output=T> + ops::BitAnd<Output=T> + convert::From<i8> {
    let one = T::from(1i8);

    let sign_bit = one.clone() << (bit_size - one.clone());
    (value.clone() & (sign_bit.clone() - one.clone())) - (value & sign_bit)
}

