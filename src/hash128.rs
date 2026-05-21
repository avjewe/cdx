//! `HashSet` with 128 bit hash used in place of &[u8]

/// `Hasher` than hashes 128 bit values into 64 bit values
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct Hasher128 {
    state: u64,
}

impl std::hash::Hasher for Hasher128 {
    // This is where you write your custom math/logic.
    // Since we know we are only hashing u128, we can use write_u128 directly.
    fn write_u128(&mut self, i: u128) {
        self.state = i as u64;
    }

    // Required by the trait, but won't be used if we only call write_u128.
    // You can just delegate it or leave it dummy.
    fn write(&mut self, _bytes: &[u8]) {}

    // Return the final 64-bit hash result
    fn finish(&self) -> u64 {
        self.state
    }
}

/// `BuildHasher` for `Hasher128`
#[derive(Default, Clone, Copy, Debug)]
pub struct Hasher128Builder;

impl std::hash::BuildHasher for Hasher128Builder {
    type Hasher = Hasher128;

    fn build_hasher(&self) -> Self::Hasher {
        // Initialize your hasher with a starting state/seed
        Hasher128 { state: 0 }
    }
}

/// `HashSet` of 128 bit hashes using `Hasher128`
pub type HashSet128 = std::collections::HashSet<u128, Hasher128Builder>;

/// Create new `HashSet128`
#[must_use]
pub const fn new_hash_set() -> HashSet128 {
    std::collections::HashSet::with_hasher(Hasher128Builder)
}

/// insert hash of value
pub fn insert(set: &mut HashSet128, value: &[u8]) -> bool {
    let value = twox_hash::xxhash3_128::Hasher::oneshot(value);
    set.insert(value)
}
