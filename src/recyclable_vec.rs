//! `RecyclableVec<T>` is a vector-like collection that keeps previously active
//! elements initialized after logical shrink operations.
//!
//! This is useful for values such as `String` and `Vec<U>` that repeatedly grow
//! and shrink. Calling [`clear`](RecyclableVec::clear) or
//! [`truncate`](RecyclableVec::truncate) only changes the active length, so later
//! growth can reuse the inactive values and their internal allocations.
//!
//! ```
//! use cdx::recyclable_vec::{Recycle, RecyclableVec};
//!
//! let mut values = RecyclableVec::new();
//! values.push(String::from("large allocation"));
//! let ptr = values[0].as_ptr();
//!
//! values.clear();
//! let slot = values.push_recycled();
//! slot.push_str("reused");
//!
//! assert_eq!(values.active(), ["reused"]);
//! assert_eq!(values[0].as_ptr(), ptr);
//! ```
//!
//! You can also create one with [`rvec!`], which mirrors the standard
//! `vec!` macro:
//!
//! ```
//! use cdx::rvec;
//!
//! let values = rvec![1, 2, 3];
//! assert_eq!(values.active(), &[1, 2, 3]);
//!
//! let repeated = rvec!["x".to_string(); 2];
//! assert_eq!(repeated.active(), ["x", "x"]);
//! ```
//!
//! Most read-only `Vec` behavior comes through `Deref<Target = [T]>`, and the
//! crate implements the usual iterator, indexing, collection, comparison,
//! hashing, and slice conversion traits.
//!
//! # Behavior model
//!
//! `RecyclableVec` has three related sizes:
//!
//! | Region | Meaning |
//! | --- | --- |
//! | active | Logically present elements, returned by [`active`](RecyclableVec::active). |
//! | inactive | Initialized reusable storage, returned by [`inactive`](RecyclableVec::inactive). |
//! | capacity | Raw backing `Vec` capacity, returned by [`capacity`](RecyclableVec::capacity). |
//!
//! Mutation methods are recycling-first. Use methods such as
//! [`clear`](RecyclableVec::clear), [`truncate`](RecyclableVec::truncate),
//! [`recycle_pop`](RecyclableVec::recycle_pop),
//! [`recycle_remove`](RecyclableVec::recycle_remove),
//! [`recycle_swap_remove`](RecyclableVec::recycle_swap_remove),
//! [`recycle_drain`](RecyclableVec::recycle_drain), and
//! [`recycle_split_off`](RecyclableVec::recycle_split_off) when you want removed
//! active elements to remain initialized for later reuse.
//!
//! When you really need ownership of removed elements, use the explicit `take_*`
//! methods: [`take_pop`](RecyclableVec::take_pop),
//! [`take_remove`](RecyclableVec::take_remove),
//! [`take_swap_remove`](RecyclableVec::take_swap_remove),
//! [`take_drain`](RecyclableVec::take_drain), and
//! [`take_split_off`](RecyclableVec::take_split_off). These remove values from
//! the recycling pool because ownership is transferred to the caller.
//!
//! | Method family | Keeps initialized values for reuse? | Transfers ownership out? |
//! | --- | --- | --- |
//! | `clear`, `truncate`, `recycle_*` | yes | no |
//! | `take_*` | no | yes |
//!
//! # Ensuring reusable values
//!
//! ```
//! use cdx::recyclable_vec::RecyclableVec;
//!
//! let mut rows = RecyclableVec::<Vec<u8>>::with_recycled_capacity(2);
//! rows.ensure_active_len(2);
//! rows[0].extend_from_slice(b"abc");
//! rows[1].extend_from_slice(b"def");
//!
//! rows.recycle_active();
//! rows.ensure_active_len(1);
//! rows[0].extend_from_slice(b"reused");
//!
//! assert_eq!(rows.active(), [b"reused".to_vec()]);
//! assert_eq!(rows.storage_len(), 2);
//! ```
//!
use std::borrow::{Borrow, BorrowMut};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut, Index, IndexMut, RangeBounds};
use std::slice;

/// Creates a [`RecyclableVec`] containing the given elements.
///
/// This mirrors the standard `vec!` macro:
///
/// ```
/// use cdx::rvec;
///
/// let empty: cdx::recyclable_vec::RecyclableVec<i32> = rvec![];
/// assert!(empty.is_empty());
///
/// let values = rvec![1, 2, 3];
/// assert_eq!(values.active(), &[1, 2, 3]);
///
/// let repeated = rvec!["x".to_string(); 2];
/// assert_eq!(repeated.active(), ["x", "x"]);
/// ```
#[macro_export]
macro_rules! rvec {
    () => {
        $crate::recyclable_vec::RecyclableVec::new()
    };
    ($elem:expr; $n:expr) => {
        $crate::recyclable_vec::RecyclableVec::from_vec(::std::vec![$elem; $n])
    };
    ($($element:expr),+ $(,)?) => {
        $crate::recyclable_vec::RecyclableVec::from_vec(::std::vec![$($element),+])
    };
}

/// Resets a value so it can be reused without giving up its allocation.
pub trait Recycle {
    /// Resets this value into an empty reusable state.
    fn recycle(&mut self);
}

impl Recycle for String {
    fn recycle(&mut self) {
        self.clear();
    }
}

impl<T> Recycle for Vec<T> {
    fn recycle(&mut self) {
        self.clear();
    }
}

/// A vector-like collection that keeps previously used elements around.
///
/// `RecyclableVec` exposes only the first `active_len` elements as logically
/// present. Elements past that point remain allocated and initialized so later
/// growth can reuse their internal allocations.
///
/// This is most useful for values like `String` and `Vec<U>` that can be
/// cleared and refilled many times.
#[derive(Clone)]
pub struct RecyclableVec<T> {
    // Invariant: active_len <= storage.len().
    storage: Vec<T>,
    active_len: usize,
}

impl<T> RecyclableVec<T> {
    fn debug_assert_invariant(&self) {
        debug_assert!(
            self.active_len <= self.storage.len(),
            "active_len ({}) must be <= storage.len() ({})",
            self.active_len,
            self.storage.len()
        );
    }

    /// Creates an empty `RecyclableVec`.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            storage: Vec::new(),
            active_len: 0,
        }
    }

    /// Creates an empty `RecyclableVec` with at least the given storage capacity.
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            storage: Vec::with_capacity(capacity),
            active_len: 0,
        }
    }

    /// Creates an empty `RecyclableVec` with `len` inactive initialized slots.
    #[must_use]
    pub fn with_storage_len(len: usize) -> Self
    where
        T: Default,
    {
        let storage = std::iter::repeat_with(T::default).take(len).collect();
        Self {
            storage,
            active_len: 0,
        }
    }

    /// Creates an empty `RecyclableVec` with `capacity` inactive initialized slots.
    #[must_use]
    pub fn with_recycled_capacity(capacity: usize) -> Self
    where
        T: Default,
    {
        Self::with_storage_len(capacity)
    }

    /// Wraps a `Vec`, making all of its elements active.
    #[must_use]
    pub const fn from_vec(storage: Vec<T>) -> Self {
        let active_len = storage.len();
        Self {
            storage,
            active_len,
        }
    }

    /// Returns the active length.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.active_len
    }

    /// Returns the active length.
    #[must_use]
    pub const fn active_len(&self) -> usize {
        self.active_len
    }

    /// Returns `true` if there are no active elements.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.active_len == 0
    }

    /// Returns the capacity of the backing `Vec`.
    #[must_use]
    pub const fn capacity(&self) -> usize {
        self.storage.capacity()
    }

    /// Returns the number of initialized storage slots, active and inactive.
    #[must_use]
    pub const fn stored_len(&self) -> usize {
        self.storage.len()
    }

    /// Returns the number of initialized storage slots, active and inactive.
    #[must_use]
    pub const fn storage_len(&self) -> usize {
        self.storage.len()
    }

    /// Returns the number of inactive initialized elements available for reuse.
    #[must_use]
    pub const fn recyclable_len(&self) -> usize {
        self.storage.len() - self.active_len
    }

    /// Returns the number of inactive initialized elements available for reuse.
    #[must_use]
    pub const fn inactive_len(&self) -> usize {
        self.storage.len() - self.active_len
    }

    /// Returns all initialized storage, including inactive elements.
    #[must_use]
    pub fn storage(&self) -> &[T] {
        &self.storage
    }

    /// Returns all initialized storage, including inactive elements.
    pub fn storage_mut(&mut self) -> &mut [T] {
        &mut self.storage
    }

    /// Returns the active elements as a slice.
    #[must_use]
    pub fn as_slice(&self) -> &[T] {
        &self.storage[..self.active_len]
    }

    /// Returns the active elements as a slice.
    #[must_use]
    pub fn active(&self) -> &[T] {
        self.as_slice()
    }

    /// Returns the active elements as a mutable slice.
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.storage[..self.active_len]
    }

    /// Returns the active elements as a mutable slice.
    pub fn active_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }

    /// Returns the inactive initialized elements available for reuse.
    #[must_use]
    pub fn inactive(&self) -> &[T] {
        &self.storage[self.active_len..]
    }

    /// Returns the inactive initialized elements available for reuse.
    pub fn inactive_mut(&mut self) -> &mut [T] {
        &mut self.storage[self.active_len..]
    }

    /// Returns an iterator over the active elements.
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.as_slice().iter()
    }

    /// Returns a mutable iterator over the active elements.
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, T> {
        self.as_mut_slice().iter_mut()
    }

    /// Appends an element.
    ///
    /// If an inactive element exists, it is replaced. Use [`push_with_recycle`]
    /// when you want to reuse the inactive element's internal allocation.
    ///
    /// [`push_with_recycle`]: Self::push_with_recycle
    pub fn push(&mut self, value: T) {
        if self.active_len == self.storage.len() {
            self.storage.push(value);
        } else {
            self.storage[self.active_len] = value;
        }
        self.active_len += 1;
        self.debug_assert_invariant();
    }

    /// Appends an element by reusing an inactive slot when possible.
    ///
    /// `recycle` receives an inactive element and should reset it into the
    /// desired active value. `create` is called only when no inactive slot is
    /// available.
    pub fn push_with_recycle(
        &mut self,
        recycle: impl FnOnce(&mut T),
        create: impl FnOnce() -> T,
    ) -> &mut T {
        if self.active_len == self.storage.len() {
            self.storage.push(create());
        } else {
            recycle(&mut self.storage[self.active_len]);
        }

        let index = self.active_len;
        self.active_len += 1;
        self.debug_assert_invariant();
        &mut self.storage[index]
    }

    /// Appends a recycled element, creating a default value when no inactive
    /// slot is available.
    pub fn push_recycled(&mut self) -> &mut T
    where
        T: Default + Recycle,
    {
        self.push_with_recycle(Recycle::recycle, T::default)
    }

    /// Appends a recycled element and then configures it with `update`.
    pub fn push_recycled_with(&mut self, update: impl FnOnce(&mut T)) -> &mut T
    where
        T: Default + Recycle,
    {
        let value = self.push_recycled();
        update(value);
        value
    }

    /// Appends a default element or reuses an inactive one after clearing it.
    pub fn push_default(&mut self) -> &mut T
    where
        T: Clone + Default,
    {
        self.push_with_recycle(|value| value.clone_from(&T::default()), T::default)
    }

    fn push_cloned(&mut self, value: &T)
    where
        T: Clone,
    {
        if self.active_len == self.storage.len() {
            self.storage.push(value.clone());
        } else {
            self.storage[self.active_len].clone_from(value);
        }
        self.active_len += 1;
        self.debug_assert_invariant();
    }

    /// Clears active elements while retaining them for reuse.
    pub const fn clear(&mut self) {
        self.active_len = 0;
    }

    /// Shortens the active length while retaining inactive elements for reuse.
    pub fn truncate(&mut self, len: usize) {
        self.active_len = self.active_len.min(len);
        self.debug_assert_invariant();
    }

    /// Grows or shrinks the active length, cloning `value` for new slots.
    ///
    /// Inactive slots are overwritten with [`Clone::clone_from`] so their
    /// internal allocations can be reused.
    ///
    /// [`resize_with_recycle`]: Self::resize_with_recycle
    pub fn resize(&mut self, new_len: usize, value: T)
    where
        T: Clone,
    {
        if new_len <= self.active_len {
            self.active_len = new_len;
            self.debug_assert_invariant();
            return;
        }

        while self.active_len < new_len {
            self.push_cloned(&value);
        }
        self.debug_assert_invariant();
    }

    /// Grows or shrinks the active length, creating new elements as needed.
    pub fn resize_with(&mut self, new_len: usize, mut create: impl FnMut() -> T) {
        if new_len <= self.active_len {
            self.active_len = new_len;
            self.debug_assert_invariant();
            return;
        }

        while self.active_len < new_len {
            self.push(create());
        }
        self.debug_assert_invariant();
    }

    /// Grows or shrinks the active length using [`Recycle`] for inactive slots.
    pub fn resize_recycled(&mut self, new_len: usize)
    where
        T: Default + Recycle,
    {
        self.resize_with_recycle(new_len, Recycle::recycle, T::default);
    }

    /// Ensures that at least `len` elements are active.
    ///
    /// If the vector is already at least this long, this method does nothing.
    pub fn ensure_active_len(&mut self, len: usize)
    where
        T: Default + Recycle,
    {
        if self.active_len < len {
            self.resize_recycled(len);
        }
    }

    /// Ensures that at least `len` elements are active, configuring newly active
    /// recycled elements with `update`.
    pub fn ensure_active_len_with(&mut self, len: usize, mut update: impl FnMut(&mut T))
    where
        T: Default + Recycle,
    {
        let old_len = self.active_len;
        self.ensure_active_len(len);
        for value in &mut self.storage[old_len..self.active_len] {
            update(value);
        }
    }

    /// Grows or shrinks the active length while reusing inactive elements.
    pub fn resize_with_recycle(
        &mut self,
        new_len: usize,
        mut recycle: impl FnMut(&mut T),
        mut create: impl FnMut() -> T,
    ) {
        if new_len <= self.active_len {
            self.active_len = new_len;
            self.debug_assert_invariant();
            return;
        }

        while self.active_len < new_len {
            self.push_with_recycle(&mut recycle, &mut create);
        }
        self.debug_assert_invariant();
    }

    /// Recycles all active elements and then marks them inactive.
    pub fn recycle_active(&mut self)
    where
        T: Recycle,
    {
        for value in self.as_mut_slice() {
            value.recycle();
        }
        self.active_len = 0;
        self.debug_assert_invariant();
    }

    /// Reserves capacity for at least `additional` more active elements.
    pub fn reserve(&mut self, additional: usize) {
        self.storage.reserve(additional);
    }

    /// Reserves enough backing capacity for `additional` more active elements,
    /// accounting for already-initialized inactive slots.
    pub fn reserve_active(&mut self, additional: usize) {
        let target = self
            .active_len
            .checked_add(additional)
            .expect("capacity overflow");
        if target > self.storage.capacity() {
            self.storage.reserve(target - self.storage.len());
        }
    }

    /// Reserves the minimum capacity for at least `additional` more active elements.
    pub fn reserve_exact(&mut self, additional: usize) {
        self.storage.reserve_exact(additional);
    }

    /// Reserves the minimum backing capacity for `additional` more active
    /// elements, accounting for already-initialized inactive slots.
    pub fn reserve_active_exact(&mut self, additional: usize) {
        let target = self
            .active_len
            .checked_add(additional)
            .expect("capacity overflow");
        if target > self.storage.capacity() {
            self.storage.reserve_exact(target - self.storage.len());
        }
    }

    /// Shrinks the backing capacity as much as possible.
    pub fn shrink_to_fit(&mut self) {
        self.storage.truncate(self.active_len);
        self.storage.shrink_to_fit();
        self.debug_assert_invariant();
    }

    /// Shrinks initialized storage and backing capacity to at least `min_capacity`.
    ///
    /// Active elements are never discarded, so the effective minimum is
    /// `min_capacity.max(self.len())`.
    pub fn shrink_to(&mut self, min_capacity: usize) {
        let min_capacity = min_capacity.max(self.active_len);
        self.storage.truncate(min_capacity);
        self.storage.shrink_to(min_capacity);
        self.debug_assert_invariant();
    }

    fn active_range_bounds<R>(&self, range: R, operation: &str) -> (usize, usize)
    where
        R: RangeBounds<usize>,
    {
        let start = match range.start_bound() {
            std::ops::Bound::Included(&start) => start,
            std::ops::Bound::Excluded(&start) => start + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            std::ops::Bound::Included(&end) => end + 1,
            std::ops::Bound::Excluded(&end) => end,
            std::ops::Bound::Unbounded => self.active_len,
        };

        assert!(
            start <= end && end <= self.active_len,
            "{operation} range {start}..{end} out of bounds for len {}",
            self.active_len
        );
        (start, end)
    }

    /// Returns a reference to an active element.
    pub fn get<I>(&self, index: I) -> Option<&I::Output>
    where
        I: slice::SliceIndex<[T]>,
    {
        self.as_slice().get(index)
    }

    /// Returns a mutable reference to an active element.
    pub fn get_mut<I>(&mut self, index: I) -> Option<&mut I::Output>
    where
        I: slice::SliceIndex<[T]>,
    {
        self.as_mut_slice().get_mut(index)
    }

    /// Returns the first active element.
    #[must_use]
    pub fn first(&self) -> Option<&T> {
        self.as_slice().first()
    }

    /// Returns the first active element mutably.
    pub fn first_mut(&mut self) -> Option<&mut T> {
        self.as_mut_slice().first_mut()
    }

    /// Returns the last active element.
    #[must_use]
    pub fn last(&self) -> Option<&T> {
        self.as_slice().last()
    }

    /// Returns the last active element mutably.
    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.as_mut_slice().last_mut()
    }

    /// Inserts an element at `index`.
    ///
    /// Existing active elements are shifted right. If an inactive slot exists,
    /// storage length is preserved.
    pub fn insert(&mut self, index: usize, element: T) {
        assert!(
            index <= self.active_len,
            "insertion index (is {index}) should be <= len (is {})",
            self.active_len
        );

        if self.active_len == self.storage.len() {
            self.storage.push(element);
        } else {
            self.storage[self.active_len] = element;
        }

        self.storage[index..=self.active_len].rotate_right(1);
        self.active_len += 1;
        self.debug_assert_invariant();
    }

    /// Removes the last active element while retaining it for reuse.
    ///
    /// Returns `true` if an active element was removed.
    pub const fn recycle_pop(&mut self) -> bool {
        if self.active_len == 0 {
            return false;
        }

        self.active_len -= 1;
        true
    }

    /// Removes the active element at `index` while retaining it for reuse.
    ///
    /// Remaining active elements keep their relative order.
    pub fn recycle_remove(&mut self, index: usize) {
        assert!(
            index < self.active_len,
            "recycle_remove index (is {index}) should be < len (is {})",
            self.active_len
        );

        self.storage[index..self.active_len].rotate_left(1);
        self.active_len -= 1;
        self.debug_assert_invariant();
    }

    /// Removes the active element at `index` by replacing it with the last
    /// active element, retaining the removed element for reuse.
    pub fn recycle_swap_remove(&mut self, index: usize) {
        assert!(
            index < self.active_len,
            "recycle_swap_remove index (is {index}) should be < len (is {})",
            self.active_len
        );

        self.active_len -= 1;
        self.storage.swap(index, self.active_len);
        self.debug_assert_invariant();
    }

    /// Retains only active elements that satisfy `f`.
    pub fn retain(&mut self, mut f: impl FnMut(&T) -> bool) {
        self.retain_mut(|value| f(value));
    }

    /// Retains only active elements that satisfy `f`, passing mutable references.
    pub fn retain_mut(&mut self, mut f: impl FnMut(&mut T) -> bool) {
        let mut deleted = 0;
        for index in 0..self.active_len {
            if f(&mut self.storage[index]) {
                if deleted != 0 {
                    self.storage.swap(index - deleted, index);
                }
            } else {
                deleted += 1;
            }
        }
        self.active_len -= deleted;
        self.debug_assert_invariant();
    }

    /// Deduplicates consecutive active elements.
    pub fn dedup(&mut self)
    where
        T: PartialEq,
    {
        self.dedup_by(|a, b| a == b);
    }

    /// Deduplicates consecutive active elements by key.
    pub fn dedup_by_key<K: PartialEq>(&mut self, mut key: impl FnMut(&mut T) -> K) {
        self.dedup_by(|a, b| key(a) == key(b));
    }

    /// Deduplicates consecutive active elements using a comparison function.
    pub fn dedup_by(&mut self, mut same_bucket: impl FnMut(&mut T, &mut T) -> bool) {
        if self.active_len <= 1 {
            return;
        }

        let mut write = 1;
        for read in 1..self.active_len {
            let same = {
                let (left, right) = self.storage.split_at_mut(read);
                same_bucket(&mut left[write - 1], &mut right[0])
            };

            if !same {
                if write != read {
                    self.storage.swap(write, read);
                }
                write += 1;
            }
        }
        self.active_len = write;
        self.debug_assert_invariant();
    }

    /// Sorts the active elements.
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.as_mut_slice().sort();
    }

    /// Sorts the active elements with a comparison function.
    pub fn sort_by(&mut self, compare: impl FnMut(&T, &T) -> std::cmp::Ordering) {
        self.as_mut_slice().sort_by(compare);
    }

    /// Sorts the active elements by key.
    pub fn sort_by_key<K: Ord>(&mut self, key: impl FnMut(&T) -> K) {
        self.as_mut_slice().sort_by_key(key);
    }

    /// Reverses the active elements.
    pub fn reverse(&mut self) {
        self.as_mut_slice().reverse();
    }

    /// Splits off active elements at `at`, retaining them for reuse.
    pub fn recycle_split_off(&mut self, at: usize) {
        assert!(
            at <= self.active_len,
            "`at` recycle_split_off index (is {at}) should be <= len (is {})",
            self.active_len
        );

        self.active_len = at;
        self.debug_assert_invariant();
    }

    /// Drains active elements in `range`, retaining them for reuse.
    pub fn recycle_drain<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        let (start, end) = self.active_range_bounds(range, "recycle_drain");
        let removed_len = end - start;

        self.storage[start..self.active_len].rotate_left(removed_len);
        self.active_len -= removed_len;
        self.debug_assert_invariant();
    }

    /// Removes the last active element and returns it.
    ///
    /// This transfers ownership out of the collection and removes the element
    /// from the recycling pool. Prefer [`recycle_pop`] when ownership is not
    /// needed.
    ///
    /// [`recycle_pop`]: Self::recycle_pop
    pub fn take_pop(&mut self) -> Option<T> {
        if self.active_len == 0 {
            return None;
        }

        self.active_len -= 1;
        let removed = self.storage.remove(self.active_len);
        self.debug_assert_invariant();
        Some(removed)
    }

    /// Removes and returns the active element at `index`.
    ///
    /// This transfers ownership out of the collection and removes the element
    /// from the recycling pool. Prefer [`recycle_remove`] when ownership is not
    /// needed.
    ///
    /// [`recycle_remove`]: Self::recycle_remove
    pub fn take_remove(&mut self, index: usize) -> T {
        assert!(
            index < self.active_len,
            "take_remove index (is {index}) should be < len (is {})",
            self.active_len
        );

        self.active_len -= 1;
        let removed = self.storage.remove(index);
        self.debug_assert_invariant();
        removed
    }

    /// Removes and returns the active element at `index`, replacing it with the
    /// last active element.
    ///
    /// This transfers ownership out of the collection and removes the element
    /// from the recycling pool. Prefer [`recycle_swap_remove`] when ownership is
    /// not needed.
    ///
    /// [`recycle_swap_remove`]: Self::recycle_swap_remove
    pub fn take_swap_remove(&mut self, index: usize) -> T {
        assert!(
            index < self.active_len,
            "take_swap_remove index (is {index}) should be < len (is {})",
            self.active_len
        );

        let last_active_index = self.active_len - 1;
        self.storage.swap(index, last_active_index);
        self.active_len -= 1;
        let removed = self.storage.remove(last_active_index);
        self.debug_assert_invariant();
        removed
    }

    /// Splits off active elements at `at` and returns them as a `Vec`.
    ///
    /// This transfers ownership out of the collection and removes those
    /// elements from the recycling pool. Prefer [`recycle_split_off`] when
    /// ownership is not needed.
    ///
    /// [`recycle_split_off`]: Self::recycle_split_off
    pub fn take_split_off(&mut self, at: usize) -> Vec<T> {
        assert!(
            at <= self.active_len,
            "`at` take_split_off index (is {at}) should be <= len (is {})",
            self.active_len
        );

        let old_active_len = self.active_len;
        self.active_len = at;
        let removed = self.storage.drain(at..old_active_len).collect();
        self.debug_assert_invariant();
        removed
    }

    /// Drains active elements in `range`, returning them as an iterator.
    ///
    /// This transfers ownership out of the collection and removes those
    /// elements from the recycling pool. Prefer [`recycle_drain`] when ownership
    /// is not needed.
    ///
    /// [`recycle_drain`]: Self::recycle_drain
    pub fn take_drain<R>(&mut self, range: R) -> std::vec::IntoIter<T>
    where
        R: RangeBounds<usize>,
    {
        let (start, end) = self.active_range_bounds(range, "take_drain");
        let final_active_len = self.active_len - (end - start);
        self.active_len = start;
        let drained = self.storage.drain(start..end).collect::<Vec<_>>();
        self.active_len = final_active_len;
        self.debug_assert_invariant();
        drained.into_iter()
    }

    /// Appends all active elements from `other`.
    pub fn append(&mut self, other: &mut Self) {
        let other_active_len = other.active_len;
        other.active_len = 0;
        for value in other.storage.drain(..other_active_len) {
            self.push(value);
        }
        self.debug_assert_invariant();
        other.debug_assert_invariant();
    }

    /// Extends from `other` and recycles `other`'s active elements.
    ///
    /// Unlike [`append`](Self::append), this clones active values from `other`
    /// instead of moving them, so both collections keep their initialized
    /// storage available for reuse.
    pub fn extend_recycled_from(&mut self, other: &mut Self)
    where
        T: Clone + Default + Recycle,
    {
        for value in other.as_slice() {
            self.push_cloned(value);
        }
        other.recycle_active();
        self.debug_assert_invariant();
        other.debug_assert_invariant();
    }

    /// Extends with elements from a slice.
    pub fn extend_from_slice(&mut self, other: &[T])
    where
        T: Clone,
    {
        for value in other {
            self.push_cloned(value);
        }
    }

    /// Converts into a `Vec` containing only active elements.
    #[must_use]
    pub fn into_vec(mut self) -> Vec<T> {
        self.storage.truncate(self.active_len);
        self.storage
    }
}

impl<T> Default for RecyclableVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Deref for RecyclableVec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T> DerefMut for RecyclableVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<T, I> Index<I> for RecyclableVec<T>
where
    I: slice::SliceIndex<[T]>,
{
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T, I> IndexMut<I> for RecyclableVec<T>
where
    I: slice::SliceIndex<[T]>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.as_mut_slice()[index]
    }
}

impl<T> Extend<T> for RecyclableVec<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for value in iter {
            self.push(value);
        }
    }
}

impl<'a, T: 'a + Clone> Extend<&'a T> for RecyclableVec<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        for value in iter {
            self.push_cloned(value);
        }
    }
}

impl<T> From<Vec<T>> for RecyclableVec<T> {
    fn from(value: Vec<T>) -> Self {
        Self::from_vec(value)
    }
}

impl<T> From<RecyclableVec<T>> for Vec<T> {
    fn from(value: RecyclableVec<T>) -> Self {
        value.into_vec()
    }
}

impl<T> FromIterator<T> for RecyclableVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let storage: Vec<T> = iter.into_iter().collect();
        Self::from_vec(storage)
    }
}

impl<T> IntoIterator for RecyclableVec<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a RecyclableVec<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut RecyclableVec<T> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T: fmt::Debug> fmt::Debug for RecyclableVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.as_slice()).finish()
    }
}

impl<T: PartialEq> PartialEq for RecyclableVec<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<T: Eq> Eq for RecyclableVec<T> {}

impl<T: PartialOrd> PartialOrd for RecyclableVec<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_slice().partial_cmp(other.as_slice())
    }
}

impl<T: Ord> Ord for RecyclableVec<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl<T: Hash> Hash for RecyclableVec<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

impl<T> AsRef<[T]> for RecyclableVec<T> {
    fn as_ref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T> AsMut<[T]> for RecyclableVec<T> {
    fn as_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }
}

impl<T> Borrow<[T]> for RecyclableVec<T> {
    fn borrow(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T> BorrowMut<[T]> for RecyclableVec<T> {
    fn borrow_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_invariant<T>(vec: &RecyclableVec<T>) {
        assert!(
            vec.active_len <= vec.storage.len(),
            "active_len ({}) must be <= storage.len() ({})",
            vec.active_len,
            vec.storage.len()
        );
    }

    #[test]
    fn clear_retains_elements_for_reuse() {
        let mut vec = RecyclableVec::new();
        vec.push(String::from("alpha"));
        vec.push(String::from("beta"));
        let first_ptr = vec[0].as_ptr();

        vec.clear();
        assert_eq!(vec.len(), 0);
        assert_eq!(vec.stored_len(), 2);

        vec.push_with_recycle(
            |slot| {
                slot.clear();
                slot.push_str("gamma");
            },
            || String::from("unused"),
        );

        assert_eq!(vec.as_slice(), ["gamma"]);
        assert_eq!(vec[0].as_ptr(), first_ptr);
    }

    #[test]
    fn macro_creates_empty_recyclable_vec() {
        let vec: RecyclableVec<i32> = crate::rvec![];

        assert!(vec.is_empty());
        assert_eq!(vec.storage_len(), 0);
    }

    #[test]
    fn macro_creates_recyclable_vec_from_elements() {
        let vec = crate::rvec![1, 2, 3,];

        assert_eq!(vec.active(), &[1, 2, 3]);
        assert_eq!(vec.storage_len(), 3);
    }

    #[test]
    fn macro_creates_recyclable_vec_from_repeated_element() {
        let vec = crate::rvec![String::from("x"); 2];

        assert_eq!(vec.active(), ["x", "x"]);
        assert_eq!(vec.storage_len(), 2);
    }

    #[test]
    fn truncate_retains_tail_slots() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3, 4]);
        vec.truncate(2);

        assert_eq!(vec.as_slice(), &[1, 2]);
        assert_eq!(vec.storage(), &[1, 2, 3, 4]);
        assert_eq!(vec.recyclable_len(), 2);

        vec.push_with_recycle(|slot| *slot = 9, || 0);
        assert_eq!(vec.as_slice(), &[1, 2, 9]);
        assert_eq!(vec.storage(), &[1, 2, 9, 4]);
    }

    #[test]
    fn resize_with_recycle_reuses_inactive_values() {
        let mut vec = RecyclableVec::from_vec(vec![
            String::from("wide allocation"),
            String::from("other allocation"),
        ]);
        let ptr = vec[1].as_ptr();

        vec.truncate(1);
        vec.resize_with_recycle(
            2,
            |slot| {
                slot.clear();
                slot.push_str("reuse");
            },
            String::new,
        );

        assert_eq!(vec.as_slice(), ["wide allocation", "reuse"]);
        assert_eq!(vec[1].as_ptr(), ptr);
    }

    #[test]
    fn resize_uses_clone_from_for_inactive_values() {
        let mut vec = RecyclableVec::from_vec(vec![String::from("wide allocation")]);
        let ptr = vec[0].as_ptr();

        vec.clear();
        vec.resize(1, String::new());

        assert_eq!(vec.as_slice(), [""]);
        assert_eq!(vec[0].as_ptr(), ptr);
    }

    #[test]
    fn push_default_uses_clone_from_for_inactive_values() {
        let mut vec = RecyclableVec::from_vec(vec![String::from("wide allocation")]);
        let ptr = vec[0].as_ptr();

        vec.clear();
        vec.push_default();

        assert_eq!(vec.as_slice(), [""]);
        assert_eq!(vec[0].as_ptr(), ptr);
    }

    #[test]
    fn push_recycled_uses_recycle_trait_for_inactive_values() {
        let mut vec = RecyclableVec::from_vec(vec![String::from("wide allocation")]);
        let ptr = vec[0].as_ptr();

        vec.clear();
        let slot = vec.push_recycled();
        slot.push_str("reuse");

        assert_eq!(vec.active(), ["reuse"]);
        assert_eq!(vec[0].as_ptr(), ptr);
    }

    #[test]
    fn default_constructors_preinitialize_inactive_storage() {
        let vec = RecyclableVec::<String>::with_storage_len(2);

        assert!(vec.active().is_empty());
        assert_eq!(vec.inactive(), ["", ""]);
        assert_eq!(vec.storage_len(), 2);

        let vec = RecyclableVec::<Vec<u8>>::with_recycled_capacity(3);
        assert_eq!(vec.inactive_len(), 3);
    }

    #[test]
    fn push_recycled_with_configures_the_new_slot() {
        let mut vec = RecyclableVec::from_vec(vec![String::from("wide allocation")]);
        let ptr = vec[0].as_ptr();

        vec.clear();
        vec.push_recycled_with(|slot| slot.push_str("configured"));

        assert_eq!(vec.active(), ["configured"]);
        assert_eq!(vec[0].as_ptr(), ptr);
    }

    #[test]
    fn resize_recycled_uses_recycle_trait_for_inactive_values() {
        let mut vec = RecyclableVec::from_vec(vec![String::from("wide allocation")]);
        let ptr = vec[0].as_ptr();

        vec.clear();
        vec.resize_recycled(1);

        assert_eq!(vec.active(), [""]);
        assert_eq!(vec[0].as_ptr(), ptr);
    }

    #[test]
    fn ensure_active_len_only_grows_when_needed() {
        let mut vec = RecyclableVec::from_vec(vec![
            String::from("wide allocation"),
            String::from("other wide allocation"),
        ]);
        vec.clear();
        let ptr = vec.inactive()[0].as_ptr();

        vec.ensure_active_len_with(2, |slot| slot.push('x'));
        assert_eq!(vec.active(), ["x", "x"]);
        assert_eq!(vec[0].as_ptr(), ptr);

        vec[0].push('y');
        vec.ensure_active_len(1);
        assert_eq!(vec.active(), ["xy", "x"]);
    }

    #[test]
    fn recycle_active_resets_values_before_making_them_inactive() {
        let mut vec = RecyclableVec::from_vec(vec![String::from("wide allocation")]);
        let ptr = vec[0].as_ptr();

        vec.recycle_active();

        assert!(vec.active().is_empty());
        assert_eq!(vec.inactive(), [""]);
        assert_eq!(vec.inactive()[0].as_ptr(), ptr);
    }

    #[test]
    fn active_and_inactive_aliases_expose_the_two_regions() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3]);
        vec.truncate(1);

        assert_eq!(vec.active_len(), 1);
        assert_eq!(vec.storage_len(), 3);
        assert_eq!(vec.inactive_len(), 2);
        assert_eq!(vec.active(), &[1]);
        assert_eq!(vec.inactive(), &[2, 3]);

        vec.active_mut()[0] = 9;
        vec.inactive_mut()[0] = 8;

        assert_eq!(vec.storage(), &[9, 8, 3]);
    }

    #[test]
    fn reserve_active_accounts_for_inactive_slots() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3, 4]);
        vec.truncate(1);
        let capacity = vec.capacity();

        vec.reserve_active(3);
        assert_eq!(vec.capacity(), capacity);

        vec.reserve_active(4);
        assert!(vec.capacity() >= 5);
    }

    #[test]
    fn extend_recycled_from_preserves_both_recycling_pools() {
        let mut left = RecyclableVec::from_vec(vec![String::from(
            "large left allocation with spare capacity",
        )]);
        let left_ptr = left[0].as_ptr();
        left.clear();

        let mut right = RecyclableVec::from_vec(vec![String::from("right allocation")]);
        let right_ptr = right[0].as_ptr();

        left.extend_recycled_from(&mut right);

        assert_eq!(left.active(), ["right allocation"]);
        assert_eq!(left[0].as_ptr(), left_ptr);
        assert!(right.active().is_empty());
        assert_eq!(right.inactive(), [""]);
        assert_eq!(right.inactive()[0].as_ptr(), right_ptr);
    }

    #[test]
    fn extend_from_slice_uses_clone_from_for_inactive_values() {
        let mut vec = RecyclableVec::from_vec(vec![String::from("wide allocation")]);
        let ptr = vec[0].as_ptr();

        vec.clear();
        vec.extend_from_slice(&[String::from("short")]);

        assert_eq!(vec.as_slice(), ["short"]);
        assert_eq!(vec[0].as_ptr(), ptr);
    }

    #[test]
    fn iterators_only_visit_active_elements() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3]);
        vec.truncate(2);

        assert_eq!(vec.iter().copied().collect::<Vec<_>>(), vec![1, 2]);
        assert_eq!((&vec).into_iter().copied().collect::<Vec<_>>(), vec![1, 2]);

        for value in &mut vec {
            *value *= 10;
        }
        assert_eq!(vec.as_slice(), &[10, 20]);
        assert_eq!(vec.storage(), &[10, 20, 3]);
    }

    #[test]
    fn into_iterator_consumes_only_active_elements() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3]);
        vec.truncate(2);

        assert_eq!(vec.into_iter().collect::<Vec<_>>(), vec![1, 2]);
    }

    #[test]
    fn vec_like_mutators_work_on_active_elements() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 2, 3, 4]);
        vec.truncate(4);
        vec.insert(1, 9);
        assert_eq!(vec.as_slice(), &[1, 9, 2, 2, 3]);

        vec.dedup();
        assert_eq!(vec.as_slice(), &[1, 9, 2, 3]);

        vec.retain(|value| value % 2 == 1);
        assert_eq!(vec.as_slice(), &[1, 9, 3]);

        vec.recycle_remove(1);
        assert_eq!(vec.as_slice(), &[1, 3]);
        assert_eq!(vec.storage(), &[1, 3, 9, 2, 2]);
    }

    #[test]
    fn extend_and_collect_are_vec_like() {
        let mut vec = RecyclableVec::new();
        vec.extend([1, 2, 3]);
        vec.truncate(1);
        vec.extend([4, 5]);

        assert_eq!(vec.as_slice(), &[1, 4, 5]);
        assert_eq!(Vec::from(vec), vec![1, 4, 5]);

        let collected: RecyclableVec<_> = [7, 8].into_iter().collect();
        assert_eq!(collected.as_slice(), &[7, 8]);
    }

    #[test]
    fn shrink_to_never_discards_active_elements() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3, 4]);
        vec.truncate(3);

        vec.shrink_to(1);

        assert_invariant(&vec);
        assert_eq!(vec.as_slice(), &[1, 2, 3]);
        assert_eq!(vec.storage(), &[1, 2, 3]);
        assert!(vec.capacity() >= 3);
    }

    #[test]
    fn shrink_to_fit_discards_inactive_elements() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3, 4]);
        vec.truncate(2);

        vec.shrink_to_fit();

        assert_invariant(&vec);
        assert_eq!(vec.as_slice(), &[1, 2]);
        assert_eq!(vec.storage(), &[1, 2]);
    }

    #[test]
    fn recycle_drain_preserves_invariant_and_reuses_removed_values() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3, 4, 5]);
        vec.truncate(4);

        vec.recycle_drain(1..3);

        assert_invariant(&vec);
        assert_eq!(vec.as_slice(), &[1, 4]);
        assert_eq!(vec.storage(), &[1, 4, 2, 3, 5]);
    }

    #[test]
    fn recycle_split_off_preserves_invariant_and_storage() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3, 4]);
        vec.truncate(3);

        vec.recycle_split_off(1);

        assert_invariant(&vec);
        assert_eq!(vec.as_slice(), &[1]);
        assert_eq!(vec.storage(), &[1, 2, 3, 4]);
    }

    #[test]
    fn append_preserves_other_invariant() {
        let mut left = RecyclableVec::from_vec(vec![1]);
        let mut right = RecyclableVec::from_vec(vec![2, 3, 4]);
        right.truncate(2);

        left.append(&mut right);

        assert_invariant(&left);
        assert_invariant(&right);
        assert_eq!(left.as_slice(), &[1, 2, 3]);
        assert_eq!(right.as_slice(), &[]);
        assert_eq!(right.storage(), &[4]);
    }

    #[test]
    fn recycle_swap_remove_uses_last_active_element() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3, 99]);
        vec.truncate(3);

        vec.recycle_swap_remove(0);

        assert_invariant(&vec);
        assert_eq!(vec.as_slice(), &[3, 2]);
        assert_eq!(vec.storage(), &[3, 2, 1, 99]);
    }

    #[test]
    fn recycle_pop_retains_last_active_element() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3]);

        assert!(vec.recycle_pop());
        assert_invariant(&vec);
        assert_eq!(vec.as_slice(), &[1, 2]);
        assert_eq!(vec.storage(), &[1, 2, 3]);

        vec.clear();
        assert!(!vec.recycle_pop());
    }

    #[test]
    fn take_methods_transfer_ownership_out_of_storage() {
        let mut vec = RecyclableVec::from_vec(vec![1, 2, 3, 4, 5]);
        vec.truncate(4);

        assert_eq!(vec.take_pop(), Some(4));
        assert_eq!(vec.storage(), &[1, 2, 3, 5]);

        assert_eq!(vec.take_remove(1), 2);
        assert_eq!(vec.storage(), &[1, 3, 5]);

        assert_eq!(vec.take_swap_remove(0), 1);
        assert_eq!(vec.as_slice(), &[3]);
        assert_eq!(vec.storage(), &[3, 5]);

        vec.resize(4, 9);
        let drained = vec.take_drain(1..3).collect::<Vec<_>>();
        assert_eq!(drained, vec![9, 9]);
        assert_eq!(vec.as_slice(), &[3, 9]);

        let split = vec.take_split_off(1);
        assert_eq!(split, vec![9]);
        assert_eq!(vec.as_slice(), &[3]);
        assert_invariant(&vec);
    }
}
