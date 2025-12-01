use std::ops::{Index, IndexMut};

use gridly::direction::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DirectionMap<T> {
    values: [T; 4],
}

impl<T> DirectionMap<T> {
    /// Create a new direction from the given values. The values should be
    /// oriented as [Up, Right, Down, Left]. Prefer using `direction_map!`
    /// instead of this constructor.
    #[inline]
    #[must_use]
    pub fn from_array(values: [T; 4]) -> Self {
        Self { values }
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Direction, &T)> {
        EACH_DIRECTION.into_iter().zip(&self.values)
    }

    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Direction, &mut T)> {
        EACH_DIRECTION.into_iter().zip(&mut self.values)
    }
}

impl<T> Index<Direction> for DirectionMap<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: Direction) -> &Self::Output {
        match index {
            Up => &self.values[0],
            Right => &self.values[1],
            Down => &self.values[2],
            Left => &self.values[3],
        }
    }
}

impl<T> IndexMut<Direction> for DirectionMap<T> {
    #[inline]
    fn index_mut(&mut self, index: Direction) -> &mut Self::Output {
        match index {
            Up => &mut self.values[0],
            Right => &mut self.values[1],
            Down => &mut self.values[2],
            Left => &mut self.values[3],
        }
    }
}

#[macro_export]
macro_rules! direction_map {
    ( $( $direction:pat $(if $condition:expr )? => $value:expr ),+ $(,)? ) => {
        $crate::library::direction_map::DirectionMap::from_array([
            match gridly::direction::Up { $($direction $(if $condition )? => $value,)+ },
            match gridly::direction::Right { $($direction $(if $condition )? => $value,)+ },
            match gridly::direction::Down { $($direction $(if $condition )? => $value,)+ },
            match gridly::direction::Left { $($direction $(if $condition )? => $value,)+ },
        ])
    };
}
