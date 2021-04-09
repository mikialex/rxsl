use std::marker::PhantomData;

pub struct Storage<T, S: StorageBehavior<T>> {
    data: S::Container,
}
pub struct Handle<T, S: StorageBehavior<T>> {
    phantom: PhantomData<S>,
    phantom_t: PhantomData<T>,
    handle: S::Handle,
}

impl<T, S: StorageBehavior<T>> Clone for Handle<T, S> {
    fn clone(&self) -> Self {
        Self::new(self.handle)
    }
}

impl<T, S: StorageBehavior<T>> Handle<T, S> {
    pub fn new(handle: S::Handle) -> Self {
        Self {
            phantom: PhantomData,
            phantom_t: PhantomData,
            handle,
        }
    }
}

pub trait StorageBehavior<T>: Sized {
    type Container: Default;
    type Handle: Copy;

    fn insert(c: &mut Self::Container, v: T) -> Handle<T, Self>;

    fn get(c: &Self::Container, handle: Self::Handle) -> &T;
    fn get_mut(c: &mut Self::Container, handle: Self::Handle) -> &mut T;
}

pub struct VecStorage;

impl<T> StorageBehavior<T> for VecStorage {
    type Container = Vec<T>;
    type Handle = usize;

    fn insert(c: &mut Self::Container, v: T) -> Handle<T, Self> {
        c.push(v);
        Handle::new(c.len() - 1)
    }

    fn get(c: &Self::Container, handle: Self::Handle) -> &T {
        &c[handle]
    }
    fn get_mut(c: &mut Self::Container, handle: Self::Handle) -> &mut T {
        &mut c[handle]
    }
}

pub struct DeduplicateVecStorage;
impl<T: PartialEq + Copy> StorageBehavior<T> for DeduplicateVecStorage {
    type Container = Vec<T>;
    type Handle = usize;

    fn insert(c: &mut Self::Container, v: T) -> Handle<T, Self> {
        c.push(v);
        let index = c.iter().position(|&cv| cv == v).unwrap_or_else(|| {
            c.push(v);
            c.len() - 1
        });
        Handle::new(index)
    }

    fn get(c: &Self::Container, handle: Self::Handle) -> &T {
        &c[handle]
    }
    fn get_mut(c: &mut Self::Container, handle: Self::Handle) -> &mut T {
        &mut c[handle]
    }
}

impl<T, S: StorageBehavior<T>> Storage<T, S> {
    pub fn new() -> Self {
        Self {
            data: S::Container::default(),
        }
    }

    pub fn insert(&mut self, v: T) -> Handle<T, S> {
        S::insert(&mut self.data, v)
    }

    pub fn get(&self, h: Handle<T, S>) -> &T {
        S::get(&self.data, h.handle)
    }

    pub fn get_mut(&mut self, h: Handle<T, S>) -> &mut T {
        S::get_mut(&mut self.data, h.handle)
    }
}
