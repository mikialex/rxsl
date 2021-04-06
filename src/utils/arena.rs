use std::marker::PhantomData;

pub struct Storage<T, S: StorageBehavior<T>> {
    data: S::Container,
}
pub struct Handle<T, S: StorageBehavior<T>> {
    phantom: PhantomData<S>,
    phantom_t: PhantomData<T>,
    handle: S::Handle,
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
    type Container;
    type Handle: Copy;

    fn insert(c: &mut Self::Container, v: T) -> Handle<T, Self>;

    fn get(c: &Self::Container, handle: Self::Handle) -> &T;
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
}

impl<T, S: StorageBehavior<T>> Storage<T, S> {
    pub fn insert(&mut self, v: T) -> Handle<T, S> {
        S::insert(&mut self.data, v)
    }

    pub fn get(&self, h: Handle<T, S>) -> &T {
        S::get(&self.data, h.handle)
    }
}
