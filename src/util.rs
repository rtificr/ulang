pub trait Pipeable {
    fn pipe<T>(self, mut f: impl FnMut(Self) -> T) -> T 
    where Self: Sized {
        f(self)
    }
}
impl<T> Pipeable for T {}