use ahash::AHashMap as HashMap;
use std::fmt::Debug;

pub struct Scopes<K, V> {
    pub scopes: Vec<Scope<K, V>>,
}
impl<K, V> Scopes<K, V>
where
    K: Eq + std::hash::Hash + Debug,
    V: Debug,
{
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(key) {
                return Some(v);
            }
        }
        None
    }
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.get_mut(key) {
                return Some(v);
            }
        }
        None
    }
    pub fn insert(&mut self, key: K, value: V) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(key, value);
        } else {
            self.scopes.push(Scope::new());
            self.scopes.last_mut().unwrap().insert(key, value);
        }
    }
    pub fn contains(&self, key: &K) -> bool {
        self.scopes.iter().rev().any(|scope| scope.get(key).is_some())
    }
    pub fn push(&mut self) {
        self.scopes.push(Scope::new());
    }
    pub fn pop(&mut self) {
        self.scopes.pop();
    }
    pub fn iter(&self) -> impl Iterator<Item = &Scope<K, V>> + '_ {
        self.scopes.iter()
    }
    pub fn insert_global(&mut self, key: K, value: V) {
        if let Some(scope) = self.scopes.first_mut() {
            scope.insert(key, value);
        } else {
            self.scopes.push(Scope::new());
            self.scopes[0].insert(key, value);
        }
    }
    pub fn lookup_global(&self, key: &K) -> Option<&V> {
        if let Some(scope) = self.scopes.first() {
            scope.get(key)
        } else {
            None
        }
    }
}

pub struct Scope<K, V> {
    pub map: HashMap<K, V>,
}
impl<K, V> Scope<K, V>
where
    K: Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    pub fn insert(&mut self, key: K, value: V) {
        self.map.insert(key, value);
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        self.map.get(key)
    }
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.map.get_mut(key)
    }
    pub fn items(&self) -> impl Iterator<Item = (&K, &V)> {
        self.map.iter()
    }
}