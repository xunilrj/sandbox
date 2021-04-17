pub struct Matrix<T>(usize, Vec<T>); // usize = qty rows
impl<T> Matrix<T>
where
    T: Default + Clone,
{
    pub fn new(cols: usize, rows: usize) -> Self {
        Self(rows, vec![Default::default(); cols * rows])
    }

    #[inline(always)]
    pub fn at(&self, col: isize, row: isize) -> Option<&T> {
        if col < 0 {
            None
        } else if row < 0 {
            None
        } else {
            let idx = col * (self.0 as isize) + row;
            self.1.get(idx as usize)
        }
    }

    #[inline(always)]
    pub fn set(&mut self, col: isize, row: isize, v: T) {
        let idx = col * (self.0 as isize) + row;
        match self.1.get_mut(idx as usize) {
            Some(ptr) => *ptr = v,
            None => {}
        }
    }
}

impl<T> std::fmt::Debug for Matrix<T>
where
    T: std::fmt::Display + Default + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cols = self.1.len() / self.0;

        print!("{:10}", " ");

        for col in 0..cols {
            print!("{:10}", col);
        }
        println!("\n--------------------------------------------------------");
        for row in 0..self.0 {
            print!("{:10}", row);
            for col in 0..cols {
                let v = self.at(col as isize, row as isize).unwrap();
                print!("{:10}", v);
            }
            println!("");
        }

        f.write_str("")
    }
}

#[extension_trait::extension_trait]
pub impl<T> SliceISizeApi<T> for &[T] {
    fn len_isize(&self) -> isize {
        self.len() as isize
    }

    fn get_isize(&self, idx: isize) -> Option<&T> {
        if idx < 0 {
            None
        } else {
            let idx = idx as usize;
            if idx >= self.len() {
                None
            } else {
                Some(unsafe { self.get_unchecked(idx) })
            }
        }
    }
}

#[extension_trait::extension_trait]
pub impl<T> VecISizeApi<T> for Vec<T> {
    fn len_isize(&self) -> isize {
        self.len() as isize
    }

    fn get_isize(&self, idx: isize) -> Option<&T> {
        if idx < 0 {
            None
        } else {
            let idx = idx as usize;
            if idx >= self.len() {
                None
            } else {
                Some(unsafe { self.get_unchecked(idx) })
            }
        }
    }
}
