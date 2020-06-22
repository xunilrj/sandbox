#[macro_use]
extern crate arrayref;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use sqlparser::ast::SetExpr::Select;
use sqlparser::ast::SetExpr::Values;
use sqlparser::ast::Statement::{Insert, Query};
use sqlparser::ast::TableFactor::Table;
use sqlparser::ast::Value::Number;
use sqlparser::dialect::GenericDialect;
use sqlparser::parser::Parser;
use std::collections::HashMap;
use std::io::*;

mod Core {
    use std::collections::HashMap;
    use std::slice::Iter;

    enum PageStatus {
        NotInit,
        Dirty,
        Ok,
    }

    pub struct Page {
        file_name: String,
        file_offset: usize,
        status: PageStatus,
        data: [u8; 1024 * 4],
    }

    impl Page {
        pub fn size(&self) -> usize {
            self.data.len()
        }
    }

    impl Default for Page {
        fn default() -> Page {
            return Page {
                file_name: "".to_owned(),
                file_offset: 0,
                status: PageStatus::NotInit,
                data: [0; 1024 * 4],
            };
        }
    }

    #[derive(Debug)]
    pub enum FieldType {
        Number,
    }

    impl FieldType {
        pub fn size(&self) -> usize {
            match self {
                Number => std::mem::size_of::<u64>(),
            }
        }

        pub fn read_from(&self, array: &[u8], addr: usize) -> (usize, Field) {
            let mut at = addr;
            match self {
                Number => {
                    let n = u64::from_le_bytes(*array_ref!(array, at, 8));
                    (at, Field::Number(n))
                }
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    pub enum Field {
        Number(u64),
    }

    impl Field {
        pub fn write_to(&self, array: &mut [u8], addr: usize) -> usize {
            let mut at = addr;
            match self {
                Field::Number(n) => {
                    let bytes = n.to_le_bytes();
                    for a in bytes.iter() {
                        array[at] = *a;
                        at += 1;
                    }
                    at
                }
            }
        }
    }

    #[derive(Debug)]
    pub struct ColumnMetadata {
        pub name: String,
        col_type: FieldType,
    }

    #[derive(Debug, Default)]
    pub struct Row {
        fields: HashMap<String, Field>,
    }

    impl Row {
        pub fn set_field(&mut self, name: &str, f: Field) {
            match self.fields.get_mut(name) {
                Some(v) => {
                    *v = f;
                }
                None => {
                    self.fields.insert(name.to_owned(), f);
                }
            }
        }

        pub fn read_from(&mut self, columns: &Vec<ColumnMetadata>, page: &Page, addr: usize) {
            let mut at = addr;
            for col in columns {
                let (inc, newf) = col.col_type.read_from(&page.data, addr);
                at += inc;

                self.fields.insert(col.name.clone(), newf);
            }
        }

        pub fn write_to(&self, columns: &Vec<ColumnMetadata>, page: &mut Page, addr: usize) {
            page.status = PageStatus::Dirty;
            let mut at = addr;
            for col in columns {
                at += match self.fields.get(&col.name) {
                    Some(v) => v.write_to(&mut page.data, at),
                    None => col.col_type.size(),
                }
            }
        }
    }

    #[derive(Default)]
    pub struct Table {
        count: usize,
        row_size: usize,
        pages: [Page; 10],
        columns: Vec<ColumnMetadata>,
    }

    impl Table {
        fn pages_count(&self) -> usize {
            self.pages.len()
        }

        fn append_addr(&self) -> (usize, usize) {
            return (
                self.row_size * self.count / 1024,
                self.row_size * self.count % 1024,
            );
        }

        pub fn append(&mut self, row: &Row) -> usize {
            let (pagei, addr) = self.append_addr();
            let page = &mut self.pages[pagei];

            row.write_to(&self.columns, page, addr);
            self.count += 1;

            pagei
        }

        pub fn new_field(&mut self, name: &str, col_type: FieldType) {
            self.row_size += col_type.size();
            self.columns.push(ColumnMetadata {
                name: name.to_owned(),
                col_type,
            });
        }

        pub fn iter_cols(&self) -> Iter<ColumnMetadata> {
            self.columns.iter()
        }
    }

    pub struct RowIter<'a> {
        table: &'a Table,
        i: usize,
        page: usize,
        addr: usize,
    }

    impl<'a> Iterator for RowIter<'a> {
        type Item = Row;
        fn next(&mut self) -> std::option::Option<<Self as std::iter::Iterator>::Item> {
            if self.page >= self.table.pages_count() || self.i == self.table.count {
                None
            } else {
                let mut row: Row = Default::default();
                row.set_field("id", Field::Number(0));

                let page = &self.table.pages[self.page];
                row.read_from(&self.table.columns, page, self.addr);

                self.addr += self.table.row_size;
                self.i += 1;
                if self.addr + self.table.row_size > page.size() {
                    self.page += 1;
                    self.addr = 0;
                }

                Some(row)
            }
        }
    }

    impl<'a> IntoIterator for &'a Table {
        type Item = Row;
        type IntoIter = RowIter<'a>;
        fn into_iter(self) -> <Self as std::iter::IntoIterator>::IntoIter {
            RowIter {
                table: self,
                i: 0,
                page: 0,
                addr: 0,
            }
        }
    }

    pub struct DirtyPage {
        table: String,
        idx: usize,
    }

    #[derive(Default)]
    pub struct Database {
        pub tables: HashMap<String, Table>,
        pub filename: Option<String>,
        dirty: Vec<DirtyPage>,
        fileoffset: HashMap<String, usize>,
    }

    impl Database {
        pub fn new_table(&mut self, name: &str) -> &mut Table {
            self.tables.insert(name.to_owned(), Default::default());
            return self.tables.get_mut(name).unwrap();
        }

        pub fn save_to(&mut self, filename: &str) {
            self.filename = Some(filename.to_owned())
        }

        pub fn append(&mut self, table: &str, row: &Row) {
            let t = self.tables.get(table).unwrap();
            let dirty_page = t.append(row);
            let p = t.pages[dirty_page];

            p.status = PageStatus::Dirty;
            p.file_offset = match self.fileoffset.get(&p.file_name) {
                Some(v) => *v,
                None => {
                    self.fileoffset.insert(p.file_name, 0);
                    0
                }
            };

            self.dirty.push(DirtyPage {
                table: table.to_owned(),
                idx: dirty_page,
            });
        }

        pub fn commit(&mut self) {
            for dirty in self.dirty {
                let t = self.tables.get(&dirty.table).unwrap();
                let p = t.pages[dirty.idx];
            }

            self.dirty.clear();
        }
    }
}

fn exec(cmd: &str, db: &mut Core::Database) -> bool {
    if cmd == ".exit" {
        return false;
    } else {
        let dialect = GenericDialect {};
        let stmts = match Parser::parse_sql(&dialect, cmd.to_owned()) {
            Err(e) => {
                println!("{:?}", e);
                return false;
            }
            Ok(ast) => ast,
        };
        for stmt in stmts {
            println!("{:?}", stmt);
            match stmt {
                Query(q) => match q.body {
                    Select(s) => {
                        for t in s.from {
                            let table = match t.relation {
                                Table { name, .. } => format!("{}", name.0[0]),
                                _ => "".to_owned(),
                            };
                            println!("select table: [{}]", table);
                            match db.tables.get(&table) {
                                Some(t) => {
                                    for row in t {
                                        println!("Row: {:?}", row);
                                    }
                                }
                                None => println!("Table Not Found"),
                            }
                        }
                    }
                    _ => (),
                },
                Insert {
                    table_name,
                    columns,
                    source,
                } => {
                    let mut r: Core::Row = Default::default();
                    r.set_field("id", Core::Field::Number(0));

                    let table_name = table_name.0[0].to_owned();
                    match db.tables.get_mut(&table_name) {
                        Some(t) => {
                            let q = *source;
                            match q.body {
                                Values(v) => match v {
                                    sqlparser::ast::Values(values) => {
                                        let cols = t.iter_cols();
                                        for (v, c) in values.iter().zip(cols) {
                                            let value = match &v[0] {
                                                sqlparser::ast::Expr::Value(v) => match v {
                                                    Number(s) => Core::Field::Number(
                                                        s.parse::<u64>().unwrap(),
                                                    ),
                                                    _ => Core::Field::Number(0),
                                                },
                                                _ => Core::Field::Number(0),
                                            };
                                            r.set_field(&c.name, value);
                                        }
                                    }
                                    _ => (),
                                },
                                _ => (),
                            }
                            t.append(&r);
                            t.commit();
                        }
                        None => {
                            println!("Table does not exist");
                        }
                    }
                }
                _ => println!("Unknown SQL command: [{:?}]", stmt),
            }
        }
        //println!("Unkown command: [{}]", cmd);
    }
    return true;
}

fn main() {
    let mut db: Core::Database = Default::default();
    db.save_to("db");
    let mut t = db.new_table("users");
    t.new_field("id", Core::FieldType::Number);

    let history_file = "history.txt";
    let mut rl = Editor::<()>::new();
    let _ = rl.load_history(history_file);
    loop {
        let run = match rl.readline(">> ") {
            Ok(line) => {
                println!("{:?}", line);
                rl.add_history_entry(line.as_str());
                rl.save_history(history_file).unwrap();
                exec(&line, &mut db)
            }
            Err(e) => {
                println!("{:?}", e);
                false
            }
        };
        if !run {
            break;
        }
    }
}
