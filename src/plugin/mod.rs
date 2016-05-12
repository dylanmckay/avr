extern crate ruby_mri;

use self::ruby_mri::RubyVM;

pub struct Plugin
{
    vm: RubyVM,
}

impl Plugin
{
    pub fn new() -> Self {
        Plugin {
            vm: RubyVM::new(),
        }
    }

    pub fn do_thing(&mut self) {
        RubyVM::eval("puts 'hello world'");
    }
}
