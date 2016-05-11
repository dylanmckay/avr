#[derive(Clone)]
pub struct Port
{
    pub address: u32,
}

impl Port
{
    pub fn new(address: u32) -> Self {
        Port {
            address: address,
        }
    }
}
