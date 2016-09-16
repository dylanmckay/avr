/// An error on the AVR.
#[derive(Debug)]
pub enum Error
{
    StackOverflow,
    SegmentationFault {
        address: usize,
    },
}

