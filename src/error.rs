/// An error on the AVR.
#[derive(Debug)]
pub enum Error
{
    UnknownInstruction,
    StackOverflow,
    SegmentationFault {
        address: usize,
    },
}

