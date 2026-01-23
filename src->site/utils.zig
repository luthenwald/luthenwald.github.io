pub fn nextPowerOf2(x: usize) usize { var n = x - 1; n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16; if (@sizeOf(usize) == 8) n |= n >> 32; return n + 1; }
