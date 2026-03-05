import Metal
import Foundation

let device = MTLCreateSystemDefaultDevice()!
let queue  = device.makeCommandQueue()!

func compileKernels(mslPath: String) -> MTLLibrary {
    let src = try! String(contentsOfFile: mslPath, encoding: .utf8)
    return try! device.makeLibrary(source: src, options: nil)
}

func makeBuffer<T>(_ data: [T]) -> MTLBuffer {
    return data.withUnsafeBytes {
        device.makeBuffer(bytes: $0.baseAddress!, length: $0.count,
                          options: .storageModeShared)!
    }
}

func makeEmptyBuffer(floatCount: Int) -> MTLBuffer {
    return device.makeBuffer(length: floatCount * 4, options: .storageModeShared)!
}

func readBuffer(_ buf: MTLBuffer, count: Int) -> [Float] {
    let ptr = buf.contents().bindMemory(to: Float.self, capacity: count)
    return Array(UnsafeBufferPointer(start: ptr, count: count))
}

let library = compileKernels(mslPath: "kernels.metal")

func run_vec_map(_ input: [Float]) -> [Float] {
    let fn  = library.makeFunction(name: "vec_map")!
    let pso = try! device.makeComputePipelineState(function: fn)
    let cb  = queue.makeCommandBuffer()!
    let enc = cb.makeComputeCommandEncoder()!
    enc.setComputePipelineState(pso)
    let buf0 = makeBuffer(input)
    let buf1 = makeEmptyBuffer(floatCount: 8)
    enc.setBuffer(buf0, offset: 0, index: 0)
    enc.setBuffer(buf1, offset: 0, index: 1)
    let grid = MTLSize(width: 8, height: 1, depth: 1)
    let tg   = MTLSize(width: min(256, 8), height: 1, depth: 1)
    enc.dispatchThreads(grid, threadsPerThreadgroup: tg)
    enc.endEncoding()
    cb.commit(); cb.waitUntilCompleted()
    return readBuffer(buf1, count: 8)
}

func run_mat_sqrt(_ input: [Float]) -> [Float] {
    let fn  = library.makeFunction(name: "mat_sqrt")!
    let pso = try! device.makeComputePipelineState(function: fn)
    let cb  = queue.makeCommandBuffer()!
    let enc = cb.makeComputeCommandEncoder()!
    enc.setComputePipelineState(pso)
    let buf0 = makeBuffer(input)
    let buf1 = makeEmptyBuffer(floatCount: 16)
    enc.setBuffer(buf0, offset: 0, index: 0)
    enc.setBuffer(buf1, offset: 0, index: 1)
    let grid = MTLSize(width: 4, height: 4, depth: 1)
    let tg   = MTLSize(width: min(256, 4), height: min(16, 4), depth: 1)
    enc.dispatchThreads(grid, threadsPerThreadgroup: tg)
    enc.endEncoding()
    cb.commit(); cb.waitUntilCompleted()
    return readBuffer(buf1, count: 16)
}

func run_cube_sin(_ input: [Float]) -> [Float] {
    let fn  = library.makeFunction(name: "cube_sin")!
    let pso = try! device.makeComputePipelineState(function: fn)
    let cb  = queue.makeCommandBuffer()!
    let enc = cb.makeComputeCommandEncoder()!
    enc.setComputePipelineState(pso)
    let buf0 = makeBuffer(input)
    let buf1 = makeEmptyBuffer(floatCount: 24)
    enc.setBuffer(buf0, offset: 0, index: 0)
    enc.setBuffer(buf1, offset: 0, index: 1)
    let grid = MTLSize(width: 4, height: 3, depth: 2)
    let tg   = MTLSize(width: min(256, 4), height: min(16, 3), depth: 1)
    enc.dispatchThreads(grid, threadsPerThreadgroup: tg)
    enc.endEncoding()
    cb.commit(); cb.waitUntilCompleted()
    return readBuffer(buf1, count: 24)
}

func run_vec_fill() -> [Float] {
    let fn  = library.makeFunction(name: "vec_fill")!
    let pso = try! device.makeComputePipelineState(function: fn)
    let cb  = queue.makeCommandBuffer()!
    let enc = cb.makeComputeCommandEncoder()!
    enc.setComputePipelineState(pso)
    let buf0 = makeEmptyBuffer(floatCount: 8)
    enc.setBuffer(buf0, offset: 0, index: 0)
    let grid = MTLSize(width: 8, height: 1, depth: 1)
    let tg   = MTLSize(width: min(256, 8), height: 1, depth: 1)
    enc.dispatchThreads(grid, threadsPerThreadgroup: tg)
    enc.endEncoding()
    cb.commit(); cb.waitUntilCompleted()
    return readBuffer(buf0, count: 8)
}

func run_mat_transpose(_ input: [Float]) -> [Float] {
    let fn  = library.makeFunction(name: "mat_transpose")!
    let pso = try! device.makeComputePipelineState(function: fn)
    let cb  = queue.makeCommandBuffer()!
    let enc = cb.makeComputeCommandEncoder()!
    enc.setComputePipelineState(pso)
    let buf0 = makeBuffer(input)
    let buf1 = makeEmptyBuffer(floatCount: 16)
    enc.setBuffer(buf0, offset: 0, index: 0)
    enc.setBuffer(buf1, offset: 0, index: 1)
    let grid = MTLSize(width: 4, height: 4, depth: 1)
    let tg   = MTLSize(width: min(256, 4), height: min(16, 4), depth: 1)
    enc.dispatchThreads(grid, threadsPerThreadgroup: tg)
    enc.endEncoding()
    cb.commit(); cb.waitUntilCompleted()
    return readBuffer(buf1, count: 16)
}

func run_vec_tabulate() -> [Float] {
    let fn  = library.makeFunction(name: "vec_tabulate")!
    let pso = try! device.makeComputePipelineState(function: fn)
    let cb  = queue.makeCommandBuffer()!
    let enc = cb.makeComputeCommandEncoder()!
    enc.setComputePipelineState(pso)
    let buf0 = makeEmptyBuffer(floatCount: 8)
    enc.setBuffer(buf0, offset: 0, index: 0)
    let grid = MTLSize(width: 8, height: 1, depth: 1)
    let tg   = MTLSize(width: min(256, 8), height: 1, depth: 1)
    enc.dispatchThreads(grid, threadsPerThreadgroup: tg)
    enc.endEncoding()
    cb.commit(); cb.waitUntilCompleted()
    return readBuffer(buf0, count: 8)
}

func run_vec_sum(_ input: [Float]) -> [Float] {
    let fn  = library.makeFunction(name: "vec_sum")!
    let pso = try! device.makeComputePipelineState(function: fn)
    let cb  = queue.makeCommandBuffer()!
    let enc = cb.makeComputeCommandEncoder()!
    enc.setComputePipelineState(pso)
    let buf0 = makeBuffer(input)
    let buf1 = makeEmptyBuffer(floatCount: 256)
    enc.setBuffer(buf0, offset: 0, index: 0)
    enc.setBuffer(buf1, offset: 0, index: 1)
    var gsize_val: UInt32 = 256
    let ubuf2 = device.makeBuffer(bytes: &gsize_val, length: 4, options: .storageModeShared)!
    enc.setBuffer(ubuf2, offset: 0, index: 2)
    enc.setThreadgroupMemoryLength(128, index: 0)
    let grid = MTLSize(width: 256, height: 1, depth: 1)
    let tg   = MTLSize(width: min(256, 256), height: 1, depth: 1)
    enc.dispatchThreads(grid, threadsPerThreadgroup: tg)
    enc.endEncoding()
    cb.commit(); cb.waitUntilCompleted()
    return readBuffer(buf1, count: 256)
}

// ── Validation entry point ───────────────────────────────────────────
func validate() {
    // ── vec_map
    let vec_map_input: [Float] = (0..<8).map { Float($0) + 1.0 }
    let vec_map_out = run_vec_map(vec_map_input)
    print("  vec_map: \(vec_map_out.prefix(8))")

    // ── mat_sqrt
    let mat_sqrt_input: [Float] = (0..<16).map { Float($0) + 1.0 }
    let mat_sqrt_out = run_mat_sqrt(mat_sqrt_input)
    print("  mat_sqrt: \(mat_sqrt_out.prefix(8))")

    // ── cube_sin
    let cube_sin_input: [Float] = (0..<24).map { Float($0) + 1.0 }
    let cube_sin_out = run_cube_sin(cube_sin_input)
    print("  cube_sin: \(cube_sin_out.prefix(8))")

    // ── vec_fill
    let vec_fill_out = run_vec_fill()
    print("  vec_fill: \(vec_fill_out.prefix(8))")

    // ── mat_transpose
    let mat_transpose_input: [Float] = (0..<16).map { Float($0) + 1.0 }
    let mat_transpose_out = run_mat_transpose(mat_transpose_input)
    print("  mat_transpose: \(mat_transpose_out.prefix(8))")

    // ── vec_tabulate
    let vec_tabulate_out = run_vec_tabulate()
    print("  vec_tabulate: \(vec_tabulate_out.prefix(8))")

    // ── vec_sum
    let vec_sum_input: [Float] = (0..<256).map { Float($0) + 1.0 }
    let vec_sum_out = run_vec_sum(vec_sum_input)
    print("  vec_sum: \(vec_sum_out.prefix(8))")

    print("All validations passed.")
}
validate()
