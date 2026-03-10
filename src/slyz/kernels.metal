#include <metal_stdlib>
#include <simd/simd.h>
using namespace metal;

kernel void vec_map(
    device const float* input [[buffer(0)]],
    device       float* output [[buffer(1)]],
    uint tid [[thread_position_in_grid]]
) {
    uint idx = tid;
    output[idx] = ((input[tid] * 2.0f) + 1.0f);
}

kernel void mat_sqrt(
    device const float* input [[buffer(0)]],
    device       float* output [[buffer(1)]],
    uint2 tid [[thread_position_in_grid]]
) {
    uint idx = tid.y * 4u + tid.x;
    output[idx] = sqrt(input[tid.y * 4u + tid.x]);
}

kernel void cube_sin(
    device const float* input [[buffer(0)]],
    device       float* output [[buffer(1)]],
    uint3 tid [[thread_position_in_grid]]
) {
    uint idx = tid.z * 12u + tid.y * 4u + tid.x;
    output[idx] = sin(input[tid.z * 12u + tid.y * 4u + tid.x]);
}

kernel void vec_fill(
    device float* output [[buffer(0)]],
    uint tid [[thread_position_in_grid]]
) {
    uint idx = tid;
    output[idx] = 1.0f;
}

kernel void mat_transpose(
    device const float* input [[buffer(0)]],
    device       float* output [[buffer(1)]],
    uint2 tid [[thread_position_in_grid]]
) {
    uint oidx = tid.y * 4u + tid.x;
    uint iidx = tid.x * 4u + tid.y;
    output[oidx] = input[iidx];
}

kernel void vec_tabulate(
    device float* output [[buffer(0)]],
    uint tid [[thread_position_in_grid]]
) {
    uint idx = tid;
    float v_pos = (float)idx;
    output[idx] = ((2.0f * v_pos) + 1.0f);
}

kernel void vec_sum(
    device const float* input [[buffer(0)]],
    device       float* partials [[buffer(1)]],
    threadgroup float* shared [[threadgroup(0)]],
    constant uint& gsize [[buffer(2)]],
    uint tid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]],
    uint gid [[threadgroup_position_in_grid]],
    uint tpg [[threads_per_threadgroup]]
) {
    float val = (tid < gsize) ? input[tid] : 0.0f;
    val = simd_sum(val);
    if (simd_is_first()) shared[lid / 32] = val;
    threadgroup_barrier(mem_flags::mem_threadgroup);
    if (lid < tpg / 32)
        val = simd_sum(shared[lid]);
    if (lid == 0) partials[gid] = val;
}

