#include <iostream>
#include <sstream>
#include <omp.h>
#include <vector>
#include <chrono>
#include <atomic>

#ifdef _WIN32

//  Windows
#define cpuid(info, x)    __cpuidex(info, x, 0)

#else

//  GCC Intrinsics
#include <cpuid.h>
void cpuid(int info[4], int InfoType){
    __cpuid_count(InfoType, 0, info[0], info[1], info[2], info[3]);
}

#endif

//  Misc.
bool HW_MMX;
bool HW_x64;
bool HW_ABM;      // Advanced Bit Manipulation
bool HW_RDRAND;
bool HW_BMI1;
bool HW_BMI2;
bool HW_ADX;
bool HW_PREFETCHWT1;

//  SIMD: 128-bit
bool HW_SSE;
bool HW_SSE2;
bool HW_SSE3;
bool HW_SSSE3;
bool HW_SSE41;
bool HW_SSE42;
bool HW_SSE4a;
bool HW_AES;
bool HW_SHA;

//  SIMD: 256-bit
bool HW_AVX;
bool HW_XOP;
bool HW_FMA3;
bool HW_FMA4;
bool HW_AVX2;

//  SIMD: 512-bit
bool HW_AVX512F;    //  AVX512 Foundation
bool HW_AVX512CD;   //  AVX512 Conflict Detection
bool HW_AVX512PF;   //  AVX512 Prefetch
bool HW_AVX512ER;   //  AVX512 Exponential + Reciprocal
bool HW_AVX512VL;   //  AVX512 Vector Length Extensions
bool HW_AVX512BW;   //  AVX512 Byte + Word
bool HW_AVX512DQ;   //  AVX512 Doubleword + Quadword
bool HW_AVX512IFMA; //  AVX512 Integer 52-bit Fused Multiply-Add
bool HW_AVX512VBMI; //  AVX512 Vector Byte Manipulation Instructions

void detect()
{
    int info[4];
    cpuid(info, 0);
    int nIds = info[0];

    cpuid(info, 0x80000000);
    unsigned nExIds = info[0];

    //  Detect Features
    if (nIds >= 0x00000001){
        cpuid(info,0x00000001);
        HW_MMX    = (info[3] & ((int)1 << 23)) != 0;
        HW_SSE    = (info[3] & ((int)1 << 25)) != 0;
        HW_SSE2   = (info[3] & ((int)1 << 26)) != 0;
        HW_SSE3   = (info[2] & ((int)1 <<  0)) != 0;

        HW_SSSE3  = (info[2] & ((int)1 <<  9)) != 0;
        HW_SSE41  = (info[2] & ((int)1 << 19)) != 0;
        HW_SSE42  = (info[2] & ((int)1 << 20)) != 0;
        HW_AES    = (info[2] & ((int)1 << 25)) != 0;

        HW_AVX    = (info[2] & ((int)1 << 28)) != 0;
        HW_FMA3   = (info[2] & ((int)1 << 12)) != 0;

        HW_RDRAND = (info[2] & ((int)1 << 30)) != 0;
    }
    if (nIds >= 0x00000007){
        cpuid(info,0x00000007);
        HW_AVX2   = (info[1] & ((int)1 <<  5)) != 0;

        HW_BMI1        = (info[1] & ((int)1 <<  3)) != 0;
        HW_BMI2        = (info[1] & ((int)1 <<  8)) != 0;
        HW_ADX         = (info[1] & ((int)1 << 19)) != 0;
        HW_SHA         = (info[1] & ((int)1 << 29)) != 0;
        HW_PREFETCHWT1 = (info[2] & ((int)1 <<  0)) != 0;

        HW_AVX512F     = (info[1] & ((int)1 << 16)) != 0;
        HW_AVX512CD    = (info[1] & ((int)1 << 28)) != 0;
        HW_AVX512PF    = (info[1] & ((int)1 << 26)) != 0;
        HW_AVX512ER    = (info[1] & ((int)1 << 27)) != 0;
        HW_AVX512VL    = (info[1] & ((int)1 << 31)) != 0;
        HW_AVX512BW    = (info[1] & ((int)1 << 30)) != 0;
        HW_AVX512DQ    = (info[1] & ((int)1 << 17)) != 0;
        HW_AVX512IFMA  = (info[1] & ((int)1 << 21)) != 0;
        HW_AVX512VBMI  = (info[2] & ((int)1 <<  1)) != 0;
    }
    if (nExIds >= 0x80000001){
        cpuid(info,0x80000001);
        HW_x64   = (info[3] & ((int)1 << 29)) != 0;
        HW_ABM   = (info[2] & ((int)1 <<  5)) != 0;
        HW_SSE4a = (info[2] & ((int)1 <<  6)) != 0;
        HW_FMA4  = (info[2] & ((int)1 << 16)) != 0;
        HW_XOP   = (info[2] & ((int)1 << 11)) != 0;
    }
}

template <typename Clock = std::chrono::high_resolution_clock>
class stopwatch
{
    typename Clock::time_point start_point;
public:
    stopwatch()
    {
        std::atomic_thread_fence(std::memory_order_relaxed);
        start_point = Clock::now();
        std::atomic_thread_fence(std::memory_order_relaxed);
    }

    template <typename Rep = typename Clock::duration::rep, typename Units = typename Clock::duration>
    Rep elapsed() const
    {
        std::atomic_thread_fence(std::memory_order_relaxed);
        std::chrono::duration<Rep, Units> duration = Clock::now() - start_point;
        std::atomic_thread_fence(std::memory_order_relaxed);
        return duration.count();
    }
};

#include <immintrin.h>

__attribute__ ((__target__ ("avx2")))
std::ostream& operator << (std::ostream& out, __m256i& r)
{
    out << "["
    << _mm256_extract_epi32(r, 0)
    << " " << _mm256_extract_epi32(r, 1)
    << " " << _mm256_extract_epi32(r, 2)
    << " " << _mm256_extract_epi32(r, 3)
    << " " << _mm256_extract_epi32(r, 4)
    << " " << _mm256_extract_epi32(r, 5)
    << " " << _mm256_extract_epi32(r, 6)
    << " " << _mm256_extract_epi32(r, 7) << "]";

    return out;
}

//https://db.in.tum.de/~finis/x86-intrin-cheatsheet-v2.1.pdf

const long long loopsize = 1000000000;

__attribute__ ((__target__ ("avx2")))
uint32_t avx2_sum32(const std::vector<uint32_t>& v) 
{
    auto loops = v.size() / 8;
    auto steps = v.size() % 8;
    // std::cout << loops << "," << steps << std::endl;

    uint32_t buffer[sizeof(__m256i)/sizeof(uint32_t)] = {0};
    
    __m256i accum = _mm256_load_si256((const __m256i*)&buffer);
    const __m256i* data = (const __m256i*)v.data();
    while(loops > 0)
    {
        auto current = _mm256_load_si256(data);
        //std::cout << accum << "+" << current << std::endl;
        accum = _mm256_add_epi32(accum, current);        
        //std::cout << accum << std::endl;
        --loops;
        ++data;
    }

    _mm256_store_si256((__m256i *)buffer, accum);

    auto total = buffer[0]+buffer[1]+buffer[2]+buffer[3]+buffer[4]+buffer[5]+buffer[6]+buffer[7];
    auto start = (v.size() / 8) * 8;
    for(int i = start;i <  v.size(); ++i)
        total += v[i];

    return total;
}

__attribute__ ((__target__ ("avx512f")))
uint32_t avx512f_sum32(const std::vector<uint32_t>& v) 
{
    auto size = v.size();

    __m512i accum_simd = _mm512_setzero_epi32();
    for(long long  i = 0;i < size; i+=16)
    {
        auto current = _mm512_load_si512((const __m256i*)&v[i]);
        accum_simd = _mm512_add_epi32(accum_simd, current);
    }

    return _mm512_reduce_add_epi32(accum_simd);
}

template <typename TR, typename T1>
using FUNCR = TR (*) (const T1& v);

FUNCR<uint32_t,std::vector<uint32_t>> sum32;

int main(int argc, char** argv)
{
    detect();

    if(HW_AVX512F) sum32 = &avx512f_sum32;
    else if(HW_AVX2) sum32 = &avx2_sum32;

    std::cout << "HW_MMX: " << HW_MMX << std::endl;
    std::cout << "HW_x64: " << HW_x64 << std::endl;
    std::cout << "HW_ABM: " << HW_ABM << std::endl;
    std::cout << "HW_RDRAND: " << HW_RDRAND << std::endl;
    std::cout << "HW_BMI1: " << HW_BMI1 << std::endl;
    std::cout << "HW_BMI2: " << HW_BMI2 << std::endl;
    std::cout << "HW_ADX: " << HW_ADX << std::endl;
    std::cout << "HW_PREFETCHWT1: " << HW_PREFETCHWT1 << std::endl;
    std::cout << "HW_SSE: " << HW_SSE << std::endl;
    std::cout << "HW_SSE2: " << HW_SSE2 << std::endl;
    std::cout << "HW_SSE3: " << HW_SSE3 << std::endl;
    std::cout << "HW_SSSE3: " << HW_SSSE3 << std::endl;
    std::cout << "HW_SSE41: " << HW_SSE41 << std::endl;
    std::cout << "HW_SSE42: " << HW_SSE42 << std::endl;
    std::cout << "HW_SSE4a: " << HW_SSE4a << std::endl;
    std::cout << "HW_AES: " << HW_AES << std::endl;
    std::cout << "HW_SHA: " << HW_SHA << std::endl;
    std::cout << "HW_AVX: " << HW_AVX << std::endl;
    std::cout << "HW_XOP: " << HW_XOP << std::endl;
    std::cout << "HW_FMA3: " << HW_FMA3 << std::endl;
    std::cout << "HW_FMA4: " << HW_FMA4 << std::endl;
    std::cout << "HW_AVX2: " << HW_AVX2 << std::endl;
    std::cout << "HW_AVX512F: " << HW_AVX512F << std::endl;
    std::cout << "HW_AVX512CD: " << HW_AVX512CD << std::endl;
    std::cout << "HW_AVX512PF: " << HW_AVX512PF << std::endl;
    std::cout << "HW_AVX512ER: " << HW_AVX512ER << std::endl;
    std::cout << "HW_AVX512VL: " << HW_AVX512VL << std::endl;
    std::cout << "HW_AVX512BW: " << HW_AVX512BW << std::endl;
    std::cout << "HW_AVX512DQ: " << HW_AVX512DQ << std::endl;
    std::cout << "HW_AVX512IFMA: " << HW_AVX512IFMA << std::endl;
    std::cout << "HW_AVX512VBMI: " << HW_AVX512VBMI << std::endl;

    std::stringstream ss;
    ss << argv[1];

    int value;
    ss >> value;

    int numThreads = 1;
    if(argc > 2)
    {
        ss << argv[2];
        ss >> numThreads;
        omp_set_num_threads(numThreads);
    }

    std::vector<uint32_t> v;

    do
    {
        std::string line;
        std::cout << "> ";
        std::getline (std::cin, line);

        std::stringstream parser;
        parser << line;
        
        std::string cmd;
        parser >> cmd;

        if(cmd == "array")
        {
            size_t size;
            parser >> size;

            // std::cout << ":" << cmd << " " << size << std::endl;

            v = std::vector<uint32_t>(size, 1);
        }

        {
            auto timer = stopwatch<>{};
            std::cout << "..." << std::endl;
            auto total = sum32(v);
            std::cout << total << std::endl;
            std::cout << numThreads << " thread(s), elapsed: " << timer.elapsed<double, std::chrono::seconds::period>() << std::endl;
        }
    }while(true);
}