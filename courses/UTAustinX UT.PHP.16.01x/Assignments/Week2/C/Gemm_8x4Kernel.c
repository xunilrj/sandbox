#define alpha(i, j) A[(j)*ldA + (i)] // map alpha( i,j ) to array A
#define beta(i, j) B[(j)*ldB + (i)]  // map beta( i,j ) to array B
#define gamma(i, j) C[(j)*ldC + (i)] // map gamma( i,j ) to array C

#include <immintrin.h>

void Gemm_MRxNRKernel(int k, double *A, int ldA, double *B, int ldB,
                      double *C, int ldC)
{
   __m256d gamma_0123_0 = _mm256_loadu_pd(&gamma(0, 0));
   __m256d gamma_4567_0 = _mm256_loadu_pd(&gamma(4, 0));

   __m256d gamma_0123_1 = _mm256_loadu_pd(&gamma(0, 1));
   __m256d gamma_4567_1 = _mm256_loadu_pd(&gamma(4, 1));

   __m256d gamma_0123_2 = _mm256_loadu_pd(&gamma(0, 2));
   __m256d gamma_4567_2 = _mm256_loadu_pd(&gamma(4, 2));

   __m256d gamma_0123_3 = _mm256_loadu_pd(&gamma(0, 3));
   __m256d gamma_4567_3 = _mm256_loadu_pd(&gamma(4, 3));

   for (int p = 0; p < k; p++)
   {
      __m256d beta_p_j;
      __m256d alpha_0123_p = _mm256_loadu_pd(&alpha(0, p));
      __m256d alpha_4567_p = _mm256_loadu_pd(&alpha(4, p));

      beta_p_j = _mm256_broadcast_sd(&beta(p, 0));
      gamma_0123_0 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_0);
      gamma_4567_0 = _mm256_fmadd_pd(alpha_4567_p, beta_p_j, gamma_4567_0);

      beta_p_j = _mm256_broadcast_sd(&beta(p, 1));
      gamma_0123_1 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_1);
      gamma_4567_1 = _mm256_fmadd_pd(alpha_4567_p, beta_p_j, gamma_4567_1);

      beta_p_j = _mm256_broadcast_sd(&beta(p, 2));
      gamma_0123_2 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_2);
      gamma_4567_2 = _mm256_fmadd_pd(alpha_4567_p, beta_p_j, gamma_4567_2);

      beta_p_j = _mm256_broadcast_sd(&beta(p, 3));
      gamma_0123_3 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_3);
      gamma_4567_3 = _mm256_fmadd_pd(alpha_4567_p, beta_p_j, gamma_4567_3);
   }

   _mm256_storeu_pd(&gamma(0, 0), gamma_0123_0);
   _mm256_storeu_pd(&gamma(4, 0), gamma_4567_0);
   _mm256_storeu_pd(&gamma(0, 1), gamma_0123_1);
   _mm256_storeu_pd(&gamma(4, 1), gamma_4567_1);
   _mm256_storeu_pd(&gamma(0, 2), gamma_0123_2);
   _mm256_storeu_pd(&gamma(4, 2), gamma_4567_2);
   _mm256_storeu_pd(&gamma(0, 3), gamma_0123_3);
   _mm256_storeu_pd(&gamma(4, 3), gamma_4567_3);
}

////////////////////////////////////// #reg per colum + (n * reg per colum) + 1
////////////////// ++n => ++ register
//4x1
// void Gemm_MRxNRKernel(int k, double *A, int ldA, double *B, int ldB,
//                       double *C, int ldC)
// {
//    __m256d gamma_0123_0 = _mm256_loadu_pd(&gamma(0, 0));
//    for (int p = 0; p < k; p++)
//    {
//       __m256d beta_p_j;
//       __m256d alpha_0123_p = _mm256_loadu_pd(&alpha(0, p));

//       beta_p_j = _mm256_broadcast_sd(&beta(p, 0));
//       gamma_0123_0 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_0);
//    }

//    _mm256_storeu_pd(&gamma(0, 0), gamma_0123_0);
// }

//4x2
// void Gemm_MRxNRKernel(int k, double *A, int ldA, double *B, int ldB,
//                       double *C, int ldC)
// {
//    __m256d gamma_0123_0 = _mm256_loadu_pd(&gamma(0, 0));
//    __m256d gamma_0123_1 = _mm256_loadu_pd(&gamma(0, 1));
//    for (int p = 0; p < k; p++)
//    {
//       __m256d beta_p_j;
//       __m256d alpha_0123_p = _mm256_loadu_pd(&alpha(0, p));

//       beta_p_j = _mm256_broadcast_sd(&beta(p, 0));
//       gamma_0123_0 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_0);

//       beta_p_j = _mm256_broadcast_sd(&beta(p, 1));
//       gamma_0123_1 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_1);
//    }

//    _mm256_storeu_pd(&gamma(0, 0), gamma_0123_0);
// }

////////////////// ++n => register +=2
//8x1
// void Gemm_MRxNRKernel(int k, double *A, int ldA, double *B, int ldB,
//                       double *C, int ldC)
// {
//    __m256d gamma_0123_0 = _mm256_loadu_pd(&gamma(0, 0));
//    __m256d gamma_4567_0 = _mm256_loadu_pd(&gamma(4, 0));
//    for (int p = 0; p < k; p++)
//    {
//       __m256d beta_p_j;
//       __m256d alpha_0123_p = _mm256_loadu_pd(&alpha(0, p));
//       __m256d alpha_4567_p = _mm256_loadu_pd(&alpha(4, p));

//       beta_p_j = _mm256_broadcast_sd(&beta(p, 0));
//       gamma_0123_0 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_0);
//       gamma_4567_0 = _mm256_fmadd_pd(alpha_4567_p, beta_p_j, gamma_4567_0);
//    }

//    _mm256_storeu_pd(&gamma(0, 0), gamma_0123_0);
//    _mm256_storeu_pd(&gamma(4, 0), gamma_4567_0);
// }

//8x2
// void Gemm_MRxNRKernel(int k, double *A, int ldA, double *B, int ldB,
//                       double *C, int ldC)
// {
//    __m256d gamma_0123_0 = _mm256_loadu_pd(&gamma(0, 0));
//    __m256d gamma_4567_0 = _mm256_loadu_pd(&gamma(4, 0));
//    __m256d gamma_0123_1 = _mm256_loadu_pd(&gamma(0, 1));
//    __m256d gamma_4567_1 = _mm256_loadu_pd(&gamma(4, 1));
//    for (int p = 0; p < k; p++)
//    {
//       __m256d beta_p_j;
//       __m256d alpha_0123_p = _mm256_loadu_pd(&alpha(0, p));
//       __m256d alpha_4567_p = _mm256_loadu_pd(&alpha(4, p));

//       beta_p_j = _mm256_broadcast_sd(&beta(p, 0));
//       gamma_0123_0 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_0);
//       gamma_4567_0 = _mm256_fmadd_pd(alpha_4567_p, beta_p_j, gamma_4567_0);

//       beta_p_j = _mm256_broadcast_sd(&beta(p, 1));
//       gamma_0123_1 = _mm256_fmadd_pd(alpha_0123_p, beta_p_j, gamma_0123_1);
//       gamma_4567_1 = _mm256_fmadd_pd(alpha_4567_p, beta_p_j, gamma_4567_1);
//    }

//    _mm256_storeu_pd(&gamma(0, 0), gamma_0123_0);
//    _mm256_storeu_pd(&gamma(4, 0), gamma_4567_0);
//    _mm256_storeu_pd(&gamma(0, 1), gamma_0123_1);
//    _mm256_storeu_pd(&gamma(4, 1), gamma_4567_1);
// }