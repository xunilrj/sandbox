#define alpha( i,j ) A[ (j)*ldA + (i) ]   // map alpha( i,j ) to array A
#define beta( i,j )  B[ (j)*ldB + (i) ]   // map beta( i,j ) to array B
#define gamma( i,j ) C[ (j)*ldC + (i) ]   // map gamma( i,j ) to array C

#include<immintrin.h>

void Gemm_MRxNRKernel( int k, double *A, int ldA, double *B, int ldB,
		double *C, int ldC )
{
  /* Declare vector registers to hold 8x6 C and load them */
  __m256d gamma_0123_0 = _mm256_loadu_pd( &gamma( 0,0 ) );
  __m256d gamma_0123_1 = _mm256_loadu_pd( &gamma( 0,1 ) );
  __m256d gamma_0123_2 = _mm256_loadu_pd( &gamma( 0,2 ) );
  __m256d gamma_0123_3 = _mm256_loadu_pd( &gamma( 0,3 ) );
  __m256d gamma_0123_4 = _mm256_loadu_pd( &gamma( 0,4 ) );
  __m256d gamma_0123_5 = _mm256_loadu_pd( &gamma( 0,5 ) );
  __m256d gamma_4567_0 = _mm256_loadu_pd( &gamma( 4,0 ) );
  __m256d gamma_4567_1 = _mm256_loadu_pd( &gamma( 4,1 ) );
  __m256d gamma_4567_2 = _mm256_loadu_pd( &gamma( 4,2 ) );
  __m256d gamma_4567_3 = _mm256_loadu_pd( &gamma( 4,3 ) );
  __m256d gamma_4567_4 = _mm256_loadu_pd( &gamma( 4,4 ) );
  __m256d gamma_4567_5 = _mm256_loadu_pd( &gamma( 4,5 ) );
  __m256d gamma_891011_0 = _mm256_loadu_pd( &gamma( 8,0 ) );
  __m256d gamma_891011_1 = _mm256_loadu_pd( &gamma( 8,1 ) );
  __m256d gamma_891011_2 = _mm256_loadu_pd( &gamma( 8,2 ) );
  __m256d gamma_891011_3 = _mm256_loadu_pd( &gamma( 8,3 ) );
  __m256d gamma_891011_4 = _mm256_loadu_pd( &gamma( 8,4 ) );
  __m256d gamma_891011_5 = _mm256_loadu_pd( &gamma( 8,5 ) );
   	
  for ( int p=0; p<k; p++ ){
    /* Declare vector register for load/broadcasting beta( p,j ) */
    __m256d beta_p_j;
    
    /* Declare vector registersx to hold the current column of A and load
       them with the eight elements of that column. */
    __m256d alpha_0123_p = _mm256_loadu_pd( &alpha( 0,p ) );
    __m256d alpha_4567_p = _mm256_loadu_pd( &alpha( 4,p ) );
    __m256d alpha_891011_p = _mm256_loadu_pd( &alpha( 8,p ) );

    /* Load/broadcast beta( p,0 ). */
    beta_p_j = _mm256_broadcast_sd( &beta( p, 0) );
    
    /* update the first column of C with the current column of A times
       beta ( p,0 ) */
    gamma_0123_0 = _mm256_fmadd_pd( alpha_0123_p, beta_p_j, gamma_0123_0 );
    gamma_4567_0 = _mm256_fmadd_pd( alpha_4567_p, beta_p_j, gamma_4567_0 );
    gamma_891011_0 = _mm256_fmadd_pd( alpha_891011_p, beta_p_j, gamma_891011_0 );
    
    /* REPEAT for second, third, and fourth columns of C.  Notice that the 
       current column of A needs not be reloaded. */

    /* Load/broadcast beta( p,1 ). */
    beta_p_j = _mm256_broadcast_sd( &beta( p, 1) );
    
    /* update the second column of C with the current column of A times
       beta ( p,1 ) */
    gamma_0123_1 = _mm256_fmadd_pd( alpha_0123_p, beta_p_j, gamma_0123_1 );
    gamma_4567_1 = _mm256_fmadd_pd( alpha_4567_p, beta_p_j, gamma_4567_1 );
    gamma_891011_1 = _mm256_fmadd_pd( alpha_891011_p, beta_p_j, gamma_891011_1 );

    /* Load/broadcast beta( p,2 ). */
    beta_p_j = _mm256_broadcast_sd( &beta( p, 2) );
    
    /* update the third column of C with the current column of A times
       beta ( p,2 ) */
    gamma_0123_2 = _mm256_fmadd_pd( alpha_0123_p, beta_p_j, gamma_0123_2 );
    gamma_4567_2 = _mm256_fmadd_pd( alpha_4567_p, beta_p_j, gamma_4567_2 );
    gamma_891011_2 = _mm256_fmadd_pd( alpha_891011_p, beta_p_j, gamma_891011_2 );

    /* Load/broadcast beta( p,3 ). */
    beta_p_j = _mm256_broadcast_sd( &beta( p, 3) );
    
    /* update the fourth column of C with the current column of A times
       beta ( p,3 ) */
    gamma_0123_3 = _mm256_fmadd_pd( alpha_0123_p, beta_p_j, gamma_0123_3 );
    gamma_4567_3 = _mm256_fmadd_pd( alpha_4567_p, beta_p_j, gamma_4567_3 );
    gamma_891011_3 = _mm256_fmadd_pd( alpha_891011_p, beta_p_j, gamma_891011_3 );

    /* Load/broadcast beta( p,4 ). */
    beta_p_j = _mm256_broadcast_sd( &beta( p, 4) );
    
    /* update the fifth column of C with the current column of A times
       beta ( p,4 ) */
    gamma_0123_4 = _mm256_fmadd_pd( alpha_0123_p, beta_p_j, gamma_0123_4 );
    gamma_4567_4 = _mm256_fmadd_pd( alpha_4567_p, beta_p_j, gamma_4567_4 );
    gamma_891011_4 = _mm256_fmadd_pd( alpha_891011_p, beta_p_j, gamma_891011_4 );

    /* Load/broadcast beta( p,5 ). */
    beta_p_j = _mm256_broadcast_sd( &beta( p, 5) );
    
    /* update the sixth column of C with the current column of A times
       beta ( p,5 ) */
    gamma_0123_5 = _mm256_fmadd_pd( alpha_0123_p, beta_p_j, gamma_0123_5 );
    gamma_4567_5 = _mm256_fmadd_pd( alpha_4567_p, beta_p_j, gamma_4567_5 );
    gamma_891011_5 = _mm256_fmadd_pd( alpha_891011_p, beta_p_j, gamma_891011_5 );
  }
  
  /* Store the updated results */
  _mm256_storeu_pd( &gamma(0,0), gamma_0123_0 );
  _mm256_storeu_pd( &gamma(0,1), gamma_0123_1 );
  _mm256_storeu_pd( &gamma(0,2), gamma_0123_2 );
  _mm256_storeu_pd( &gamma(0,3), gamma_0123_3 );
  _mm256_storeu_pd( &gamma(0,4), gamma_0123_4 );
  _mm256_storeu_pd( &gamma(0,5), gamma_0123_5 );
  _mm256_storeu_pd( &gamma(4,0), gamma_4567_0 );
  _mm256_storeu_pd( &gamma(4,1), gamma_4567_1 );
  _mm256_storeu_pd( &gamma(4,2), gamma_4567_2 );
  _mm256_storeu_pd( &gamma(4,3), gamma_4567_3 );
  _mm256_storeu_pd( &gamma(4,4), gamma_4567_4 );
  _mm256_storeu_pd( &gamma(4,5), gamma_4567_5 );
  _mm256_storeu_pd( &gamma(8,0), gamma_891011_0 );
  _mm256_storeu_pd( &gamma(8,1), gamma_891011_1 );
  _mm256_storeu_pd( &gamma(8,2), gamma_891011_2 );
  _mm256_storeu_pd( &gamma(8,3), gamma_891011_3 );
  _mm256_storeu_pd( &gamma(8,4), gamma_891011_4 );
  _mm256_storeu_pd( &gamma(8,5), gamma_891011_5 );
}
