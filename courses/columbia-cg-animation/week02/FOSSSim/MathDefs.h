#ifndef __MATH_DEFS_H__
#define __MATH_DEFS_H__

#include <Eigen/Core>
#include <Eigen/StdVector>

typedef double scalar;

typedef Eigen::Matrix<scalar, 2, 1> Vector2s;
typedef Eigen::Matrix<scalar, Eigen::Dynamic, 1> VectorXs;

typedef Eigen::Matrix<scalar, 2, 2> Matrix2s;
typedef Eigen::Matrix<scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixXs;

//typedef Matrix<int, 1, 2> RowVector2i;

#endif
