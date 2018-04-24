#ifndef __TWO_D_SCENE_H__
#define __TWO_D_SCENE_H__

#include <Eigen/Core>
#include <Eigen/StdVector>

#include "Force.h"

class TwoDScene
{
public:
  
  TwoDScene();
  
  TwoDScene( int num_particles );
  
  TwoDScene( const TwoDScene& otherscene );

  ~TwoDScene();

  int getNumParticles() const;
  int getNumEdges() const;
  int getNumHalfplanes() const;
  
  const VectorXs& getX() const;

  VectorXs& getX();

  const VectorXs& getV() const;
  
  VectorXs& getV();
  
  const VectorXs& getM() const;
  
  VectorXs& getM();
  
  const std::vector<scalar>& getRadii() const;

  void resizeSystem( int num_particles );
  
  void setPosition( int particle, const Vector2s& pos );
  
  void setVelocity( int particle, const Vector2s& vel );

  void setMass( int particle, const scalar& mass );
  
  void setFixed( int particle, bool fixed );

  bool isFixed( int particle ) const;
  
  const scalar& getRadius( int particle ) const;
  void setRadius( int particle, scalar radius );
  
  void clearEdges();

  void clearHalfplanes();
  
  void insertEdge( const std::pair<int,int>& edge, scalar radius );

  void insertHalfplane( const std::pair<VectorXs, VectorXs> &halfplane);
  
  const std::vector<std::pair<int,int> >& getEdges() const;

  // Each halfplane is a pair of two 2D vectors, a position and a normal.
  // The the Theme 02 Milestone 01 PDF for a description of these vectors.
  const std::vector<std::pair<VectorXs, VectorXs> >& getHalfplanes() const;

  const std::pair<VectorXs, VectorXs> &getHalfplane(int idx) const;

  const std::vector<scalar>& getEdgeRadii() const;
  
  const std::pair<int,int>& getEdge(int edg) const;
  
  void insertForce( Force* newforce );

  void accumulateGradU( VectorXs& F, const VectorXs& dx = VectorXs(), const VectorXs& dv = VectorXs() );

  void accumulateddUdxdx( MatrixXs& A, const VectorXs& dx = VectorXs(), const VectorXs& dv = VectorXs() );

  // Kind of a misnomer.
  void accumulateddUdxdv( MatrixXs& A, const VectorXs& dx = VectorXs(), const VectorXs& dv = VectorXs() );
  
  scalar computeKineticEnergy() const;
  scalar computePotentialEnergy() const;
  scalar computeTotalEnergy() const;

  void copyState( const TwoDScene& otherscene );

  void checkConsistency();

  std::vector<std::string>& getParticleTags();
  const std::vector<std::string>& getParticleTags() const;
  
  EIGEN_MAKE_ALIGNED_OPERATOR_NEW
  
private:
  VectorXs m_x;
  VectorXs m_v;
  VectorXs m_m;
  std::vector<bool> m_fixed;
  // Vertex radii
  std::vector<scalar> m_radii;
  std::vector<std::pair<int,int> > m_edges;
  std::vector<std::pair<VectorXs, VectorXs> > m_halfplanes;
  std::vector<scalar> m_edge_radii;
  // Forces. Note that the scene inherits responsibility for deleting forces.
  std::vector<Force*> m_forces;
  // String 'tags' assigned to particles. Can be used to identify and single out particles for special treatment.
  std::vector<std::string> m_particle_tags;
};

#endif
