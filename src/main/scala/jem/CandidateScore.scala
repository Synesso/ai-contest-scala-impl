package jem

class CandidateScore(source: (Double, Double)) {
  def forTarget(target: Projection) = {
    (target.distanceTo(source) * 5) + (target(target.worst).size / target.regen)
  }
}