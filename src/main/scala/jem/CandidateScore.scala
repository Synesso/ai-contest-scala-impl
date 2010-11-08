package jem

class CandidateScore(source: (Double, Double)) {
  def forTarget(target: Planet) = {
    (target.distanceTo(source) * 5) + (target.size / target.regen)
  }
}