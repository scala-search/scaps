package scala.tools.apiSearch.evaluation

import com.nicta.rng._
import com.nicta.rng.SetSeed

object RngExtensions {
  implicit class RichRng[A](rng: Rng[A]) {
    def runUnsafe(seed: Long): A =
      (for {
        _ <- Rng.setseed(seed)
        r <- rng
      } yield r).run.unsafePerformIO()
  }

  def normalSample(mean: Double, variance: Double): Rng[Double] = {
    // Box–Muller method
    Rng.choosedouble(0, 1).fill(2) map { u =>
      val z = math.sqrt(-2 * math.log(u(0))) * math.cos(2 * math.Pi * u(1))
      mean + math.sqrt(variance) * z
    }
  }
}
