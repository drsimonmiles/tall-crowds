import CollectionUtils.chooseWeighted
import indigo.Dice

object HeightDistribution {
  val femaleAverage = 162
  val maleAverage = 175

  // Assumes 70% people are between average heights of female and male equally distributed
  // Curves down either side by +/- 10cm
  val distribution: List[(Int, Double)] = List (
    152 -> 0.001, 153 -> 0.004, 154 -> 0.007, 155 -> 0.010, 156 -> 0.013,
    157 -> 0.017, 158 -> 0.020, 159 -> 0.023, 160 -> 0.026, 161 -> 0.029,
    162 -> 0.05, 163 -> 0.05, 165 -> 0.05, 166 -> 0.05, 167 -> 0.05, 168 -> 0.05,
    169 -> 0.05, 170 -> 0.05, 171 -> 0.05, 172 -> 0.05, 173 -> 0.05, 174 -> 0.05, 175 -> 0.05,
    176 -> 0.029, 177 -> 0.026, 178 -> 0.023, 179 -> 0.020, 180 -> 0.017,
    181 -> 0.013, 182 -> 0.010, 183 -> 0.007, 184 -> 0.004, 185 -> 0.001
  )

  def chooseHeight (dice: Dice): Int = 162
//    chooseWeighted (dice, distribution)
}

// 1 + 4 + 7 + 10 + 13 + 17 + 20 + 23 + 26 + 29 = 150
