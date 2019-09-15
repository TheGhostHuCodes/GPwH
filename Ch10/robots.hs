type Name = String

data Robot = Robot { name :: Name
                   , attack :: Integer
                   , hp:: Integer
} deriving (Show)

setName :: Robot -> Name -> Robot
setName r newName = r { name = newName }
setAttack :: Robot -> Integer -> Robot
setAttack r newAttack = r { attack = newAttack }
setHP :: Robot -> Integer -> Robot
setHP r newHP = r { hp = newHP }

damage :: Robot -> Integer -> Robot
damage r attackDamage = r { hp = hp r - attackDamage }

fight :: Robot -> Robot -> Robot
fight attacker defender = damage defender attackDamage
  where attackDamage = if hp attacker > 0 then attack attacker else 0

killerRobot = Robot "Kill3r" 25 200
nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50
gentleGiant = Robot "Mr. Friendly" 10 300

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

fastRobot = Robot "speedy" 15 40
slowRobot = Robot "slowpoke" 20 30

slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
slowRobotRound3 = fight fastRobotRound1 slowRobotRound2
fastRobotRound2 = fight slowRobotRound3 fastRobotRound1

-- Use map on a list of robot objects to get the life of each robot in the list.
life = map hp [killerRobot, nicerRobot, gentlerRobot, softerRobot, gentleGiant]

-- Write a threeRoundFight function that takes two robots and has them fight for
-- three rounds, returning the winner. To avoid having so many different
-- variables for robot state, use a series of nested lambda functions so you can
-- just overwrite robotA and robotB.
threeRoundFight a b = if hp aFinal > hp bFinal then aFinal else bFinal
 where
  (aFinal, bFinal) = (\(r1, r2) -> (fight r2 r1, fight r1 r2))
    ((\(r1, r2) -> (fight r2 r1, fight r1 r2))
      ((\(r1, r2) -> (fight r2 r1, fight r1 r2)) (a, b))
    )

-- Create a list of three robots. Then create a fourth robot. Use partial
-- application to create a closure for the fight method so the fourth robot can
-- fight all three robots at once, using map. Finally, use map to get the
-- remaining life from the rest of the robots.
robots = [nicerRobot, gentlerRobot, softerRobot]
killerFight = fight killerRobot
robotsAttacked = map killerFight robots
remainingLife = map hp robotsAttacked
