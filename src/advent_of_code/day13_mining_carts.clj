(ns advent-of-code.day13-mining-carts
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(def input (str/split-lines "                                           /-----------------------------------------------------------------\\                                        \n  /----------------------\\/----------------+-------------------------------------------------------\\    /----+----\\                                   \n  |                      ||                |   /----------------------------\\                      |    | /--+----+---------------------------------\\ \n /+----------\\    /------++----------------+---+-----------\\                |   /------------------+----+-+--+----+---------------------------\\     | \n ||          |    |/-----++----------------+---+-----------+--------\\       |   |                  |    | |  |    |                           |     | \n ||          |    ||     ||                |   |    /------+-------\\|       |   |            /-----+----+\\|  |    |                  /--------+----\\| \n ||          |    ||     ||         /------+---+----+------+-------++-------+---+------------+-----+----+++--+----+\\                 |        |    || \n ||     /----+----++--\\  ||         |      |   |    |      |       ||     /-+---+------------+-----+----+++--+----++------\\          |        |    || \n ||     |    |    ||  |  ||         |      |   |    |      |       ||     | |   |            |     |    |||  |    ||      |          |        |    || \n ||     |    |    ||  |  ||     /---+------+---+----+------+-------++-----+-+---+------------+-----+----+++--+----++------+-----\\    |        |    || \n ||     |    |    ||  | /++-----+---+--\\   |   |    |      |       ||     | |   |            |     |    |||  |    ||      |     |    |        |    || \n ||     |    |    ||  | |||     |   |  |   \\---+----+------+-------++-----+-+---+------------+-----+----+++--/    ||      |     |    |        |    || \n ||  /--+----+----++--+-+++-----+---+--+-------+----+------+------\\||     | |   |         /--+-----+----+++-------++------+-----+----+--------+--\\ || \n ||  |  |    |    ||  | |||     |   |  v       |    |      |      |||     | |   |         |  |     |    |||       ||      |     |    |        |  | || \n ||  |  |/---+----++--+-+++-----+---+--+-------+----+------+------+++----\\| |   |         |  |     |    |||       ||      |     |    \\--------+--+-/| \n \\+--+--++---/    |\\--+-+++-----+---+--+-------+----+------+------++/ /--++-+---+---------+--+---\\ |    |||       ||      |     |             |  |  | \n  |  |  ||        |   | |||     |   |  |       |    |   /--+------++--+--++-+---+------\\  |  |   | |    |||       ||      |     |             |  |  | \n  |  |  ||        |/--+-+++-----+---+--+-------+----+---+--+---\\  ||  |  || |   |      |  |  |   | |    |||       ||      |   /-+-------------+\\ |  | \n  |/-+--++--------++--+-+++-----+---+--+-------+----+---+--+--\\|  ||  |  || |   |      |  |  |   | |    |||       ||      |   | |             || |  | \n  || |  ||        ||  | |||     |   |  |       |    \\---+--+--++--+/  |  || |   \\------+--+--+---+-+----+++-------++------+---+-+-------------/| |  | \n  || |  ||        ||  | |||/----+---+--+-------+--------+--+--++--+---+--++-+--->------+--+--+---+-+----+++-------++------+---+-+--\\           | |  | \n  || |  ||        ||  | ||||    \\---+--+-------+--------+--+--++--+---+--++-+----------+--+--+---+-+----+++-------++------+---+-/  |           | |  | \n  || |  ||        ||  | ||||        |  |   /--<+----\\   |  |  ||  |   |  |\\-+----------+--+--+---+-+----+++-------++------/   |    |           | |  | \n  || |  \\+--------++--/ ||||        |  |   |  /+----+---+--+--++--+---+\\ |  |          |  |  |   | |    |||       ||          |    |           | |  | \n  || |   |/-------++----++++--------+--+---+--++---\\|   |/-+--++--+---++-+--+--------\\ | /+--+---+-+----+++-------++----------+----+-----\\     | |  | \n  || |   ||       ||    ||||        |  |   |  ||   ||/--++-+--++--+---++-+--+--------+-+-++--+---+-+----+++-------++----------+----+-----+-----+-+--+\\\n  || |   ||       ||    ||||        |  |   |  ||   |||  || |  ^|  |   || |  |        | | ||  |   | |    |||       ||          |    |     |     | |  ||\n  || |   ||       ||    ||||       /+--+---+--++---+++--++-+--++--+---++-+--+--------+-+-++--+---+-+----+++-------++----------+----+-----+---\\ | |  ||\n  || |   ||       ||    ||||   /---++--+---+--++---+++--++-+--++--+---++-+--+--------+-+-++--+---+-+----+++------\\||          \\----+-----+---+-/ |  ||\n  || |   ||       ||    ||||   |/--++--+---+--++---+++--++-+--++--+---++-+--+--------+-+-++--+---+-+----+++------+++---------------+---\\ |   |   |  ||\n  || |   ||       ||    ||||   ||  || /+---+--++---+++--++-+--++--+---++-+--+--------+-+-++--+---+-+----+++------+++---------------+---+-+---+-\\ |  ||\n  \\+-+---++-------++----+/||   ||  || ||   |  ||   |||  || |  ||  |   || |  |        | | ||  | /-+-+----+++------+++---------\\     |   | |   | | |  ||\n   | |   ||     /-++----+-++---++--++-++---+--++---+++--++-+--++--+---++-+--+--------+-+-++--+-+-+-+\\   |||      |||         |     |   | |   | | |  ||\n   | |   ||/----+-++----+-++---++--++-++---+--++---+++--++-+--++--+---++-+--+--------+-+-++--+-+-+-++---+++----\\ |||         |     |   | |   | | |  ||\n   | |   |||    | ||    | ||   ||  || ||   |  ||   |||  || |  ||  |   || |  |   /----+-+-++--+-+-+-++---+++----+-+++--\\      |     |   | |   | | |  ||\n   | |   |||    | ||    | ||   ||  || ||   |  |\\---+++--++-+--++--+---++-+--/   | /--+-+-++-\\| | | ||   |||    | |||  |      |     |   | |   | | |  ||\n   | |   |||    | ||    | ||   ||  || ||   |  |    |||  || |/-++--+---++-+------+-+--+-+-++-++-+-+-++---+++----+-+++--+------+-----+---+-+\\  | | |  ||\n   | |   |||    | ||   /+-++---++--++-++---+--+----+++--++-++-++--+---++-+------+-+--+-+-++-++-+-+-++---+++-\\  | |||  |      |     |   | ||  | | |  ||\n  /+-+---+++----+-++---++-++---++--++-++---+--+-\\  |||  || || ||  |   || |      | |  | | || || | | ||   ||| |  | |||  |      |     |   | ||  | | |  ||\n  || |   |||    | ||   || ||   ||  || ||   |  | |  |||  || || ||  ^  /++-+------+-+--+-+-++-++-+-+-++--\\||| |  | |||  |      |     |   | ||  | | |  ||\n  || |   |||    | ||   || ||   ||  || ||/--+--+-+--+++--++-++-++--+--+++-+------+-+--+-+-++-++-+-+-++--++++-+-\\| |||  |      |     |   | ||  | | |  ||\n  || |   |||    | ||   || ||   ||  || |||  |/-+-+--+++--++-++-++--+--+++-+------+-+--+-+-++-++-+-+-++--++++-+-++-+++--+------+-----+---+-++-\\| | |  ||\n  || |   |||    | ||   || ||   ||  || |||  || | |  |||  |\\-++-++--+--+++-+------+-+--/ | || || | | ||  |||| | || |||  |      |     |   | || || | |  ||\n  || |   |||    | ||   || ||/--++--++-+++--++-+-+--+++--+--++-++--+--+++-+------+-+---\\| || || | |/++--++++-+-++-+++--+------+---\\ | /-+-++-++-+-+-\\||\n  || |   |\\+----+-++---++-+++--++--++-+++--++-+-+--/|| /+--++-++--+--+++-+------+-+--\\|| || || | ||||  |||| | || |||  |      |   | | | | || || | | |||\n/-++-+---+-+----+-++---++-+++--++--++-+++--++-+-+---++-++--++-++--+--+++-+------+-+--+++-++-++-+-++++-\\|||| | || |||  |      |   | | | | || || | | |||\n| || |   | |    | \\+---++-+++--++--++-+++--++-+-+---++-++--/| ||  |  ||| |      | |  ||| || || | |||| ||||| | || |||  |      |   | | | | || || | | |||\n| || |   | |    |  \\---++-+++--++--++-+++--++-+-+---++-++---+-+/  |  ||| |      | |  ||| || || | |||| ||||| | || |||  |      |   | | | | || || | | |||\n| || |   | |   /+------++-+++--++--++-+++--++-+-+---++-++---+-+--\\|  |\\+-+------+-+--+++-++-++-+-/||| ||||| | || |||  |      |   | | | | || || | | |||\n| || |   | |   ||      || |||  ||  || |||  || | |   || ||   | |  ||  | | |      | |  ||| || |\\-+--+++-+++/| | || |||  |      |   | | | | || || | | |||\n| || |   | |   ||      || |||  ||  || |||  || | |   || ||   | |  ||  | | |      | |  ||| || |  |  ||| ||| | | || |||  |   /--+---+-+-+-+-++\\|| | | |||\n| || |   | |   ||  /---++-+++--++--++-+++--++-+-+---++-++---+-+--++\\ | | |     /+-+--+++\\|| |  |  ||| ||| | | || |||  |/--+--+---+\\| | | ||||| | | |||\n| || |   | |   ||  |   || |||  ||  || ||| /++-+-+---++-++---+-+--+++-+-+-+-----++-+--++++++-+--+--+++-+++-+-+-++-+++--++--+--+\\  ||| | | ||||| | | |||\n| || |   | |   ||  |   || |||  ||/-++-+++-+++-+-+---++-++---+-+--+++-+-+-+-----++-+\\ |||||| |  |  ||| ||| \\-+-++-+++--++--+--++--+++-+-+-+++++-+-+-+/|\n| || |/--+-+---++--+---++-+++--+++-++-+++-+++-+-+---++-++---+-+--+++-+-+-+-----++-++-++++++-+-\\|  ||| |||   | || |||  ||  |  ||  ||| | | ||||| | | | |\n| || ||  | |   || /+---++-+++--+++-++-+++-+++-+-+---++-++---+-+--+++-+-+-+-----++-++-++++++-+-++--+++-+++-\\ | || |||  ||  |  ||  ||| | | ||||| | | | |\n| || ||  | |   || ||   ||/+++--+++-++-+++-+++-+-+---++-++---+-+>-+++-+-+-+-----++-++-++++++-+-++--+++-+++-+-+-++-+++--++--+--++\\ ||| | | ||||| | | | |\n| || ||  | |   || ||   ||||||  ||| || ||| |\\+-+-+---/| ||   | |  ||| | | |     || || |||||| | ||  ||| ||| | | || |||  ||  |  ||| ||| | | ||||| | | | |\n| || ||  | |   || ||   ||||||  |\\+-++-+++-+-+-+-+----+-++---+-+--+++-+-+-+-----++-++-++++++-+-++--+++-+++-+-+-++-+++--++--+--+++-+++-+-/ ||||| | | | |\n| || ||  | |   || ||   ||||||  | | || ||| | | | |    | ||   | |  ||| | | |     || || ||||||/+-++--+++-+++-+-+-++-+++--++\\ |  ||| ||| |   ||||| | | | |\n| || ||  | |   || ||   ||||\\+--+-+-++-+++-+-+-+-+----+-++---+-+--+++-+-+-+---->++-++-++++++++-++--+++-+++-+-+-++-+++--+++-+--+++-++/ |   ||||| | | | |\n| || ||  | \\---++-++---++++-+--+-+-++-+++-+-+-+-+----+-++---+-+--+++-+-+-+-----++-++-++++++++-++--+++-+++-+-+-+/ |||  ||| |  ||| ||  |   ||||| | | | |\n| || ||  | /---++-++\\  |||| |  | | || ||| | | | |    |/++---+-+--+++-+-+-+-----++-++-++++++++-++--+++-+++-+-+-+--+++--+++-+--+++-++--+---+++++\\| | | |\n| || ||  | |   || |\\+--++++-+--+-+-++-+++-+-+-+-+----++++---+-+--++/ | | |     || || |||||||| ||  ||| ||| | | |  |||  ||| |  ||| ||  |   ||||||| | | |\n| || ||  | |   || | |  |||| |  | | || ||| \\-+-+-+----++++---+-+--++--+-+-+-----++-++-++++++++-++--+++-+++-+-+-+--+++--+++-+--+/| ||  |   ||||||| | | |\n| || ||  | |   || | |  |||\\-+--+-+-++-+++---+-+-+----++++---+-+--++--+-+-+-----++-++-++++++++-++--+/| ||| | v |  |||  ||| |  | | ||  |   ||||||| | | |\n| || ||  | |   || | |  |||  |/-+-+-++-+++---+-+-+----++++---+-+--++--+\\| |     || || |||||||| ||  | | ||| | | |  |||  ||| |  | | ||  |   ||||||| | | |\n| || ||  | |   || | |  ||| /++-+-+-++-+++---+-+-+----++++---+-+\\ ||  ||| | /---++-++-++++++++-++--+-+-+++-+-+\\|  |||  ||| |  | | ||  |   ||||||| | | |\n| ||/++--+-+---++-+-+--+++-+++-+-+-++-+++---+-+-+----++++--\\| || ||  ||| | |   || || |||||||| ||  | | ||| | |||  |||  ||| |  | | ||  |   ||||||| | | |\n| |||||  | |   |\\-+-+--+++-+++-+-+-++-+++---+-+-+----++++--++-++-++--+++-+-+---++-++-++++++++-++--+-/ ||| | |||  |||  ||| |  | | ||  |   ||||||| | | |\n| |||||  | |   |  | |  ||| ||| | | || |||   | | |    ||||  || || ||  ||| | |   || || ||||\\+++-++--+---+++-+-+++--+++--+++-+--+-+-++--+---/|||||| | | |\n| |||||  | |   |  | |  ||| ||| | | || |||   | | |    ||\\+--++-++-++--+++-+-+---++-++-/||| ||| |\\--+---+++-+-+++--+++--+++-+--/ | ||  |    |||||| | | |\n| |||||  | |   | /+-+--+++-+++-+-+-++-+++---+-+-+----++-+-\\|| || ||  ||| | |   || ||  ||| ||| |   |   ||| | |||  |||  ||| |    | ||  |    |||||| | | |\n| |||||  | |   | || |  ||| ||| | | || |||   | | |    || | ||| || ||  ||| | |   || ||  ||| ||| | /-+---+++-+-+++--+++--+++-+----+-++\\ \\----++++++-+-/ |\n| |||||  | |   | || |  ||| ||| | | || |||   | | |    || | ||| || ||  ||| | |   || ||  ||| \\++-+-+-+---+++-+-+++--+++--+++-+----+-+++------++++++-/   |\n| |||||  | |   | || |  ||| ||| | | || |||   | \\-+----++-+-+++-++-++--++/ | |   || ||  |||  || | | |   ||| | |||  |||  ||| |    | |||      ||||||     |\n| |||||  | |   | || |  ||| ||| | | || |||   |   |    || | ||| || ||  ||  | |   || ||  |||  || | | |   ||| | |||  |||  ||| |    | |||      ||||||     |\n| |||||  | |   | || |  ||| |v| | | || |||  /+---+----++-+-+++-++-++--++--+-+---++-++--+++--++-+-+-+---+++-+-+++--+++--+++-+\\   | |||      ||||||     |\n| |||||  \\-+---+-++-+--+++-+++-+-+-++-+++--++---+----++-+-+++-++-++--++--/ |   || ||  |||  || | | |   ||| | |||  |||  ||| ||   | |||      ||||||     |\n| |||||    |   | || |  ||| ||| | | || |||  |\\---+----++-+-+++-++-++--++----+---++-++--+++--++-+-+-+---+++-+-+++--+++--+++-++---+-+++------++/|||     |\n| |||||    |   | || |  ||| ||| | | || |||  |    |    || | ||| || ||  ||/---+---++-++--+++--++\\| | |   ||| | |||  |||  ||| ||   | |||      || |||     |\n\\-+++++----+---+-++-+--+++-+++-+-+-++-+++--+----+----++-+-+++-++-++--+++---+---++-++--+++--++++-+-+---/|| | |||/-+++--+++-++---+-+++------++-+++\\    |\n  |||||    |   | || |  ||| |||/+-+-++-+++--+----+--\\ || | ||| || ||  |||   |   |\\-++--+++--++++-+-+----++-+-++++-+++--/|| ||   | |||      || ||||    |\n  |||\\+----+---+-++-+--+++-+++++-+-++-+++--+----+--+-++-+-+++-++-+/  |||   |   |  ||  |||  |||| | |    || | |||| |||   || ||   | |||      || ||||    |\n  ||| |    |   | || | /+++-+++++-+-++-+++--+----+--+-++-+-+++-++-+---+++---+---+-\\||  |||  |||| | |    || | |||| |||   || ||   | |||      || ||||    |\n  ||| |    |   | || | |||| ||||| | || |||  |    |/-+-++-+-+++-++-+---+++---+---+-+++--+++--++++-+-+----++-+-++++-+++---++-++--\\| |||      || ||||    |\n  ||| |    \\---+-++-/ ||\\+-+++++-+-++-+/|  |    || | |\\-+-+++-++-+---+++---+---+-+++--+++--++++-+-+----++-+-++++-+++---++-++--++-+++------++-+/||    |\n/-+++-+--------+-++-\\ || | ||||| | || | |  |    || |/+--+-+++-++-+---+++---+<--+\\|||/-+++--++++-+-+----++-+-++++-+++---++-++--++-+++\\     || | ||    |\n| ||| |        | || | || | ||||| | |\\-+-+--+----++-+++--+-+++-++-+---+++---+---++++++-+++--++++-+-+----++-+-++++-++/   || ||  || ||||     || | ||    |\n| ||| |        | || | || | ||\\++-+-+--+-+--+----++-+++--+-+++-++-+---+/|   |   |||||| |||  |||| | |    || | |||| ||    || ||  || ||||     || | ||    |\n| ||| |        | || | || | || |\\-+-+--+-+--+----++-+++--+-+++-++-+---+-+---+---++++++-+++--++++-+-+----++-+-++++-/|    || ||  || ||||     || | ||    |\n| ||| |        | || | || | || |  | |  | |  |    || ||| /+-+++-++-+---+-+---+---++++++-+++--++++-+-+--\\ || | ||||  |    || ||  || ||||     || | ||    |\n| ||| |        | || | || \\-++-+--+-+--+-+--+----++-+++-++-+++-++-+---+-+---+---++++++-+++--++++-+-+--+-++-+-++++--+----++-++--+/ ||||     || | ||    |\n| ||| |        | || | ||   || |  | |  | |  |    || ||\\-++-+++-++-+---+-+---+---++++++-+++--++++-+-+--+-++-+-++++--+----++-++--+--++++-->--++-+-++----/\n|/+++-+-------\\| || | ||   || |  | \\--+-+--+----++-++--++-+++-++-+---+-+---+---++++++-+++--++++-+-+--+-++-+-++++--+----++-++--+--++++-----++-/ ||     \n||||| |       || |\\-+-++---++-+--+----+-+--+----++-++--++-+++-++-+---+-+---+---++++++-+++--++++-+-+--+-++-/ ||||  |    || ||  |  ||||     ||   ||     \n||||| |       || |  | ||   || |  |    | |  |    || ||  || ||| || |   | |   |   |||||| |||  |||| | |  | ||   ||||  |    || ||  |  ||||     ||   ||     \n||||| |       || |  | ||   || |  |    | |  |    || ||  || ||| || |   | \\---+---++++++-+++--++/| | |  | ||   ||||  |    || ||  |  ||||     ||   ||     \n||||| |       || |  | ||   || |  | /--+-+>-+----++-++--++-+++-++-+---+-----+---++++++-+++--++-+-+-+--+-++---++++--+----++-++--+--++++---\\ ||   ||     \n||||| |       || |  | ||   || |  | |/-+-+--+----++\\||  || ||| || |   |     |   |||||| |||  || | | |  | ||/--++++<-+----++-++--+--++++---+-++---++---\\ \n||||| |       || |  | ||   || | /+-++-+-+--+----+++++--++-+++-++-+---+-----+---++++++\\|||  || | | |  | |||  ||||  |    || ||  |  ||||   | ||   ||   | \n||||| |       || | /+-++---++-+-++-++-+-+--+-\\  |||||  || ||| || |   |     |   ||||||||||  || | | |  | |||  ||||  |    ||/++--+--++++---+-++--\\||   | \n||||| |       || | || ||/--++-+-++\\|| |/+--+-+--+++++--++-+++-++-+---+-\\   |   ||||||||||  || | | |  | |||  ||||  |    |||||  |  ||||   | ||  |||   | \n||||| |       || | || |\\+--++-+-+++++-+++--+-+--+++++--++-+++-++-+---+-+---+---++++++++++--++-+-+-+--+-+++--/|||  |    |||||  |  ||||   | |^  |||   | \n|||\\+-+-------++-+-++-+-+--++-+-+++++-+++--+-+--+++++--++-+++-/| |   | |   |/--++++++++++--++-+-+-+--+-+++---+++--+----+++++--+--++++---+-++-\\|||   | \n||| | |       || | || | |  ||/+-+++++-+++--+-+--+++++--++-+++--+-+---+-+---++--++++++++++\\ || | | |  | |||   ||\\--+----+++++--+--++++---+-++-+++/   | \n||| | |       || | || | |  |||| ||||| \\++--+-+--+++++--++-+++--+-+---+-+---++--+++++++++++-++-+-+-+--+-+++---++---+----+++++--+--++++---+-++-++/    | \n||| | |       || | || | |  |||| |||||  ||  | |  |||||  |\\-+++--+-+---+-+---++--++++++++/|| || | |/+--+-+++---++---+----+++++--+--++++-\\ | || ||     | \n||| | |       || | || | |  |||| |||||  ||  | |  |||||  |  |||  | |   | |   ||  |||||||| || || | |||  | |\\+---++---/    |||||  |  |||| | | || ||     | \n||| | |       || | || | |  |||| |||||  ||  | |  |||||  |  |||  | |   | |   ||  |||||||| || || | |||  | | |   ||        \\++++--+--+/|| | | || ||     | \n||| | |       || | || | |  |||| |||||  |\\--+-+--+++++--+--+++--+-+---+-+---++--++++++++-++-++-+-+++--+-+-+---+/         ||||  |  | || | | || ||     | \n||| | |       || | || | |  |||| |||||  |   v |  ||||\\--+--+++--+-+---+-+---++--+/|||||| || || | ||\\--+-+-+---+----------++++--+--/ || | | || ||     | \n||| | |       || | || | |  |||| |||||  |   | |  ||||   |  |||  | |   | |   ||  | |||||| || || | ||   | | |/--+-----\\    ||\\+--+----++-+-+-+/ ||     | \n||| | |       || | || | |  |||\\-+++++--+---+-+--+++/   |  |||  | |   | |   ||  | |||||| || || | ||   | | ||  |     |    || |  |    || | | |  ||     | \n||| | |       || | || | |  |||  |||||  |   | |  |||    |  |||  | |   | |   ||  | |||\\++-++-++-+-++---+-+-++--+-----+----++-+--+----+/ | | |  ||     | \n||| | |       || | || | |  |||  ||||| /+---+-+--+++----+--+++--+-+---+-+---++-\\| ||| || || || | ||   | | ||  |     |    || |  |    |  | | |  ||     | \n||| | |       || | || | |  ||\\--+++++-++---+-+--+++----+--+++--+-+---+-+---++-++-+++-++-+/ || | |\\---+-+-++--+-----+----++-+--+----+--/ | |  ||     | \n||| | |       || | || | |  ||   |\\+++-++---+-+--+++----+--+++--+-+---+-+---++-++-++/ || |  || | |    | | ||  |     |    || |  |    |    | |  ||     | \n||| | \\-------++-+-++-+-+--++---+-+++-++---+-+--+++----+--+++--+-+---+-+---++-++-++--++-+--++-/ |    | | ||  |     |    |\\-+--+----+----+-+--+/     | \n||| |         |\\-+-++-+-+--++---+-+++-++---+-+--+++----+--+++--+-/   | |   || || |\\--++-+--+/   |    | | ||  |     |    |  |  |    |    | |  |      | \n||| |         |  | || | |  ||   | ||| ||   | |  |||    |  |||  |     | |   || || |   || |  \\----+----+-+-++--+-----+----/  |  |    |    | |  |      | \n||| |         |  | || | |  ||   | ||| ||   | |  |||    |  |||  |     | |   || || |   || |      /+----+-+-++--+-----+-------+--+----+--\\ | |  |      | \n||| |  /------+--+-++-+-+--++---+-+++-++---+-+--+++----+--+++--+-----+-+---++-++-+---++-+------++----+-+-++--+-\\   |       |  |    |  | | |  |      | \n||| |  |      |  | || | |  |\\---+-+++-++---+-+--+++----+--+++--+-----+-+---++-++-+---+/ |      ||    | | ||  | |   |       |  |    |  | | |  |      | \n||| |  |      |  | || | |  |    | ||| ||   | |  |||    |  |||  |     | |   || || |   |  |      ||    | | \\+--+-+---+-------+--+----+--+-+-+--+------/ \n||| |  |      |  | || | |  |    | ||| ||   | |  |||    |  |||  |     | |   || || |   |  |      ||    | |  |  | |   |       |  |    |  | | |  |        \n||| |  |      |  | || | |  |    | ||| ||   | |  |||    |  |||  |     \\-+---++-++-+---+--+------++----+-/  |  | |   |       |  |    |  | | |  |        \n||| |  |      |  | || | |  |    | ||| ||   | |  |||    |  |||  |       |   || || |   |  |      |\\----+<---+--+-+---+-------+--+----/  | | |  |        \n|\\+-+--+------/  | || | |  |    | ||| ||   | |  |||    |  |||  |       |   || || | /-+--+------+-----+----+\\ | |   |       |  |       | | |  |        \n| | |  |         | || | |  |    | ||| ||   | |  |||    |  |||  |       |   || || | | |  |      |     |    || | |   |       |  |       | | |  |        \n| | |  |         | || | |  |    | ||| ||   | |  |||    |  |||  |       |   |\\-++-+-+-+--+------+-----+----++-+-+---+-------+--+-------+-+-+--/        \n| | |  |         | || | |  |    | ||| ||   | |  |||    \\--+++--+-------+---+--++-+-+-+--+------+-----/    || | |   |       |  |       | | |           \n| | |  |         | || | |  \\----+-+++-++---+-+--+++-------+++--/       |   \\--++-+-+-+--+------+----------++-/ |   |       |  |       | | |           \n\\-+-+--+---------+-+/ | |       | |\\+-++---+-+--+++-------+++----------+------++-+-+-+--+------+----------++---+---+-------+--+-------+-/ |           \n  | |  |         | |  | \\-------+-/ | |\\---+-+--+++-------+++----------/      || | | |  |      \\----------++---+---+-------+--+-------/   |           \n  | |  |         | |  |         |   | \\----+-+--+++-------+++-----------------/| | | |  |                 ||   |   |      /+--+---------\\ |           \n  | |  |         | \\--+---------+---+------+-//-+++-------+++------------------+-+-+-+--+-----------------++---+---+------++--+---------+\\|           \n  | |  |         |    |         |   |      |  | |||       |||                  | | | |  |                 ||   |   |      ||  |         |||           \n  | \\--+---------+----+---------+---+------+--+-+++-------+/|                  | | | |  |                 \\+---+---/      ||  |         |||           \n  |    |         |    |         |   |      |  | |v|       | |                  | | | |  |                  |   |          ||  |         |||           \n  |    |         \\----+---------+---+------+--+-+++-------/ |                  | | | |  |                  |   |          ||  |         |||           \n  |    |              |         |   |      \\--+-+++---------+------------------+-+-+-+--+------------------+---+----------+/  |         |||           \n  |    |              |         |   |         | |\\+---------+------------------+-+-+-+--+------------------+---+----------+---/         |||           \n  |    |              |         |   \\---------+-+-/         \\------------------+-+-+-+--+------------------+---+----------+-------------++/           \n  |    |              |         \\-------------+-+------------------------------+-+-+-/  |                  |   |          |             ||            \n  \\----+--------------+-----------------------+-/                              | | \\----+------------------/   |          |             ||            \n       \\--------------+-----------------------+--------------------------------+-+------+----------------------/          |             ||            \n                      \\-----------------------+--------------------------------+-/      |                                 |             ||            \n                                              \\--------------------------------+--------+---------------------------------+-------------+/            \n                                                                               \\--------/                                 \\-------------/             "))

(defn get-coordinate [rail-map [x y]]
  (if (or (< x 0) (< y 0)
          (<= (count (first rail-map)) x) (<= (count rail-map) y))
    \space
    (nth (nth rail-map y) x)))

(def three-sixty [\^ \> \v \<])

(def cart-chars (into #{} three-sixty))

(defn match
  {:test (fn []
           (is (not (match [" | "
                            "- -"
                            " | "]
                           [" | "
                            "   "
                            " + "])))
           (is (match [" | "
                       ". ."
                       " | "]
                      [" | "
                       "   "
                       " + "])))}
  [pattern sample]
  (every? true?
          (map (fn [sample-char pattern-char]
                 (or (= sample-char pattern-char)
                     (= pattern-char \.)
                     (and (= pattern-char \-)
                          (or (= sample-char \-)
                              (= sample-char \+)))
                     (and (= pattern-char \|)
                          (or (= sample-char \|)
                              (= sample-char \+)))))
               (str/join sample)
               (str/join pattern))))

(comment (match [" | "
                 "   "
                 " + "]
                [" | "
                 "- -"
                 " | "])
         (match [" | "
                 "   "
                 " + "]
                [" | "
                 ". ."
                 " | "]))

(defn spy [x] (clojure.pprint/pprint x) x)

(defn deduce-rail-char-for-coord
  {:test (fn [] (is (= (deduce-rail-char-for-coord [" | "
                                                    "   "
                                                    "-+-"]
                                                   [1 1])
                       \|)))}
  [input [x y]]
  (let [x-1 (get-coordinate input [(- x 1) y])
        x+1 (get-coordinate input [(+ x 1) y])
        y-1 (get-coordinate input [x (- y 1)])
        y+1 (get-coordinate input [x (+ y 1)])]
    (condp match [(str " " y-1 " ")
                  (str x-1 " " x+1)
                  (str " " y+1 " ")]
      [" | "
       "- -"
       " | "] \+

      [" | "
       ". -"
       " . "] \\

      [" | "
       "- ."
       " . "] \/

      [" . "
       "- ."
       " | "] \\

      [" . "
       ". -"
       " | "] \/

      [" | "
       ". ."
       " | "] \|

      [" . "
       "- -"
       " . "] \-

      ["   "
       ". ."
       "   "] \-

      [" . "
       "   "
       " . "] \|)))

(defn update-position [cart carts]
  (let [[x y] (:position cart)
        new-position (condp = (:direction cart)
                       \v [x (inc y)]
                       \^ [x (dec y)]
                       \< [(dec x) y]
                       \> [(inc x) y])]
    (cond-> cart
            :always
            (assoc :position
                   new-position)
            (some (fn [other-cart] (= (:position other-cart) new-position))
                  carts)
            (assoc :crashed new-position))))

(def turn-right
  (->> (map vector
            three-sixty
            (->> three-sixty cycle (drop 1) (take 4)))
       (into {})))

(def turn-left
  (->> (map reverse turn-right)
       (map vec)
       (into {})))

(defn update-direction [lines cart]
  (let [track (get-coordinate lines (:position cart))
        new-direction (condp = track
                        \\ (condp = (:direction cart)
                             \< \^
                             \v \>
                             \^ \<
                             \> \v)
                        \/ (condp = (:direction cart)
                             \< \v
                             \v \<
                             \^ \>
                             \> \^)
                        \+ (condp = (mod (:crossroads cart) 3)
                             0 (get turn-left (:direction cart))
                             1 (:direction cart)
                             2 (get turn-right (:direction cart)))
                        (:direction cart))]
    (cond-> cart
            new-direction
            (assoc :direction new-direction)
            (= track \+)
            (update :crossroads inc))))

(defn tick [lines carts]
  (->> carts
       (sort-by (comp second :position))
       (vec)
       ((fn [carts]
          (loop [index 0
                 carts-state carts]
            (if (< index (count carts-state))
              (recur (inc index)
                     (update carts-state index update-position carts-state))
              carts-state))))
       (map (partial update-direction lines))))

(defn get-crashes
  [carts]
  (->> carts
       (filter :crashed)
       (map :crashed)
       (into #{})))

(defn spy-railroad [railroad]
  (doall (map println railroad))
  railroad)

(defn extract-carts [input]
  (->> input
       (map (fn [y line]
              (->> line
                   (map vector (range 0 1000) line)
                   (reduce (fn [[line carts] [x char]]
                             (let [rail-char (if (contains? cart-chars char)
                                               (deduce-rail-char-for-coord input [x y])
                                               char)]
                               [(conj line rail-char)
                                (if (contains? cart-chars char)
                                  (conj carts {:origin     [x y]
                                               :position   [x y]
                                               :direction  char
                                               :crossroads 0})
                                  carts)]))
                           [[] []])))
            (range 0 9999))
       ((fn [lines-with-carts]
          [(map (fn [[line _]] (str/replace (apply str line) "\\\\" "\\"))
                lines-with-carts)
           (vec (apply concat
                       (map second lines-with-carts)))]))))

(defn part1
  {:test (fn []
           (is (= (part1 (str/split-lines "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "))
                  #{[7 3]}))
           (is (= (part1 (str/split-lines "|\nv\n|\n|\n|\n^\n|"))
                  #{[0 3]})))}
  [input]
  (let [[rail-map carts] (extract-carts input)]
    (loop [carts carts
           crashes (get-crashes carts)]
      (if (< 0 (count crashes))
        crashes
        (let [new-carts (tick rail-map carts)]
          (recur new-carts
                 (get-crashes new-carts)))))))

