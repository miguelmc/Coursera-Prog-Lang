val strings = ["one","two","three","four", "two", "six"] val names1 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]] val names2 = [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]

val cards1 = [(Clubs,Jack),(Spades,Num(8))]
val cards2 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
val cards3 = [(Clubs,Ace),(Diamonds,King)]

val test11 = all_except_option ("five", strings)=NONE

val test21 = get_substitutions1(names1, "Fred")=["Fredrick","Freddie","F"]
val test22 = get_substitutions1(names2, "Jeff")=["Jeffrey","Geoff","Jeffrey"]   
val test23 = get_substitutions2(names2, "Maris")=[]

val test31 = get_substitutions2(names1, "Fred")=["Fredrick","Freddie","F"]
val test32 = get_substitutions2(names2, "Jeff")=["Jeffrey","Geoff","Jeffrey"]   
val test33 = get_substitutions2(names2, "Maris")=[]

val test41 = similar_names(names1, {first="Fred", middle="W", last="Smith"} ) = 
         [{first="Fred", last="Smith", middle="W"},
              {first="Fredrick", last="Smith", middle="W"},
              {first="Freddie", last="Smith", middle="W"},
              {first="F", last="Smith", middle="W"}]

val test42 = similar_names(names2, {first="Jeff", middle="W", last="Smith"} ) = 
         [{first="Jeff", last="Smith", middle="W"},
              {first="Jeffrey", last="Smith", middle="W"},
              {first="Geoff", last="Smith", middle="W"},
              {first="Jeffrey", last="Smith", middle="W"}]

val test43 = similar_names(names1, {first="Jeff", middle="W", last="Smith"} ) = [{first="Jeff", middle="W", last="Smith"}]

val testA1 = card_color((Clubs,Jack))=Black
val testA2 = card_color((Spades,Jack))=Black
val testA3 = card_color((Diamonds,Ace))=Red
val testA4 = card_color((Hearts,Ace))=Red

val testB1 = card_value((Clubs,Jack))=10
val testB2 = card_value((Clubs,Queen))=10
val testB3 = card_value((Clubs,King))=10
val testB4 = card_value((Clubs,Ace))=11
val testB5 = card_value((Clubs,Num(2)))=2
val testB6 = card_value((Clubs,Num(3)))=3
val testB7 = card_value((Clubs,Num(10)))=10

val testC1 = remove_card(cards1,(Clubs,Jack),IllegalMove)=[(Spades,Num(8))]
val testC2 = remove_card(cards2,(Spades,Ace),IllegalMove)=[(Clubs,Ace),(Clubs,Ace),(Spades,Ace)]
val testC3 = remove_card(cards2,(Clubs,Ace),IllegalMove)=[(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
val testC4 = remove_card(cards1,(Spades,Num(8)),IllegalMove)=[(Clubs,Jack)]
val testC5 = (remove_card(cards2,(Spades,Num(8)),IllegalMove) handle IllegalMove => []) = []

val testD1 = all_same_color(cards1)=true
val testD2 = all_same_color(cards2)=true
val testD3 = all_same_color([(Clubs,Jack),(Spades,Num(8)),(Hearts,King)])=false
val testD4 = all_same_color([(Clubs,Jack),(Hearts,King),(Spades,Num(8))])=false
val testD5 = all_same_color([(Hearts,King),(Clubs,Jack),(Spades,Num(8))])=false
val testD6 = all_same_color(cards3)=false

val testE1 = sum_cards(cards1)=18
val testE2 = sum_cards(cards2)=44
val testE3 = sum_cards(cards3)=21

val testF1 = score(cards3,21)=0
val testF2 = score(cards3,25)=4
val testF3 = score(cards3,17)=12

val testF4 = score(cards2,44)=0
val testF5 = score(cards2,48)=2
val testF6 = score(cards2,40)=6

val testF7 = score([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42)=3

val testG0 = ( officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)] ,42) handle IllegalMove => 9999 ) = 9999

val testG1 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],42)=3

val testG2 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],30)=4
val testG3 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],22)=16
val testG4 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],100)=28
val testG5 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],44)=0

val testG6 = officiate([(Diamonds,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],30)=9
val testG7 = officiate([(Clubs,Ace),(Hearts,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],22)=33
val testG8 = officiate([(Clubs,Ace),(Spades,Ace),(Diamonds,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw],100)=56
val testG9 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw,Draw,Draw,Draw],44)=0

val testG10 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],30)=8
val testG11 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],22)=0
val testG12 = officiate([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Draw],11)=33

val testG13 = officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],11)=33
val testG14 = officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],22)=0
val testG15 = officiate([(Clubs,Queen),(Diamonds,Ace),(Clubs,Ace),(Hearts,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],30)=8

val testG16 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],11)=16
val testG17 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],22)=0
val testG18 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Discard(Clubs,Queen),Draw,Draw],30)=4

val testG19 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],11)=30
val testG20 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],22)=0
val testG21 = officiate([(Clubs,Queen),(Diamonds,Ace),(Hearts,Ace),(Diamonds,Ace)], [Draw,Draw,Discard(Clubs,Queen),Draw],30)=4
