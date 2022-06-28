## Brief Introduction to Konnakol

Konnakol, in South Indian Carnatic music, is the art of performing percussion syllables vocally. It is the recitation os _Solkattu_ - the vocal syllables related to the sounds of the _Mridangam_, a major percussion instrument in Carnatic Music. Recognized as an art form of principal study, Konnakol is also a medium used by Carnatic musicians to convery rhythmic ideas to each other.

Although this system has been under development for thousands of years, there has been no successful attempts in creating a system which could formalise the rules of Konnakol. One of the chief reasons could be that not many records exist which explicitly state all the rules that need to be followed. Rather, the rules have been passed on from one generation to the next, and hence, there are not many rules, but rather, conventions that are followed.

This project shall attempt to create a system that will help in facilitating the generation of certain compositions in Konnakol, namely the Mohra, the Korvai and the Koraippu. The project shall also aim to display such generations using grid structres, so as to facilitate better understanding of complex compositions.  

## The Suladi Sapta Thala System

Thala, is the term used in Carnatic music to denote a complicated equivalent of a rhythmic cycle. A thala is usually represented with three hand gestures, namely the Laghu, the Dhruta, and the Anudhruta. A laghu is a hand clap along with finger counts, a dhruta is two claps, the second one with the hand overturned. The anudhruta is merely a clap. 

There are seven fundamental thalas, which are as follows:
1. Dhruva Thala - Laghu, Dhruta, Laghu, Laghu
2. Matya Thala - Laghu, Dhruta, Laghu
3. Rupaka Thala - Dhruta, Laghu
4. Jhampe Thala - Laghu, Anudhruta, Dhruta
5. Thriputa Thala - Laghu, Dhruta, Dhruta
6. Atta Thala - Laghu, Laghu, Dhruta, Dhruta
7. Eka Thala - Laghu

Along with the 7 thalas, there are 5 _Jatis_, where each Jati will tell us the length of the Laghu in the thala. 
The five Jatis are as follows:
1. Tisra - 3
2. Chaturasra - 4
3. Khanda - 5
4. Misra - 7
5. Sankirna - 9

Hence, the Chaturasra Jati Thriputa thala gives us a Laghu of length 4, followed by two Dhrutas. This is also known as the Adi Thala, one of the most commonly known thalas. Therefore, these 7 thalas with 5 possibilities of thalas gives us 35 thalas. 

Within each thala, one must also take into consideration the _Gati_, which is the subdivision of each beat within the thala. The five different Gatis are the same as the 5 gatis.

## Mohra

The _Mohra_ is one of the fundamental strucutues in Carnatic classical music, and is performed towards the end of a solo section. Conventionally, it can be said that a mohra will have within it 4 main components. Here, the first and the fourth components will remain identical, while the second and the fourth will be of the same size. Here, the fourth component remains fixed, and usually depends only on the gati of the composition. Once the size of the fourth component has been derived, one uses this and the total length of one _avarta_ (cycle) of the thala to derive the lengths of the first component.

If the total length of one _avarata_ is $n$, and the length of the fourth (and by extension, the second) component is is $l_2$, and that of the first (and the third) component is $l_1$, then if we know two of them, the third can be dervied using the relation

$$l_1 + l_2 = n$$

Note that in case the size of the thala ($n$) is so small that one of $l_1$ or $l_2$ comes out to be $0$ or negative, then once can consider two cycles instead of one. In other words, one can consider two cycles to be $n$ instead of just one, so that a Mohra can be constructed.

There are two other constants, dependent on the fourth constant, which are used towards the conclusion of the Mohra. If we consider these constants to be $C_1$ and $C_2$, and the initial four constants to be $A, B, C$ and $D$, then the structure of the Mohra can be represented as follows:

<p align = "center">
$$A\:B\:C\:D$$  
$$A\:B\:C\:D$$  
$$A\:B\:C\:C_1$$  
$$A\:C_1\:A\:C_2\:C_2\:C_2$$  
</p>

The structure of the Mohra is such that if one begins the Mohra at the beginning of the cycle, then the last syllable in the final $C_2$ will land exactly on the first beat of a cycle. 

Another point to note is that this is a conventional structure followed for the Mohra. There are Mohras which might not entirely satisfy this structure, but will still be accepted. However, this standard is followed most of the time.

## Korvai 

The _Korvai_/ _Mukhthaya_ is another fundamental structure is Carnatic Classical music which is also performed during the end of a solo section. In general the size of a Korvai is said to be of length $2^n$ _avartaas_, where n is a natural number (in most cases, it is restricted to 4, or atmost 8 _avartaas_). There are two kinds of Korvais, depending on the number of divisions into which the composition is divided. 

In the first kind, the _Korvai_ is divided into two structures : the Purvardha, and the Uttarardha. In the second kind, there is no particular division into the Purvardha and the Uttarardha, and the entire composition may be treated as a single unit. We shall discuss the first (and the more conventionally followed) kind in greater detail.

The _Purvardha_ is usually constructed using a set of numbers which follows some sequence in between them. This set of numbers could be a constant set (like [7, 7, 7]), or it can be a set which follows some arithmetic or geometric progression (like [2, 4, 6], or [2, 4, 8], or [7, 4, 1]). In the constant case, the norm is that the sequence has exactly three elements. However, in the other cases, one shall have a sequence as long as they would like (but the norm is still a sequence of size 3). One also has the option to intersperse these sequences with a phrase of a constant length.

The _Uttarardha_ is the second half of the _Korvai_, and is usually comprised of phrases such as _Tha Di Gi Na Thom_. These phrases are either repeated thrice, interspersed with pauses, or the number of times they are repeated increases each time. Even in this case, they can be interspersed with pauses. Here, the final syllable of the final phrase shall land exactly on the last count of the _avarta_.

One option that has been explored greatly with the Korvai (and upto an extent, with the Mohra), is that of changing _Gatis_. Within a composition, one can vary the gati so as to improve the complexity as well as the aesthetics. 

## Resources

To read more about Konnakol, one can refer the following research papers:

1. https://lisayoungmusic.com/wp-content/uploads/masters/masters.pdf - Konnakol (The History and Development of Solkattu - the Vocal Syllables - of the Mridangam) by Lisa Young
2. https://phaidra.bruckneruni.at/open/o:233 - Composing Techniques based on Indian Classical Rhythmical Structures by Anton Bruckner Privatuniversität Linz

To learn the basics of Konnakol, one can refer to the following video lecture series by Vidwan Shri B R Somashekar Jois: 
https://www.youtube.com/watch?v=ZuZF8BaOt58&list=PLmnmCVC0PGc2D3VLuSjyJVhmXA8zPlojN
