This document provides the necessary information regarding the notation that is followed in the code. This shall help even users who are not familiar with Konnakol to run the code. To understand the terms used in this document, refer [here](https://github.com/tidalcycles/konnakol-gsoc/blob/main/ABOUT_KONNAKOL.md)

## Datatypes and Instances (Define.hs)

The new datatypes defined in the code are as follows:

* **BeatCount** : Contains the components that are usually used to count a thala. Consists of 3 components, namely_Laghu_,_Dhruta_ and _Anudhruta_. The show instance of this datatype is redefined such that the _Laghu_, _Dhruta_, and _Anudhruta_ are represented using "|", "O" and "U" resepectively. 
* **Thala** : This datatype has been defined as a list of BeatCounts. The show instance here shall use the show type of BeatCount, and hence thalas will be represented a combination of "|", "O" and "U". The seven basic thalas in the _Suladi Sapta Thala System_ have already been defined here, for ease of access. For example, the thriputa thala, stored as [Laghu, Dhruta, Dhruta], will be represented as "| O O".
* **JatiGati** : A common datatype to store the _Jati_ (number of counts in the laghu of the thala), and the _Gati_ (used to identify the number of counts in a single beat). This contains the five possible jatis/gatis, namely _Tisra, Chaturasra, Khanda, Misra_ and _Sankirna_. The Enum instance has been redefined so as to store each jati/gati as their equivalent counts.
* **Syllable**: Datatype used to store the syllables that are generally used in Konnakol. A single syllable gap and a double syllable gap have been defined as _Gdot_ and _Gsc_ respectively, and their show instances have been modified to show "-" and ";" respectively. This has been done in an attempt to simplify representation of compositions. This has a toNum function which helps get the number of counts the syllable is uttered for.
* **Phrase**: This has been defined as a list of syllables. The main purpose here is to define a show method for this function, so as to facilitate a neat display of a composition. This also has a function toNumP, which helps represent a phrase by its equivalent count.
* **Comp** : This type is a unit in representing a composition, which will be list of Comp. There are two possible types this type can take. The first one is _Composition_, which is an array of tuples, where each tuple contains a list of syllables and an integer denoting the speed, and the second is _Kalach_, which denotes the gati in which this shall the respective _Composition_ is. 
* **UIComp**: This datatype is for enabling the users to enter compositions as numbers. They can enter a phrase as well as a gap as a list of two member tuples, the first indicating the phrase, and the second the speed. They can also enter a change in gati.
* **JustNums**:  This datatype is for the user to input a Korvai as just numbers without having to enter the Speeds. It has three types, a phrase, a Gap, or a section break.

## Functions (Define.hs)

* **showFinalThala**: This shows the actual final thala after taking into consideration the jati and the gati. The jati is represented as a number between paranthesis, while the gati is represented as a number between "<" and ">".
* **getCountPerBeat** : This takes the gati, and an integer representing the speed, and then returns the count per beat. There is a general rules for all gatis except _Chaturasra_, which is an exception and has a different governing rule.
* **calculateCount**: This takes the gati and the thala, and obtains the number of beats in one cycle of the thalas.
* **getThalaSplitPoints** : This takes the gati and thala, and returns an array with the length of the total counts. For each count it returns a symbol. If it is the start of a BeatCount, it returns the symbol for that particular BeatCount. Otherwise it returns "^". 
* **getRepresentation** : This takes a list of Comp, and recursively takes 2 elements, the first one being _KalaCh_ and the second one being _Composition_, and calls the getStringComp function on the same. This is then displayed along with the recursive call to the next two elements.
* **getStringComp** : This takes a Comp, the jati, the thala, the gati, and a starting position in the thala, and then we obtain the string equivalent for that Comp. For this, it first calculates the maximum speed that has been obtained, and then obtained the count per beat and count per avarta. It first calls the convToList on the Comp, and then calls the finalDisp on the composition and returns that along with the position where the next Comp will run. This function will return an error if the change in gati does not happen at the end of one beat.
* **ConvToList** : This takes a composition, the countPerBeat and the Gati. Then, for parts of the composition which are slower than the maximum speed, it appends gaps in the list so that the entire part has been normalised to the maximum speed. Hence, this functions returns a list of syllables.
* **finalDisp** : This function takes the list of syllables, the thala, an array containing the count per beat replicated to the size of the thala, the starting position, the countPerBeat and the string containing the outsput from getThalaSPlitPoints, and then returns a string with the syllables separated with the symbols. This also adds a linebreak when the new cycle starts.
* **phrase4Len** : This function returns lists for upto the length 9. This has been created keeping in mind the fact that there are general phrases which exist for smaller lengths. For phrases of longer lengths, there exist other helper functions.
* **phraseGenerator** : This function takes a given length and a random value. If the length of the phrase is less than 9 then it selects a random value by index from phrase4Len. Otherwise it randomly partitions the size into two parts and recursively calls phraseGenerator on these.
* **genPhrase4Me** :  This function takes a length and a generator, and then randomly generates a value. If the value is odd it takes a phrase of given length, otherwise it takes a phrase of smaller length and intersperses it with gaps.
* **genValues** : This function takes a list of values and a random value and gnerates phrases for all these values using genPhrase4Me.
* **mohrad** : This function takes the gati and then returns the standard final phrase for the mohra of that gati.
* **mohraC1** and **mohraC2** : These functions also returns constans based on the gatis.
* **getMohraSpeed** : This gets the kala, or the speed in which the Mohra has to be composed.
* **getMohraSeparation** :  This function sepearates the cycle based on the thala into four sections.
*  **genMohra** : This function generates the Mohra by getting the separation, and then calling genPhrase4Me for all separations. It joins these generated values and then returns the result as a String, by calling getRepresentation on the result. The function requires the user to provide the Jati, the Thala, the Gati and a generator.
*  **getThala** : This is just a helper function which enable uniformity in the way the thalas are entered within a system. With this, one enables reading the thalas with the first letter capitalized.
*  **getPurvardha** : This function helps generate the Purvardha for a Korvai. It first generates the appropriate possible sequences that are possible for the given size. Then, after randomly selecting the sequence to be followed, it generates phrases for the same, and then strings them together.
*  **getUttar** : This function generates the latter half of the Korvai, in the case where the phrases are to remain constant. Here, it randomly selects the gaps between the phrases among the possibilities for the given size. Then, it strings together the phrases.
*  **getUttarVarying** : This function generates the Uttarardha for the Korvai in the case where the number of times the phrase is repeated varies. The procedure of selection is the same as getUttar.
*  **genKorvai** : This function takes the Jati, the thala and the gati along with a generator, and then randomly divides the total possible count into two halves. It then calls getPurvardha on the first and one of the functins for the Uttarardha on the second half (depending on the allotted size). Then it combines them and calls getRepresentation on the same.
* **genComp** : This takes a list of UIComp, and converts each list into equivalent phrases, and joins them into one single datatype of the king [Comp].
* **concatPhGp** : This takes a composition and joing any two "Composition" dataypes into one, so that one may be able to call getRepresentation.
* **findString** : This is just a helper function which allows us to check for a substring within a string.
* **compValidator** : This is a function which takes a User Input of numbers, along with the jati and the thala, and then first generates the composition using genComp and the concatPhGp functions. Then, if there is no error in the composition it displays the composition.
* **groupPhs** : This method simplifies a Korvai to the maximum extent - a sequence of phrases and gaps, which can then be analysed.
* **getNums** : This function converts a simplified korvai from groupPhs to just numbers. Phrases, gaps and breaks are represented using just numbers.
* **validateKorvai** :  This function is used to validate a korvai entered as a list of JustNums. This function first gets the numerical representation of the functions using groupPhs and getNums, and then checks whether the output is a valid Korvai.

## Datatypes (diagram.hs)

* **Varying** : This function is used to take composition with varying time signatures. Here, compositions are entered as a list of tuples containing the count per beat for a section and that particular section as a list of JustNums.

## Function (diagram.hs)
 
* **sepToSingles** : This function takes a list of JustNums and converts each Phrase/ Gap of a particular length (say n) into n units with increasing/decreasing values between 0 and 1/-1. This is to facilitate a gradient in the grid generated. Phrases are converted into decreasing gradients of red, while gaps are converted into increasing gradients of blue.
* **toColors** : This function takes a list of JustNums along with an integer denoting the size of an avarta. It calls sepToSingles on the list and then breaks that list into subsets of the size of the integer.
* **getLabels**: This function takes the thala, the jati and the count per beat to construct the header string for visualizations.
* **visNums** : This function is to visualize a given list of JustNums along with the jati, thala, gati and the speed using toColors, getLabels, and gridKon.
* **pictureKorvai** : This is the core function in order to generate grid for Korvai. Here, one calls the genKorvai function on the required jati, thala and gati, and used the list of JustNums returned to form the image.
* **getSquares** : This function takes a single double value and converts it into the square of the required size with the assigned colour.
* **gridKon** : This function generates the grid for the Korvai, by calling getSquares for each row.
* **pictureMohra**: This is the core function to generate the grid for a Mohra. Here, we denote each subdivision by a colour. Hence, each section is assigned a number between 1 and 5. sepToSinglesM is then called to assign the gradient value. Then, getSquaresM is called to assign colour based on the gradient value and the identifier number. Note that the reason why the Mohra has a different representation is because the separations are the main characteristic for the Mohra (unlike the Korvai, where gaps play a more significant role).
* **sepToSinglesM** : This function is similar to the sepToSingles function, but it also contains the identifier for the Mohra segment.
* **getSquaresM**: This generates the squares for the Mohra. Here, based on the segment the value is, a different colour is generated.
*  **gridKonM** : This function generates the grid for the Mohra. 
*  **createSector**: This function enables the creation of an annular wedge for circular visualization of the Mohra.
*  **getSectors**: This function takes a list of values, and creates overlapping annular wedges for each component. The size of annular wedge for a particular component will be larger than that for the component in the previous index.
*  **CompToCircle**: This function takes a list of JustNums, along with the jati, thala, gati and the speed and creates the circular representation for the same.
*  **pictureKorvaiC** : This is the core function to generate a Korvai and to obtain its circular representation.
*  **toColors'**: This is similar to the toColors function. However, component-wise addition is performed here for overlapping representation.
*  **sepToSingles'**: This function converts a series of JustNums into a progression of numbers between 0 and 1 based on the length of the phrase/ gap for circular visualization.
*  **getCols**: This function takes a double value and returns a color for circular visualisation.
*  **convToChanging**: This function takes a Varying composition and converts it into a list of tuples containing the count per beat and a corresponding sepToSingles component.
*  **getSquaresV**: This function is used for creation of rectangles for changing time compositions. A normalised count per beat is used as the length, which varies for different count per beats.
*  **splitIt**: This function is used for recursively splitting up a composition with varying time signatures into its corresponding components.
*  **visNumsVarying**: This is the core function to visualize a composition with varying time signatures. Here, the maximum of the count per beats is used to normalise the composition.
