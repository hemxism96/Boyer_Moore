import scala.collection.immutable.ListMap

class BoyerMoore(val text: String, val pattern: String):

    def find_index_alphabet(string_text: String, index: Int): Char =
        index match
            case x if x<0 => string_text.reverse.charAt(-x-1)
            case _ => string_text.charAt(index)
    // CABCA
    def make_bad_charactor_list(tmp_pattern: String, pattern_length: Int) =
        tmp_pattern.map(l => (l, pattern_length-tmp_pattern.indexOf(l)-1)).toMap

    def flag_function(badchar: Char, suffix: String, full_pattern: String, suffix_index: Int, offset:Int): Int = 
        if ((suffix_index) == suffix.length() | suffix.length==0) then 1
        else {
            val term_index = offset - suffix.length() -1 + suffix_index
            val compare_char = find_index_alphabet(full_pattern,term_index)

            if (term_index < 0 | suffix.charAt(suffix_index)==compare_char) then
                flag_function(badchar,suffix,full_pattern,suffix_index+1,offset)
            else 0
        }

    def find_suffix_point(badchar: Char, suffix: String, full_pattern: String = pattern, offset:Int = pattern.length()): Int =
        val flag = flag_function(badchar,suffix,full_pattern,0,offset)
        val term_index = offset - suffix.length() -1
        val compare_char = find_index_alphabet(full_pattern,term_index-1)

        // compare_char != badchar : it's for beeing sure about the left character of suffix is different
        // term_index <= 0 : in array shift having shift[i] = 0
        // in other words, the expected index is out of pattern 0, -1, -2
        // flag==1 : check there's same suffix in pattern
        if ((flag==1) & (term_index <= 0 | compare_char != badchar)) then
            // set the border position of the first character of the pattern to all indices
            full_pattern.length()+1-offset
        // suffix becomes shorter than bpos[0], use the position of next widest border as value of j
        else find_suffix_point(badchar,suffix,full_pattern,offset-1)

    def suffix_list_function(x: String = pattern, y: List[String], res: Map[Int, Int] = Map[Int, Int]()): Map[Int, Int] = 
        (x,y,res) match {
            case (_,Nil,_) => res
            case (x, y :: ys,res) => 
                // Map(Key->value): key: i, value: shift size
                //input: the left char of suffix ,suffix (ex: "","A","CA")
                suffix_list_function(y=ys,res=res+(res.size -> find_suffix_point(badchar=x.charAt(x.length()-res.size-1),suffix=y)))
        }
        
    def make_suffix_list(x: List[Int], y: List[String], res: List[String] = Nil): List[String] = 
        (x,y) match{
            case (x :: xs, y :: ys) => make_suffix_list(xs,ys,res:::List(y.substring(x)))
            //case (Nil,Nil) => res
            case (_,_) => res
        }

    def make_good_suffix_list(pattern_length: Int) = 
        val key_list = List.range(1,pattern_length+1)
        var value_list = List.fill(pattern_length)(pattern)
        // make a list of suffix 
        // starts from "","A","CA" in this order
        value_list = make_suffix_list(key_list,value_list).reverse
        println(value_list)
        suffix_list_function(y=value_list)

    // find a different character between pattern and text from p to p+j
    def search_different_character(p: Int, q: Int): Int =
        val compare_char_pattern = find_index_alphabet(pattern,q-1)
        val compare_char_text = find_index_alphabet(text,p+q-1)

        if (q>0) & (compare_char_pattern==compare_char_text) then search_different_character(p,q-1)
        else q

    def search_function(limit: Int, p: Int, q: Int, bad_list: Map[Char, Int], good_list: Map[Int, Int]): Int = 
        if (p>=limit) then
            println("Can't find the word in text")
            -1
        else{
            val j = pattern.length()
            val modified_q = search_different_character(p,j)

            if (modified_q>0) then {
                // if there's same alphabet in text the offset is a distance
                // or the size of pattern
                var badCharShift = bad_list.getOrElse(text.charAt(p+modified_q-1),j)
                // reverse
                var goodSuffixShift = good_list(j-modified_q)

                if (badCharShift > goodSuffixShift) then 
                    search_function(limit,p + badCharShift,modified_q,bad_list,good_list)
                else
                    search_function(limit,p + goodSuffixShift,modified_q,bad_list,good_list)
                
            }
            else
                println("We found it!")
                p
        } 

    def search(): Int =
        val i = text.length()
        val j = pattern.length()
        assert(i != 0 & j != 0)

        if (i<j) then 
            println("Pattern is shorter than text. Please check them")
            -1
        else{
                val bad_list = make_bad_charactor_list(pattern,j)
                val good_list = make_good_suffix_list(j)
                println(bad_list)
                println(good_list)

                search_function(i-j+1,0,j,bad_list,good_list)
        }

@main 
def main() = 
    //val text = "ABAACABCAD"
    //val pattern = "CABCA"
    val text = "Hello! Welcome to Scala World of Suyeon"
    val pattern = "Suyeon"
    val BM_Algo = new BoyerMoore(text,pattern)
    print(BM_Algo.search())
    println("th index")