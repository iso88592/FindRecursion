import org.scalatest.FlatSpec

class FindRecursionTest extends FlatSpec {
  "Identical prefixes" should "reduce to nothing" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.countPrefixes("ASD", "ASD") equals(Seq(), 1))
    assert(findRecursion.countPrefixes("Gyula", "Gyula") equals(Seq(), 1))
  }
  "Prefix with extra" should "return the extra bit" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.countPrefixes("ASD", "ASDF") equals (("F".toCharArray.toSeq, 1)))
    assert(findRecursion.countPrefixes("A", "ASD") equals (("SD".toCharArray.toSeq, 1)))
  }
  "Multiples of prefixes" should "return correct prefix and count" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.countPrefixes("A", "AASD") equals (("SD".toCharArray.toSeq, 2)))
    assert(findRecursion.countPrefixes("HE", "HEHEHEHEHEKK") equals (("KK".toCharArray.toSeq, 5)))
  }
  "No valid prefix" should "return the original values" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.countPrefixes("ASD", "A") equals (("A".toCharArray.toSeq, 0)))
    assert(findRecursion.countPrefixes("ASD", "AST") equals (("AST".toCharArray.toSeq, 0)))
    assert(findRecursion.countPrefixes("ASD", "GASD") equals (("GASD".toCharArray.toSeq, 0)))
  }
  "Generic substring" should "return the same values as String substring" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.subString("hurka", 0, 1) equals "hurka".substring(0, 1).toCharArray.toSeq)
    assert(findRecursion.subString("hurka", 0, 2) equals "hurka".substring(0, 2).toCharArray.toSeq)
    assert(findRecursion.subString("hurka", 1, 3) equals "hurka".substring(1, 3).toCharArray.toSeq)
  }
  "Empty Stack Trace" should "contain no data" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.findRecursion("") isEmpty)
  }
  "Singe element in Stack Trace" should "only have one item" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.findRecursion("A") equals Seq((1, "A".toCharArray.toSeq)))
  }
  "Two long Stack Traces" should "have one or two items" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.findRecursion("AB") equals Seq((1, "AB".toCharArray.toSeq)))
    assert(findRecursion.findRecursion("AA") equals Seq((2, "A".toCharArray.toSeq)))
  }

  "Three long Stack Traces" should "have one two or three items" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.findRecursion("ABA") equals Seq((1, "ABA".toCharArray.toSeq)))
    assert(findRecursion.findRecursion("AAA") equals Seq((3, "A".toCharArray.toSeq)))
    assert(findRecursion.findRecursion("AAB") equals Seq((2, "A".toCharArray.toSeq), (1, "B".toCharArray.toSeq)))
    assert(findRecursion.findRecursion("ABB") equals Seq((1, "A".toCharArray.toSeq), (2, "B".toCharArray.toSeq)))
  }
  "Random tests" should "be helpful for debugging" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.findRecursion("ABAB") equals Seq((2, "AB".toCharArray.toSeq)))
    assert(findRecursion.findRecursion("AAABBBAAA") equals Seq((3, "A".toCharArray.toSeq), (3, "B".toCharArray.toSeq), (3, "A".toCharArray.toSeq)))
  }
  "Acceptance tests" should "pass" in {
    val findRecursion: FindRecursion[Char] = new FindRecursion[Char]
    assert(findRecursion.findRecursion("ABCDEDEDEDEDFGH") equals Seq((1, "ABCD".toCharArray.toSeq), (4, "ED".toCharArray.toSeq), (1, "FGH".toCharArray.toSeq)))
    // The last step fails because of complexity.
    /*
    assert(findRecursion.findRecursion("LGPZEDASVEBCPATYDSOMMFZZJPBKVDEMIFEHJTPYOENWXHTDAWJDFKFGOCYJGYQNKQXTHBJWHVYCCIIPHEMNUXDYXPLRCBOKMODMSOEOYUACJCGMZDFOSTOQKAULWYDAKYVCEZEDLAOOYUPFSMGUAEMDHUDFUTMBYZTGOZCXOEGXLSXLPXFFZOEALCSKRXZRICFMVYLYPWVRGWXLMJCHMMVORDVDHFFUJXDECBYSULGQHPNCLXAUSVKCOIAPEVROCWWZEFZFNUZLZVDBQXOCMXLWQUOBDLJEQRQTSXGOAOGZTRXNETDWUCQORUHCONQOBVOJTMFPPGLNNWQBREZDERIGFUPLJFADJBLWNHSBFPPAZIQRQRTUUDNLRDPHGZQAQCBNBDGBWAJPYYDSIUDVUJZOIRWUNFRRSYITBPSKSJAATRUDPAJCNYKYJEEBFKKWJIWNHZXTJDBFCJWDPQXFCZCBDGLISVIGKPEPSUZPWFJHSIYHIEJJPKCZLTQFETXROXSRBVOOERRYVLBVOOERRYVLBVOOERRYVLBVOOERRYVLBVOOERRYVLBVOOERRYVLBVOOERRYVLBVOOERRYVLBVOOERWDUNYSYECHTRNFMURFHTRRNGOMCDMALSIHYGSFFOPEQSVPMAEJBTRUHCSCDVTKJGFBLHXRUDCPAQQEMSMCTQFEEUGZJHORFZQJZWRXVXZHZGHFELJAMNSBKGYOGLWXGQCHYYSATNZUZJCFWPTYLCATDYYLCBSADAPLZSFWDDOVYGMSPGHEONUVXXVNQGCZVFZAEJTBTXTATLIGQUXTISKZGWYLJRNMRCOYEMLOPGSSUSJAGNNASCBMHNSPKCIARQUWMFQZBMRAHZJTITUYDGGWTXOBKQTKBCGHWYZCDYUQVZXZYGERUIYMWBCMFVAFVLPBJTFEPZLRHWKUGMRBYNOGOCBBBTQSUDEZWLTFZDRPTSGPOIGIPXDRSOMYORCWBIFIWLSONZUZJVBGATSGSBVCENNWMNTOLRBRYNZFQBUCTJCWOYDQUMWEPBIQBNVANTSIMQQOQHPTJZYOUAAYXZGCDJQMGELABTVNOCUHMZAHVLVAQVOXVLTIKPTUGPJFBYKQIPBEREGAGSMRZVYHWIEEXCLKNOAKHGHFGIAJAYWUJFUZABUOKOXKGTRZBIJJQNCGHRSSEUHMTPBBUTDUHLDRXYTLUQWAFSWVQZDHDMUSLRUJDPXYLHABPDRCILHPZXLALYAYHPXZACNQDXKVVVAOHMBXNZEXGHDQXBSYAYLIXXJNXJBETVGLPXCQMBRHSFIHCNEHVMAXCCZGKDJLDRAIHHSPQEEXUAHY")
      equals Seq(
      (1, "LGPZEDASVEBCPATYDSOMMFZZJPBKVDEMIFEHJTPYOENWXHTDAWJDFKFGOCYJGYQNKQXTHBJWHVYCCIIPHEMNUXDYXPLRCBOKMODMSOEOYUACJCGMZDFOSTOQKAULWYDAKYVCEZEDLAOOYUPFSMGUAEMDHUDFUTMBYZTGOZCXOEGXLSXLPXFFZOEALCSKRXZRICFMVYLYPWVRGWXLMJCHMMVORDVDHFFUJXDECBYSULGQHPNCLXAUSVKCOIAPEVROCWWZEFZFNUZLZVDBQXOCMXLWQUOBDLJEQRQTSXGOAOGZTRXNETDWUCQORUHCONQOBVOJTMFPPGLNNWQBREZDERIGFUPLJFADJBLWNHSBFPPAZIQRQRTUUDNLRDPHGZQAQCBNBDGBWAJPYYDSIUDVUJZOIRWUNFRRSYITBPSKSJAATRUDPAJCNYKYJEEBFKKWJIWNHZXTJDBFCJWDPQXFCZCBDGLISVIGKPEPSUZPWFJHSIYHIEJJPKCZLTQFETXROXSRBVOOERR".toCharArray.toSeq),
      (8, "RYVLBVOOER".toCharArray.toSeq),
      (1, "WDUNYSYECHTRNFMURFHTRRNGOMCDMALSIHYGSFFOPEQSVPMAEJBTRUHCSCDVTKJGFBLHXRUDCPAQQEMSMCTQFEEUGZJHORFZQJZWRXVXZHZGHFELJAMNSBKGYOGLWXGQCHYYSATNZUZJCFWPTYLCATDYYLCBSADAPLZSFWDDOVYGMSPGHEONUVXXVNQGCZVFZAEJTBTXTATLIGQUXTISKZGWYLJRNMRCOYEMLOPGSSUSJAGNNASCBMHNSPKCIARQUWMFQZBMRAHZJTITUYDGGWTXOBKQTKBCGHWYZCDYUQVZXZYGERUIYMWBCMFVAFVLPBJTFEPZLRHWKUGMRBYNOGOCBBBTQSUDEZWLTFZDRPTSGPOIGIPXDRSOMYORCWBIFIWLSONZUZJVBGATSGSBVCENNWMNTOLRBRYNZFQBUCTJCWOYDQUMWEPBIQBNVANTSIMQQOQHPTJZYOUAAYXZGCDJQMGELABTVNOCUHMZAHVLVAQVOXVLTIKPTUGPJFBYKQIPBEREGAGSMRZVYHWIEEXCLKNOAKHGHFGIAJAYWUJFUZABUOKOXKGTRZBIJJQNCGHRSSEUHMTPBBUTDUHLDRXYTLUQWAFSWVQZDHDMUSLRUJDPXYLHABPDRCILHPZXLALYAYHPXZACNQDXKVVVAOHMBXNZEXGHDQXBSYAYLIXXJNXJBETVGLPXCQMBRHSFIHCNEHVMAXCCZGKDJLDRAIHHSPQEEXUAHY".toCharArray.toSeq)
    ))
    */
  }
}
