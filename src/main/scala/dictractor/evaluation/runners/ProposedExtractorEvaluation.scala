package dictractor.evaluation.runners


import altractor.NPCrossCorefExtractor
import dict.{Character, Alias}
import dictractor.coref._
import dictractor.evaluation.Evaluation
import dictractor.extractors._
import dictractor.extractors.proposed._

import scala.collection.immutable.Range.Inclusive
import scala.collection.immutable.{IndexedSeq, ListMap}


/**
 * Evaluates the Proposed character dictionary extractor.
 */
private object ProposedExtractorEvaluation extends App {

  val aliasExtractor = new NPCrossCorefExtractor(new FilteredMentionCorefResoluter(NormalizedCorefResoluter,
    AllUppercaseFilter::LowerCaseFilter::Nil))
  val nodeWeightAssigner = CountNodeWeightAssigner
  val edgeWeightAssigner = CorefPairEdgeWeightAssigner
  val rules = List(NameVariantRule, new EdgeWeightRule(1))
  val antiRules: List[CharacterAntiEdgeRule] = List(GenderAntiRule, FamilyNameAntiRule, FemaleSpousalTitleAntiRule)
  val graphTransforms = List(ResolveAntiEdges, IncorporateVerbExtractedAliases, new PruneSingletonNodes(1),
    ExtractConjunctionDialogueAliases, RemoveDanglingNameTitles, FilterNotStopListCharacters, RemovePossessiveNames,
    RemoveAmbiguousNameNodes, RemovePluralNameNodes, HandleLowerCaseNameTitles)

  val dictExtractors: ListMap[String, DictExtractor] = ListMap(
    "NP-Baseline" -> NPDictExtractor,
    "Ardanuy" -> ArdanuyExtractor,
    "BookNLP" -> BookNLPExtractor,
//    "ELSON" -> ElsonExtractor,
    "Proposed" -> new ProposedExtractor(aliasExtractor, nodeWeightAssigner, edgeWeightAssigner, rules, antiRules,
      graphTransforms)
  )

//  val edgeWeightThreshRange: Inclusive = 1 to 1
//  val nodeWeightThreshRange: Inclusive = 1 to 10

//  val thresholds: IndexedSeq[(Int, Int)] = edgeWeightThreshRange.flatMap(e => nodeWeightThreshRange.map(n => (e, n)))
//
//  val dictExtractors: ListMap[String, DictExtractor] = ListMap("BookNLP" -> BookNLPExtractor,
//    "ELSON" -> ElsonExtractor) ++ thresholds.map({ case (e, n) =>
//      val rules = List(NameVariantRule, HypocorismRule, new EdgeWeightRule(e))
//      val graphTransforms = List(ResolveAntiEdges, new PruneSingletonNodes(n), RemoveAmbiguousNameNodes)
//      "Proposed (e = " + e + ", n = " + n + ")" ->
//        new ProposedExtractor(aliasExtractor, nodeWeightAssigner, edgeWeightAssigner, rules, antiRules, graphTransforms)
//    })

//  val graphTransforms = List(ResolveAntiEdges, RemoveAmbiguousNameNodes, IncorporateVerbExtractedAliases,
//    RemovePluralNameNodes)

//  val dictExtractors: ListMap[String, DictExtractor] = ListMap("BookNLP" -> BookNLPExtractor,
//    "ELSON" -> ElsonExtractor) ++ edgeWeightThreshRange.map(t => {
//    val rules = List(NameVariantRule, HypocorismRule, new EdgeWeightRule(t))
//    "Proposed (e = " + t + ")" ->
//      new ProposedExtractor(aliasExtractor, nodeWeightAssigner, edgeWeightAssigner, rules, antiRules, graphTransforms)
//  })

  (new Evaluation(dictExtractors)).evaluateToCSV

//  val aliasDiff: Map[String, List[Alias]] = dictractor.evaluation.CharacterDictionaryEvaluator.aliasDiff(
//    new ProposedExtractor(aliasExtractor, nodeWeightAssigner, edgeWeightAssigner, rules, antiRules, graphTransforms)
//      .extract(story.PrideAndPrejudice.id, false),
//    dict.CharacterDictionaryManager.getDictionary(story.PrideAndPrejudice.id, false)
//  )
//
//  val characterDiff: Map[String, Set[Character]] = dictractor.evaluation.CharacterDictionaryEvaluator.characterDiff(
//    new ProposedExtractor(aliasExtractor, nodeWeightAssigner, edgeWeightAssigner, rules, antiRules, graphTransforms)
//      .extract(story.PrideAndPrejudice.id, false),
//    dict.CharacterDictionaryManager.getDictionary(story.PrideAndPrejudice.id, false)
//  )
//
//  println("Extracted: " + aliasDiff.get("extracted").get.map(_.span).mkString(" ; "))
//  println("Gold: " + aliasDiff.get("gold").get.map(_.span).mkString(" ; "))
//
//  println()
//
//  println("Extracted: " + characterDiff.get("extracted").get.mkString(" ; "))
//  println("Gold: " + characterDiff.get("gold").get.mkString(" ; "))

}
