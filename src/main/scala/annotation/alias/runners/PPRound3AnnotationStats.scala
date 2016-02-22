package annotation.alias.runners

import java.io.File

import annotation.alias.{TypedAccuracyCalculator, UntypedAccuracyCalculator, AliasAnnotations}
import stats.Metrics
import utils.Utils._


/**
 * Calculates and outputs the results for the third round of Pride and Prejudice annotations.
 */
private object PPRound3AnnotationStats extends App {

  System.out.println("PRIDE AND PREJUDICE ROUND 3 ANNOTATION STATS")

  val dirpath = "data/annotations/alias-ident/pride-and-prejudice/shared/round-3"

  System.out.println("\n(Directory: " + dirpath + ")")

  // Directories containing annotations.
  val christyDir = new File(dirpath + File.separator + "christy")
  val estherDir = new File(dirpath + File.separator + "esther")
  val truthDir = new File(dirpath + File.separator + "truth")

  System.out.println("\n# Passages Completed")
  System.out.println("Christy: %d".format(recursiveListFiles(christyDir, noHiddenFilenameFilter).size))
  System.out.println("Esther: %d".format(recursiveListFiles(estherDir, noHiddenFilenameFilter).size))

  System.out.println("\nAvg. # Tags / Passage")
  System.out.println("Christy (Overall): %.4f".format(
    Metrics.average(recursiveListFiles(christyDir, noHiddenFilenameFilter).map(f =>
      new AliasAnnotations(f.getPath).size))))
  System.out.println("Esther (Overall): %.4f".format(
    Metrics.average(recursiveListFiles(estherDir, noHiddenFilenameFilter).map(f =>
      new AliasAnnotations(f.getPath).size))))
  System.out.println("Christy (Shared): %.4f".format(
    Metrics.average(recursiveListFiles(truthDir, noHiddenFilenameFilter).map(f =>
      new AliasAnnotations(f.getPath.replace("truth", "christy")).size))))
  System.out.println("Esther (Shared): %.4f".format(
    Metrics.average(recursiveListFiles(truthDir, noHiddenFilenameFilter).map(f =>
      new AliasAnnotations(f.getPath.replace("truth", "esther")).size))))
  System.out.println("Truth: %.4f".format(
    Metrics.average(recursiveListFiles(truthDir, noHiddenFilenameFilter).map(f =>
      new AliasAnnotations(f.getPath).size))))

  // Vectors to store evaluation measures on each passage.
  var christyUntypedPrecisions: Vector[Double] = Vector()
  var christyUntypedRecalls: Vector[Double] = Vector()
  var christyUntypedF1s: Vector[Double] = Vector()

  var estherUntypedPrecisions: Vector[Double] = Vector()
  var estherUntypedRecalls: Vector[Double] = Vector()
  var estherUntypedF1s: Vector[Double] = Vector()

  var christyTypedPrecisions: Vector[Double] = Vector()
  var christyTypedRecalls: Vector[Double] = Vector()
  var christyTypedF1s: Vector[Double] = Vector()
  var christyTypeAccuracies: Vector[Double] = Vector()

  var estherTypedPrecisions: Vector[Double] = Vector()
  var estherTypedRecalls: Vector[Double] = Vector()
  var estherTypedF1s: Vector[Double] = Vector()
  var estherTypeAccuracies: Vector[Double] = Vector()

  System.out.println("\n* Results by passage *")

  // Consider each passage in the 'truth' directory in turn (which contains the ground truth annotations for the shared
  // passages).
  recursiveListFiles(truthDir, noHiddenFilenameFilter).foreach((annotationFile: File) => {
    val trueAnnotations = new AliasAnnotations(annotationFile.getPath)

    val christyAnnotations = new AliasAnnotations(annotationFile.getPath.replace("truth", "christy"))
    val estherAnnotations = new AliasAnnotations(annotationFile.getPath.replace("truth", "esther"))

    // Chapter # of passage.
    val chapterNum = annotationFile.getParent.substring(annotationFile.getParent.lastIndexOf('-') + 1)

    System.out.println("\n** Chapter " + chapterNum + ", Passage " + trueAnnotations.id + " **")

    System.out.println("\nUNTYPED")

    var untypedPrecision = UntypedAccuracyCalculator.precision(christyAnnotations, trueAnnotations)
    var untypedRecall = UntypedAccuracyCalculator.recall(christyAnnotations, trueAnnotations)
    var untypedF1 = UntypedAccuracyCalculator.f1(christyAnnotations, trueAnnotations)

    System.out.println("\nChristy (Precision, Recall, F1): %.4f, %.4f, %.4f".format(untypedPrecision, untypedRecall,
      untypedF1))

    christyUntypedPrecisions :+= untypedPrecision
    christyUntypedRecalls :+= untypedRecall
    christyUntypedF1s :+= untypedF1

    var untypedFPs = UntypedAccuracyCalculator.getFPs(christyAnnotations, trueAnnotations)
    var untypedFNs = UntypedAccuracyCalculator.getFNs(christyAnnotations, trueAnnotations)

    System.out.println("FP's: " + (if (untypedFPs.isEmpty) "(None)" else untypedFPs.mkString(", ")))
    System.out.println("FN's: " + (if (untypedFNs.isEmpty) "(None)" else untypedFNs.mkString(", ")))

    untypedPrecision = UntypedAccuracyCalculator.precision(estherAnnotations, trueAnnotations)
    untypedRecall = UntypedAccuracyCalculator.recall(estherAnnotations, trueAnnotations)
    untypedF1 = UntypedAccuracyCalculator.f1(estherAnnotations, trueAnnotations)

    System.out.println("\nEsther (Precision, Recall, F1): %.4f, %.4f, %.4f".format(untypedPrecision, untypedRecall,
      untypedF1))

    estherUntypedPrecisions :+= untypedPrecision
    estherUntypedRecalls :+= untypedRecall
    estherUntypedF1s :+= untypedF1

    untypedFPs = UntypedAccuracyCalculator.getFPs(estherAnnotations, trueAnnotations)
    untypedFNs = UntypedAccuracyCalculator.getFNs(estherAnnotations, trueAnnotations)

    System.out.println("FP's: " + (if (untypedFPs.isEmpty) "(None)" else untypedFPs.mkString(", ")))
    System.out.println("FN's: " + (if (untypedFNs.isEmpty) "(None)" else untypedFNs.mkString(", ")))

    System.out.println("\nTYPED")

    var typedPrecision = TypedAccuracyCalculator.precision(christyAnnotations, trueAnnotations, None)
    var typedRecall = TypedAccuracyCalculator.recall(christyAnnotations, trueAnnotations, None)
    var typedF1 = TypedAccuracyCalculator.f1(christyAnnotations, trueAnnotations, None)
    var typeAccuracy = TypedAccuracyCalculator.typeAccuracy(christyAnnotations, trueAnnotations)

    System.out.println("\nChristy (Precision, Recall, F1, type Acc.): %.4f, %.4f, %.4f, %.4f".format(typedPrecision,
      typedRecall, typedF1, typeAccuracy))

    christyTypedPrecisions :+= typedPrecision
    christyTypedRecalls :+= typedRecall
    christyTypedF1s :+= typedF1
    christyTypeAccuracies :+= typeAccuracy

    var typedFPs = TypedAccuracyCalculator.getFPs(christyAnnotations, trueAnnotations, None)
    var typedFNs = TypedAccuracyCalculator.getFNs(christyAnnotations, trueAnnotations, None)

    System.out.println("FP's: " + (if (typedFPs.isEmpty) "(None)" else typedFPs.mkString(", ")))
    System.out.println("FN's: " + (if (typedFNs.isEmpty) "(None)" else typedFNs.mkString(", ")))

    typedPrecision = TypedAccuracyCalculator.precision(estherAnnotations, trueAnnotations, None)
    typedRecall = TypedAccuracyCalculator.recall(estherAnnotations, trueAnnotations, None)
    typedF1 = TypedAccuracyCalculator.f1(estherAnnotations, trueAnnotations, None)
    typeAccuracy = TypedAccuracyCalculator.typeAccuracy(estherAnnotations, trueAnnotations)

    System.out.println("\nEsther (Precision, Recall, F1, type Acc.): %.4f, %.4f, %.4f, %.4f".format(typedPrecision,
      typedRecall, typedF1, typeAccuracy))

    estherTypedPrecisions :+= typedPrecision
    estherTypedRecalls :+= typedRecall
    estherTypedF1s :+= typedF1
    estherTypeAccuracies :+= typeAccuracy

    typedFPs = TypedAccuracyCalculator.getFPs(estherAnnotations, trueAnnotations, None)
    typedFNs = TypedAccuracyCalculator.getFNs(estherAnnotations, trueAnnotations, None)

    System.out.println("FP's: " + (if (typedFPs.isEmpty) "(None)" else typedFPs.mkString(", ")))
    System.out.println("FN's: " + (if (typedFNs.isEmpty) "(None)" else typedFNs.mkString(", ")))
  })

  System.out.println("\n* Overall results *")

  System.out.println("\nChristy")
  System.out.println("\nAvg. (Untyped) Precision: %.4f".format(christyUntypedPrecisions.sum /
    christyUntypedPrecisions.size))
  System.out.println("Avg. (Untyped) Recall: %.4f".format(christyUntypedRecalls.sum / christyUntypedRecalls.size))
  System.out.println("Avg. (Untyped) F1: %.4f".format(christyUntypedF1s.sum / christyUntypedF1s.size))

  System.out.println("Avg. (Typed) Precision: %.4f".format(christyTypedPrecisions.sum / christyTypedPrecisions.size))
  System.out.println("Avg. (Typed) Recall: %.4f".format(christyTypedRecalls.sum / christyTypedRecalls.size))
  System.out.println("Avg. (Typed) F1: %.4f".format(christyTypedF1s.sum / christyTypedF1s.size))
  System.out.println("Avg. Type Acc.: %.4f".format(christyTypeAccuracies.sum/ christyTypeAccuracies.size))

  System.out.println("\nEsther")
  System.out.println("\nAvg. (Untyped) Precision: %.4f".format(estherUntypedPrecisions.sum /
    estherUntypedPrecisions.size))
  System.out.println("Avg. (Untyped) Recall: %.4f".format(estherUntypedRecalls.sum / estherUntypedRecalls.size))
  System.out.println("Avg. (Untyped) F1: %.4f".format(estherUntypedF1s.sum / estherUntypedF1s.size))

  System.out.println("Avg. (Typed) Precision: %.4f".format(estherTypedPrecisions.sum / estherTypedPrecisions.size))
  System.out.println("Avg. (Typed) Recall: %.4f".format(estherTypedRecalls.sum / estherTypedRecalls.size))
  System.out.println("Avg. (Typed) F1: %.4f".format(estherTypedF1s.sum / estherTypedF1s.size))
  System.out.println("Avg. Type Acc.: %.4f".format(estherTypeAccuracies.sum/ christyTypeAccuracies.size))

  System.out.println("\nOverall")
  System.out.println("\nAvg. (Untyped) Precision: %.4f".format(
    (christyUntypedPrecisions.sum + estherUntypedPrecisions.sum) /
      (christyUntypedPrecisions.size + estherUntypedPrecisions.size)))

  System.out.println("Avg. (Untyped) Recall: %.4f".format(
    (christyUntypedRecalls.sum + estherUntypedRecalls.sum) /
     (christyUntypedRecalls.size + estherUntypedRecalls.size)))

  System.out.println("Avg. (Untyped) F1: %.4f".format(
    (christyUntypedF1s.sum + estherUntypedF1s.sum) /
      (christyUntypedF1s.size + estherUntypedF1s.size)))

  System.out.println("Avg. (Typed) Precision: %.4f".format(
    (christyTypedPrecisions.sum + estherTypedPrecisions.sum) /
      (christyTypedPrecisions.size + estherTypedPrecisions.size)))

  System.out.println("Avg. (Typed) Recall: %.4f".format(
    (christyTypedRecalls.sum + estherTypedRecalls.sum) /
      (christyTypedRecalls.size + estherTypedRecalls.size)))

  System.out.println("Avg. (Typed) F1: %.4f".format(
    (christyTypedF1s.sum + estherTypedF1s.sum) /
      (christyTypedF1s.size + estherTypedF1s.size)))

  System.out.println("Avg. Type Acc.: %.4f".format(
    (christyTypeAccuracies.sum + estherTypeAccuracies.sum) /
      (christyTypeAccuracies.size + estherTypeAccuracies.size)))
}
