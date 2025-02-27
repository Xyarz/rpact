## |
## |  *Simulation of enrichment design with combination test*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File version: $Revision: 7126 $
## |  Last changed: $Date: 2023-06-23 14:26:39 +0200 (Fr, 23 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_simulation_utilities.R
#' @include f_core_utilities.R
NULL

.getIndicesOfSelectedSubsets <- function(gMax) {
    subsets <- .getAllAvailableSubsets(1:gMax)
    subsets <- subsets[grepl(as.character(gMax), subsets)]
    indexList <- list()
    subsetIndex <- 1
    if (length(subsets) > 1) {
        subsetIndex <- c(2:length(subsets), 1)
    }
    for (i in subsetIndex) {
        s <- subsets[i]
        indices <- as.integer(strsplit(s, "", fixed = TRUE)[[1]])
        indexList[[length(indexList) + 1]] <- indices
    }
    return(indexList)
}

.createSelectedSubsets <- function(stage, selectedPopulations) {
    gMax <- nrow(selectedPopulations)
    selectedVector <- rep(FALSE, 2^(gMax - 1))

    if (gMax == 1) {
        selectedVector[1] <- selectedPopulations[1, stage]
    }
    if (gMax == 2) {
        selectedVector[1] <- selectedPopulations[1, stage] || selectedPopulations[2, stage]
        selectedVector[2] <- selectedPopulations[2, stage]
    }
    if (gMax == 3) {
        selectedVector[1] <- selectedPopulations[1, stage] || selectedPopulations[3, stage]
        selectedVector[2] <- selectedPopulations[2, stage] || selectedPopulations[3, stage]
        selectedVector[3] <- selectedPopulations[1, stage] || selectedPopulations[2, stage] || selectedPopulations[3, stage]
        selectedVector[4] <- selectedPopulations[3, stage]
    }
    if (gMax == 4) {
        selectedVector[1] <- selectedPopulations[1, stage] || selectedPopulations[4, stage]
        selectedVector[2] <- selectedPopulations[2, stage] || selectedPopulations[4, stage]
        selectedVector[3] <- selectedPopulations[3, stage] || selectedPopulations[4, stage]
        selectedVector[4] <- selectedPopulations[1, stage] || selectedPopulations[2, stage] || selectedPopulations[4, stage]
        selectedVector[5] <- selectedPopulations[1, stage] || selectedPopulations[3, stage] || selectedPopulations[4, stage]
        selectedVector[6] <- selectedPopulations[2, stage] || selectedPopulations[3, stage] || selectedPopulations[4, stage]
        selectedVector[7] <- selectedPopulations[1, stage] || selectedPopulations[2, stage] || selectedPopulations[3, stage] ||
            selectedPopulations[4, stage]
        selectedVector[8] <- selectedPopulations[4, stage]
    }
    return(selectedVector)
}


.selectPopulations <- function(stage, effectVector, typeOfSelection,
        epsilonValue, rValue, threshold, selectPopulationsFunction) {
    gMax <- length(effectVector)

    if (typeOfSelection != "userDefined") {
        if (typeOfSelection == "all") {
            selectedPopulations <- rep(TRUE, gMax)
        } else {
            selectedPopulations <- rep(FALSE, gMax)
            if (typeOfSelection == "best") {
                selectedPopulations[which.max(effectVector)] <- TRUE
            } else if (tolower(typeOfSelection) == "rbest") {
                selectedPopulations[order(effectVector, decreasing = TRUE)[1:rValue]] <- TRUE
                selectedPopulations[is.na(effectVector)] <- FALSE
            } else if (typeOfSelection == "epsilon") {
                selectedPopulations[max(effectVector, na.rm = TRUE) - effectVector <= epsilonValue] <- TRUE
                selectedPopulations[is.na(effectVector)] <- FALSE
            }
        }
        selectedPopulations[effectVector <= threshold] <- FALSE
    } else {
        functionArgumentNames <- .getFunctionArgumentNames(selectPopulationsFunction, ignoreThreeDots = TRUE)
        if (length(functionArgumentNames) == 1) {
            .assertIsValidFunction(
                fun = selectPopulationsFunction,
                funArgName = "selectPopulationsFunction",
                expectedArguments = c("effectVector"), validateThreeDots = FALSE
            )
            selectedPopulations <- selectPopulationsFunction(effectVector)
        } else {
            .assertIsValidFunction(
                fun = selectPopulationsFunction,
                funArgName = "selectPopulationsFunction",
                expectedArguments = c("effectVector", "stage"), validateThreeDots = FALSE
            )
            selectedPopulations <- selectPopulationsFunction(effectVector = effectVector, stage = stage)
        }
        selectedPopulations[is.na(effectVector)] <- FALSE

        msg <- paste0(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'selectPopulationsFunction' returned an illegal or undefined result (", .arrayToString(selectedPopulations), "); "
        )
        if (length(selectedPopulations) != gMax) {
            stop(msg, "the output must be a logical vector of length 'gMax' (", gMax, ")")
        }
        if (!is.logical(selectedPopulations)) {
            stop(msg, "the output must be a logical vector (is ", .getClassName(selectedPopulations), ")")
        }
    }
    return(selectedPopulations)
}


.performClosedCombinationTestForSimulationEnrichment <- function(...,
        stageResults, design, indices, intersectionTest, successCriterion) {
    if (.isTrialDesignGroupSequential(design) && (design$kMax > 1)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Group sequential design cannot be used for enrichment designs with population selection"
        )
    }

    gMax <- nrow(stageResults$testStatistics)
    kMax <- design$kMax

    adjustedStageWisePValues <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = kMax)
    overallAdjustedTestStatistics <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = kMax)
    rejected <- matrix(FALSE, nrow = gMax, ncol = kMax)
    rejectedIntersections <- matrix(FALSE, nrow = nrow(indices), ncol = kMax)
    futility <- matrix(FALSE, nrow = gMax, ncol = kMax - 1)
    futilityIntersections <- matrix(FALSE, nrow = nrow(indices), ncol = kMax - 1)
    rejectedIntersectionsBefore <- matrix(FALSE, nrow = nrow(indices), ncol = 1)
    successStop <- rep(FALSE, kMax)
    futilityStop <- rep(FALSE, kMax - 1)

    if (.isTrialDesignFisher(design)) {
        weightsFisher <- .getWeightsFisher(design)
    } else {
        weightsInverseNormal <- .getWeightsInverseNormal(design)
    }

    if (gMax == 1) {
        intersectionTest <- "Bonferroni"
    }

    separatePValues <- stageResults$separatePValues
    if (intersectionTest == "SpiessensDebois") {
        subjectsPerStage <- stageResults[[ifelse(
            !is.null(stageResults[["subjectsPerStage"]]), "subjectsPerStage", "eventsPerStage"
        )]]
        testStatistics <- stageResults$testStatistics
    } else {
        subjectsPerStage <- NULL
        testStatistics <- NULL
    }

    for (k in 1:kMax) {
        for (i in 1:(2^gMax - 1)) {
            if (!all(is.na(separatePValues[indices[i, ] == 1, k]))) {
                if (intersectionTest == "SpiessensDebois") {
                    subjectsSelected <- as.numeric(na.omit(subjectsPerStage[indices[i, ] == 1 &
                        stageResults$selectedPopulations[, k], k]))
                    if (length(subjectsSelected) == 1) {
                        sigma <- 1
                    } else {
                        sigma <- matrix(sqrt(subjectsSelected[1] / sum(subjectsSelected)), nrow = 2, ncol = 2)
                        diag(sigma) <- 1
                    }

                    maxTestStatistic <- max(testStatistics[indices[i, ] == 1, k], na.rm = TRUE)
                    adjustedStageWisePValues[i, k] <- 1 - .getMultivariateDistribution(
                        type = "normal", upper = maxTestStatistic, sigma = sigma, df = NA_real_
                    )
                }
                #  Bonferroni adjusted p-values
                else if (intersectionTest == "Bonferroni") {
                    adjustedStageWisePValues[i, k] <- min(c(sum(indices[
                        i,
                        !is.na(separatePValues[, k])
                    ]) *
                        min(separatePValues[indices[i, ] == 1, k], na.rm = TRUE), 1))
                }
                #  Simes adjusted p-values
                else if (intersectionTest == "Simes") {
                    adjustedStageWisePValues[i, k] <- min(sum(indices[
                        i,
                        !is.na(separatePValues[, k])
                    ]) /
                        (1:sum(indices[i, !is.na(separatePValues[, k])])) *
                        sort(separatePValues[indices[i, ] == 1, k]))
                }
                #  Sidak adjusted p-values
                else if (intersectionTest == "Sidak") {
                    adjustedStageWisePValues[i, k] <- 1 - (1 -
                        min(separatePValues[indices[i, ] == 1, k], na.rm = TRUE))^
                        sum(indices[i, !is.na(separatePValues[, k])])
                }

                if (.isTrialDesignFisher(design)) {
                    overallAdjustedTestStatistics[i, k] <-
                        prod(adjustedStageWisePValues[i, 1:k]^weightsFisher[1:k])
                } else {
                    overallAdjustedTestStatistics[i, k] <-
                        (weightsInverseNormal[1:k] %*% .getOneMinusQNorm(adjustedStageWisePValues[i, 1:k])) /
                            sqrt(sum(weightsInverseNormal[1:k]^2))
                }
            }

            if (.isTrialDesignFisher(design)) {
                rejectedIntersections[i, k] <- (overallAdjustedTestStatistics[i, k] <= design$criticalValues[k])
                if (k < kMax) {
                    futilityIntersections[i, k] <- (adjustedStageWisePValues[i, k] >= design$alpha0Vec[k])
                }
            } else if (.isTrialDesignInverseNormal(design)) {
                rejectedIntersections[i, k] <- (overallAdjustedTestStatistics[i, k] >= design$criticalValues[k])
                if (k < kMax) {
                    futilityIntersections[i, k] <- (overallAdjustedTestStatistics[i, k] <= design$futilityBounds[k])
                }
            }

            rejectedIntersections[is.na(rejectedIntersections[, k]), k] <- FALSE

            if (k == kMax && !rejectedIntersections[1, k]) {
                break
            }
        }

        rejectedIntersections[, k] <- rejectedIntersections[, k] | rejectedIntersectionsBefore
        rejectedIntersectionsBefore <- matrix(rejectedIntersections[, k], ncol = 1)

        for (j in 1:gMax) {
            rejected[j, k] <- all(rejectedIntersections[indices[, j] == 1, k], na.rm = TRUE)
            if (k < kMax) {
                futility[j, k] <- any(futilityIntersections[indices[, j] == 1, k], na.rm = TRUE)
            }
        }

        if (successCriterion == "all") {
            successStop[k] <- all(rejected[stageResults$selectedPopulations[1:gMax, k], k])
        } else {
            successStop[k] <- any(rejected[, k])
        }

        if (k < kMax) {
            futilityStop[k] <- all(futility[stageResults$selectedPopulations[1:gMax, k], k])
            if (all(stageResults$selectedPopulations[1:gMax, k + 1] == FALSE)) {
                futilityStop[k] <- TRUE
            }
        }
    }

    return(list(
        separatePValues = separatePValues,
        adjustedStageWisePValues = adjustedStageWisePValues,
        overallAdjustedTestStatistics = overallAdjustedTestStatistics,
        rejected = rejected,
        rejectedIntersections = rejectedIntersections,
        selectedPopulations = stageResults$selectedPopulations,
        successStop = successStop,
        futilityStop = futilityStop
    ))
}

.createSimulationResultsEnrichmentObject <- function(...,
        design,
        effectList,
        intersectionTest,
        stratifiedAnalysis = NA,
        directionUpper = NA, # rates + survival only
        adaptations,
        typeOfSelection,
        effectMeasure,
        successCriterion,
        epsilonValue,
        rValue,
        threshold,
        plannedSubjects = NA_real_, # means + rates only
        plannedEvents = NA_real_, # survival only
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage = NA_real_, # means + rates only
        maxNumberOfSubjectsPerStage = NA_real_, # means + rates only
        minNumberOfEventsPerStage = NA_real_, # survival only
        maxNumberOfEventsPerStage = NA_real_, # survival only
        conditionalPower,
        thetaH1 = NA_real_, # means + survival only
        stDevH1 = NA_real_, # means only
        piTreatmentH1 = NA_real_, # rates only
        piControlH1 = NA_real_, # rates only
        maxNumberOfIterations,
        seed,
        calcSubjectsFunction = NULL, # means + rates only
        calcEventsFunction = NULL, # survival only
        selectPopulationsFunction,
        showStatistics,
        endpoint = c("means", "rates", "survival")) {
    endpoint <- match.arg(endpoint)

    .assertIsSingleNumber(threshold, "threshold", naAllowed = FALSE)

    .assertIsSingleLogical(stratifiedAnalysis, "stratifiedAnalysis")

    .assertIsSinglePositiveInteger(rValue, "rValue", naAllowed = TRUE, validateType = FALSE)

    .assertIsNumericVector(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM, naAllowed = TRUE)

    .assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
    .assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)

    .assertIsLogicalVector(adaptations, "adaptations", naAllowed = TRUE)

    if (endpoint %in% c("means", "rates")) {
        .assertIsNumericVector(minNumberOfSubjectsPerStage, "minNumberOfSubjectsPerStage", naAllowed = TRUE)
        .assertIsNumericVector(maxNumberOfSubjectsPerStage, "maxNumberOfSubjectsPerStage", naAllowed = TRUE)
    } else if (endpoint == "survival") {
        .assertIsNumericVector(minNumberOfEventsPerStage, "minNumberOfEventsPerStage", naAllowed = TRUE)
        .assertIsNumericVector(maxNumberOfEventsPerStage, "maxNumberOfEventsPerStage", naAllowed = TRUE)
    }

    .assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
    .assertIsSingleLogical(showStatistics, "showStatistics", naAllowed = FALSE)
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)

    if (endpoint %in% c("rates", "survival")) {
        .assertIsSingleLogical(directionUpper, "directionUpper")
    }

    if (endpoint %in% c("means", "survival")) {
        .assertIsSingleNumber(thetaH1, "thetaH1", naAllowed = TRUE) # means + survival only
    }

    if (endpoint == "means") {
        .assertIsSingleNumber(stDevH1, "stDevH1", naAllowed = TRUE)
        .assertIsInOpenInterval(stDevH1, "stDevH1", 0, NULL, naAllowed = TRUE)
    }

    successCriterion <- .assertIsValidSuccessCriterion(successCriterion)
    effectMeasure <- .assertIsValidEffectMeasure(effectMeasure)

    if (endpoint == "means") {
        simulationResults <- SimulationResultsEnrichmentMeans(design, showStatistics = showStatistics)
    } else if (endpoint == "rates") {
        simulationResults <- SimulationResultsEnrichmentRates(design, showStatistics = showStatistics)
    } else if (endpoint == "survival") {
        simulationResults <- SimulationResultsEnrichmentSurvival(design, showStatistics = showStatistics)
    }

    effectList <- .getValidatedEffectList(effectList, endpoint = endpoint)
    gMax <- .getGMaxFromSubGroups(effectList$subGroups)
    if (gMax > 4) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'populations' (", gMax, ") must not exceed 4")
    }

    .assertIsValidThreshold(threshold, activeArms = gMax)

    intersectionTest <- intersectionTest[1]
    .assertIsValidIntersectionTestEnrichment(design, intersectionTest)

    if (intersectionTest == "SpiessensDebois" && gMax > 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Spiessen & Debois intersection test cannot generally ",
            "be used for enrichment designs with more than two populations"
        )
    }

    typeOfSelection <- .assertIsValidTypeOfSelection(typeOfSelection, rValue, epsilonValue, gMax)
    if (length(typeOfSelection) == 1 && typeOfSelection == "userDefined" &&
            !is.null(threshold) && length(threshold) == 1 && threshold != -Inf) {
        warning("'threshold' (", threshold, ") will be ignored because 'typeOfSelection' = \"userDefined\"", call. = FALSE)
        threshold <- -Inf
    }

    if (length(typeOfSelection) == 1 && typeOfSelection != "userDefined" && !is.null(selectPopulationsFunction)) {
        warning("'selectPopulationsFunction' will be ignored because 'typeOfSelection' is not \"userDefined\"", call. = FALSE)
    } else if (!is.null(selectPopulationsFunction) && is.function(selectPopulationsFunction)) {
        simulationResults$selectPopulationsFunction <- selectPopulationsFunction
    }

    if (endpoint %in% c("rates", "survival")) {
        .setValueAndParameterType(simulationResults, "directionUpper", directionUpper, TRUE)
    }

    if (!stratifiedAnalysis && endpoint %in% c("means", "survival")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "For testing ", endpoint, ifelse(endpoint == "survival", " designs", ""),
            ", only stratified analysis is supported"
        )
    }

    kMax <- design$kMax
    if (endpoint == "means") {
        stDevH1 <- .ignoreParameterIfNotUsed(
            "stDevH1", stDevH1, kMax > 1,
            "design is fixed ('kMax' = 1)", "Assumed standard deviation"
        )
    } else if (endpoint == "rates") {
        .assertIsSingleNumber(piTreatmentH1, "piTreatmentH1", naAllowed = TRUE)
        .assertIsInOpenInterval(piTreatmentH1, "piTreatmentH1", 0, 1, naAllowed = TRUE)
        piTreatmentH1 <- .ignoreParameterIfNotUsed(
            "piTreatmentH1", piTreatmentH1, kMax > 1,
            "design is fixed ('kMax' = 1)", "Assumed active rate(s)"
        )
        .setValueAndParameterType(simulationResults, "piTreatmentH1", piTreatmentH1, NA_real_)

        .assertIsSingleNumber(piControlH1, "piControlH1", naAllowed = TRUE)
        .assertIsInOpenInterval(piControlH1, "piControlH1", 0, 1, naAllowed = TRUE)
        piControlH1 <- .ignoreParameterIfNotUsed(
            "piControlH1", piControlH1, kMax > 1,
            "design is fixed ('kMax' = 1)", "Assumed control rate(s)"
        )
        .setValueAndParameterType(simulationResults, "piControlH1", piControlH1, NA_real_)
    } else if (endpoint == "survival") {
        .assertIsIntegerVector(plannedEvents, "plannedEvents", validateType = FALSE)
        if (length(plannedEvents) != kMax) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'plannedEvents' (", .arrayToString(plannedEvents),
                ") must have length ", kMax
            )
        }
        .assertIsInClosedInterval(plannedEvents, "plannedEvents", lower = 1, upper = NULL)
        .assertValuesAreStrictlyIncreasing(plannedEvents, "plannedEvents")
        .setValueAndParameterType(simulationResults, "plannedEvents", plannedEvents, NA_real_)
    }

    if (endpoint %in% c("means", "rates")) {
        .assertIsValidPlannedSubjects(plannedSubjects, kMax) # means + rates only
    }

    if (endpoint %in% c("means", "survival")) {
        thetaH1 <- .ignoreParameterIfNotUsed(
            "thetaH1", thetaH1, kMax > 1,
            "design is fixed ('kMax' = 1)", "Assumed effect"
        )
    }

    if (endpoint == "means") {
        # 		if (is.na(conditionalPower) && is.null(calcSubjectsFunction) && !is.na(thetaH1)) {
        # 			warning("'thetaH1' will be ignored because neither 'conditionalPower' nor ",
        # 				"'calcSubjectsFunction' is defined", call. = FALSE)
        # 		}
        # 		if (is.na(conditionalPower) && is.null(calcSubjectsFunction) && !is.na(stDevH1)) {
        # 			warning("'stDevH1' will be ignored because neither 'conditionalPower' nor ",
        # 				"'calcSubjectsFunction' is defined", call. = FALSE)
        # 		}
    }

    if (endpoint == "survival") {
        # 		if (is.na(conditionalPower) && is.null(calcEventsFunction) && !is.na(thetaH1)) {
        # 			warning("'thetaH1' will be ignored because neither 'conditionalPower' nor ",
        # 				"'calcEventsFunction' is defined", call. = FALSE)
        # 		}
    }

    conditionalPower <- .ignoreParameterIfNotUsed(
        "conditionalPower",
        conditionalPower, kMax > 1, "design is fixed ('kMax' = 1)"
    )

    if (endpoint %in% c("means", "rates")) { # means + rates only

        minNumberOfSubjectsPerStage <- .ignoreParameterIfNotUsed(
            "minNumberOfSubjectsPerStage",
            minNumberOfSubjectsPerStage, kMax > 1, "design is fixed ('kMax' = 1)"
        )
        minNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(minNumberOfSubjectsPerStage,
            "minNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, calcSubjectsFunction, kMax,
            endpoint = endpoint
        )

        maxNumberOfSubjectsPerStage <- .ignoreParameterIfNotUsed(
            "maxNumberOfSubjectsPerStage",
            maxNumberOfSubjectsPerStage, kMax > 1, "design is fixed ('kMax' = 1)"
        )
        maxNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(maxNumberOfSubjectsPerStage,
            "maxNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, calcSubjectsFunction, kMax,
            endpoint = endpoint
        )

        if (kMax > 1) {
            if (!all(is.na(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage)) &&
                    any(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage < 0)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjectsPerStage' (",
                    .arrayToString(maxNumberOfSubjectsPerStage),
                    ") must be not smaller than minNumberOfSubjectsPerStage' (",
                    .arrayToString(minNumberOfSubjectsPerStage), ")"
                )
            }
            .setValueAndParameterType(
                simulationResults, "minNumberOfSubjectsPerStage",
                minNumberOfSubjectsPerStage, NA_real_
            )
            .setValueAndParameterType(
                simulationResults, "maxNumberOfSubjectsPerStage",
                maxNumberOfSubjectsPerStage, NA_real_
            )
        }
    } else if (endpoint == "survival") {
        minNumberOfEventsPerStage <- .ignoreParameterIfNotUsed(
            "minNumberOfEventsPerStage",
            minNumberOfEventsPerStage, kMax > 1, "design is fixed ('kMax' = 1)"
        )
        minNumberOfEventsPerStage <- .assertIsValidNumberOfSubjectsPerStage(minNumberOfEventsPerStage,
            "minNumberOfEventsPerStage", plannedEvents, conditionalPower, calcEventsFunction, kMax,
            endpoint = endpoint
        )

        maxNumberOfEventsPerStage <- .ignoreParameterIfNotUsed(
            "maxNumberOfEventsPerStage",
            maxNumberOfEventsPerStage, kMax > 1, "design is fixed ('kMax' = 1)"
        )
        maxNumberOfEventsPerStage <- .assertIsValidNumberOfSubjectsPerStage(maxNumberOfEventsPerStage,
            "maxNumberOfEventsPerStage", plannedEvents, conditionalPower, calcEventsFunction, kMax,
            endpoint = endpoint
        )

        if (kMax > 1) {
            if (!all(is.na(maxNumberOfEventsPerStage - minNumberOfEventsPerStage)) &&
                    any(maxNumberOfEventsPerStage - minNumberOfEventsPerStage < 0)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfEventsPerStage' (",
                    .arrayToString(maxNumberOfEventsPerStage),
                    ") must be not smaller than 'minNumberOfEventsPerStage' (",
                    .arrayToString(minNumberOfEventsPerStage), ")"
                )
            }
            .setValueAndParameterType(
                simulationResults, "minNumberOfEventsPerStage",
                minNumberOfEventsPerStage, NA_real_
            )
            .setValueAndParameterType(
                simulationResults, "maxNumberOfEventsPerStage",
                maxNumberOfEventsPerStage, NA_real_
            )
        }
    }

    if (kMax == 1 && !is.na(conditionalPower)) {
        warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
    }
    if (endpoint %in% c("means", "rates") && kMax == 1 && !is.null(calcSubjectsFunction)) {
        warning("'calcSubjectsFunction' will be ignored for fixed sample design", call. = FALSE)
    }
    if (endpoint == "survival" && kMax == 1 && !is.null(calcEventsFunction)) {
        warning("'calcEventsFunction' will be ignored for fixed sample design", call. = FALSE)
    }

    if (endpoint %in% c("means", "rates") && is.na(conditionalPower) && is.null(calcSubjectsFunction)) {
        if (length(minNumberOfSubjectsPerStage) != 1 || !is.na(minNumberOfSubjectsPerStage)) {
            warning("'minNumberOfSubjectsPerStage' (",
                .arrayToString(minNumberOfSubjectsPerStage), ") will be ignored because ",
                "neither 'conditionalPower' nor 'calcSubjectsFunction' is defined",
                call. = FALSE
            )
            simulationResults$minNumberOfSubjectsPerStage <- NA_real_
        }
        if (length(maxNumberOfSubjectsPerStage) != 1 || !is.na(maxNumberOfSubjectsPerStage)) {
            warning("'maxNumberOfSubjectsPerStage' (",
                .arrayToString(maxNumberOfSubjectsPerStage), ") will be ignored because ",
                "neither 'conditionalPower' nor 'calcSubjectsFunction' is defined",
                call. = FALSE
            )
            simulationResults$maxNumberOfSubjectsPerStage <- NA_real_
        }
    }

    if (endpoint == "survival" && is.na(conditionalPower) && is.null(calcEventsFunction)) {
        if (length(minNumberOfEventsPerStage) != 1 || !is.na(minNumberOfEventsPerStage)) {
            warning("'minNumberOfEventsPerStage' (",
                .arrayToString(minNumberOfEventsPerStage), ") ",
                "will be ignored because neither 'conditionalPower' nor 'calcEventsFunction' is defined",
                call. = FALSE
            )
            simulationResults$minNumberOfEventsPerStage <- NA_real_
        }
        if (length(maxNumberOfEventsPerStage) != 1 || !is.na(maxNumberOfEventsPerStage)) {
            warning("'maxNumberOfEventsPerStage' (",
                .arrayToString(maxNumberOfEventsPerStage), ") ",
                "will be ignored because neither 'conditionalPower' nor 'calcEventsFunction' is defined",
                call. = FALSE
            )
            simulationResults$maxNumberOfEventsPerStage <- NA_real_
        }
    }

    if (endpoint %in% c("means", "rates")) {
        simulationResults$.setParameterType(
            "calcSubjectsFunction",
            ifelse(kMax == 1, C_PARAM_NOT_APPLICABLE,
                ifelse(!is.null(calcSubjectsFunction) && kMax > 1, C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)
            )
        )
    } else if (endpoint == "survival") {
        simulationResults$.setParameterType(
            "calcEventsFunction",
            ifelse(kMax == 1, C_PARAM_NOT_APPLICABLE,
                ifelse(!is.null(calcEventsFunction) && kMax > 1, C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)
            )
        )
    }

    if (endpoint == "means") {
        if (is.null(calcSubjectsFunction)) {
            calcSubjectsFunction <- .getSimulationMeansEnrichmentStageSubjects
        } else {
            .assertIsValidFunction(
                fun = calcSubjectsFunction,
                funArgName = "calcSubjectsFunction",
                expectedFunction = .getSimulationMeansEnrichmentStageSubjects
            )
        }
        simulationResults$calcSubjectsFunction <- calcSubjectsFunction
    } else if (endpoint == "rates") {
        if (is.null(calcSubjectsFunction)) {
            calcSubjectsFunction <- .getSimulationRatesEnrichmentStageSubjects
        } else {
            .assertIsValidFunction(
                fun = calcSubjectsFunction,
                funArgName = "calcSubjectsFunction",
                expectedFunction = .getSimulationRatesEnrichmentStageSubjects
            )
        }
        simulationResults$calcSubjectsFunction <- calcSubjectsFunction
    } else if (endpoint == "survival") {
        if (is.null(calcEventsFunction)) {
            calcEventsFunction <- .getSimulationSurvivalEnrichmentStageEvents
        } else {
            .assertIsValidFunction(
                fun = calcEventsFunction,
                funArgName = "calcEventsFunction",
                expectedFunction = .getSimulationSurvivalEnrichmentStageEvents
            )
        }
        simulationResults$calcEventsFunction <- calcEventsFunction
    }

    if (any(is.na(allocationRatioPlanned))) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, design$kMax)
    } else if (length(allocationRatioPlanned) != design$kMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'allocationRatioPlanned' (", .arrayToString(allocationRatioPlanned), ") ",
            "must have length 1 or ", design$kMax, " (kMax)"
        )
    }

    if (length(unique(allocationRatioPlanned)) == 1) {
        .setValueAndParameterType(
            simulationResults, "allocationRatioPlanned",
            allocationRatioPlanned[1],
            defaultValue = 1
        )
    } else {
        .setValueAndParameterType(
            simulationResults, "allocationRatioPlanned",
            allocationRatioPlanned,
            defaultValue = rep(1, design$kMax)
        )
    }

    if (endpoint %in% c("means", "rates")) {
        .setValueAndParameterType(simulationResults, "plannedSubjects", plannedSubjects, NA_real_)
        .setValueAndParameterType(simulationResults, "minNumberOfSubjectsPerStage",
            minNumberOfSubjectsPerStage, NA_real_,
            notApplicableIfNA = TRUE
        )
        .setValueAndParameterType(simulationResults, "maxNumberOfSubjectsPerStage",
            maxNumberOfSubjectsPerStage, NA_real_,
            notApplicableIfNA = TRUE
        )
    } else if (endpoint == "survival") {
        .setValueAndParameterType(simulationResults, "plannedEvents", plannedEvents, NA_real_)
        .setValueAndParameterType(simulationResults, "minNumberOfEventsPerStage",
            minNumberOfEventsPerStage, NA_real_,
            notApplicableIfNA = TRUE
        )
        .setValueAndParameterType(simulationResults, "maxNumberOfEventsPerStage",
            maxNumberOfEventsPerStage, NA_real_,
            notApplicableIfNA = TRUE
        )
    }
    .setValueAndParameterType(simulationResults, "conditionalPower",
        conditionalPower, NA_real_,
        notApplicableIfNA = TRUE
    )
    if (endpoint %in% c("means", "survival")) {
        .setValueAndParameterType(simulationResults, "thetaH1", thetaH1, NA_real_, notApplicableIfNA = TRUE)
    }
    if (endpoint == "means") {
        .setValueAndParameterType(simulationResults, "stDevH1", stDevH1, NA_real_, notApplicableIfNA = TRUE)
    }
    .setValueAndParameterType(
        simulationResults, "maxNumberOfIterations",
        as.integer(maxNumberOfIterations), C_MAX_SIMULATION_ITERATIONS_DEFAULT
    )
    simulationResults$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    simulationResults$seed <- .setSeed(seed)

    if (is.null(adaptations) || all(is.na(adaptations))) {
        adaptations <- rep(TRUE, kMax - 1)
    }
    if (length(adaptations) != kMax - 1) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'adaptations' must have length ", (kMax - 1), " (kMax - 1)")
    }
    .setValueAndParameterType(simulationResults, "adaptations", adaptations, rep(TRUE, kMax - 1))

    simulationResults$effectList <- effectList
    simulationResults$.setParameterType("effectList", C_PARAM_USER_DEFINED)

    simulationResults$populations <- as.integer(gMax)
    simulationResults$.setParameterType("populations", C_PARAM_DERIVED)

    .setValueAndParameterType(
        simulationResults, "stratifiedAnalysis", stratifiedAnalysis,
        C_STRATIFIED_ANALYSIS_DEFAULT
    )

    if (typeOfSelection != "userDefined") {
        .setValueAndParameterType(simulationResults, "threshold", threshold, -Inf)
        .setValueAndParameterType(simulationResults, "epsilonValue", epsilonValue, NA_real_)
        .setValueAndParameterType(simulationResults, "rValue", rValue, NA_real_)
    }
    .setValueAndParameterType(simulationResults, "intersectionTest", intersectionTest, C_INTERSECTION_TEST_ENRICHMENT_DEFAULT)
    .setValueAndParameterType(simulationResults, "typeOfSelection", typeOfSelection, C_TYPE_OF_SELECTION_DEFAULT)
    .setValueAndParameterType(simulationResults, "successCriterion", successCriterion, C_SUCCESS_CRITERION_DEFAULT)
    .setValueAndParameterType(simulationResults, "effectMeasure", effectMeasure, C_EFFECT_MEASURE_DEFAULT)

    warning("Simulation of enrichment designs is experimental and hence not fully validated (see www.rpact.com/experimental)", call. = FALSE)

    return(simulationResults)
}
