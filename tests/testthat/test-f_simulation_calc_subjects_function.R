## |
## |  *Unit tests*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  RPACT package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File name: test-f_simulation_multiarm_survival.R
## |  Creation date: 06 February 2023, 12:14:51
## |  File version: $Revision: 7554 $
## |  Last changed: $Date: 2024-01-12 10:19:05 +0100 (Fr, 12 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Package Functions")

test_that(".regexprCalcSubjectsFunction", {
    expect_true(is.function(.regexprCalcSubjectsFunction))
    expect_error(.regexprCalcSubjectsFunction(pattern = "y", cmd = "x()", language = "R"))
    expect_equal(.regexprCalcSubjectsFunction(pattern = "x", cmd = "x()", language = "R")$value[1], 1)
})

test_that(".isCppCode ", {
    expect_true(is.function(.isCppCode))
    expect_true(!.isCppCode(NULL))
})

test_that(".getCalcSubjectsFunctionRCode", {
    C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL_ARGUMENTS <- list()
    expect_error(.getCalcSubjectsFunctionRCode())
    expect_error(.getCalcSubjectsFunctionRCode("thetaH0\nthetaH0", "C_SIMULATION_CALC_SUBJECTS_FUNCTION_BASE_SURVIVAL_ARGUMENTS"))
})

test_that(".getCalcSubjectsFunctionCppCode", {
    expect_error(.getCalcSubjectsFunctionCppCode())
    expect_error(.getCalcSubjectsFunctionCppCode("thetaH0\nthetaH0", "C_SIMULATION_CALC_SUBJECTS_FUNCTION_CPP_CODE"))
})

test_that(".getCalcSubjectsFunction", {
    expect_error(.getCalcSubjectsFunction())
    # means
    maxNumberOfSubjects <- 90
    informationRates <- c(0.2, 0.5, 1)
    plannedSubjects <- round(informationRates * maxNumberOfSubjects)
    design <- getDesignInverseNormal(
        futilityBounds = c(-0.5, 0.5),
        informationRates = informationRates
    )
    x_means <- getSimulationMeans(
        design = design, groups = 2, meanRatio = TRUE,
        thetaH0 = 0.4, plannedSubjects = plannedSubjects,
        maxNumberOfIterations = 500, allocationRatioPlanned = 3,
        stDev = 1.5, seed = 1234567890
    )
    expect_error(.getCalcSubjectsFunction(design = design, simulationResults = x_means, calcFunction = 45, expectedFunction = "getSimulationSurvivalStageEventsTemp", cppEnabled = TRUE))
    expect_error(.getCalcSubjectsFunction(design = design, simulationResults = x_means, calcFunction = "calcEventsFunctionSurvivalPtrTemp", expectedFunction = "calcEventsFunctionCppTemp", cppEnabled = TRUE))
})
