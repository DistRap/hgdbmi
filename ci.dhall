let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

let defSteps = haskellCi.defaultCabalSteps

in  haskellCi.generalCi
      ( defSteps
        with extraSteps.pre
             =
              defSteps.extraSteps.pre
            # [ haskellCi.BuildStep.Name
                  { name = "Install gcc gdb util-linux"
                  , run = "sudo apt install gcc gdb util-linux"
                  }
              ]
      )
      haskellCi.DhallMatrix::{=}
