{-# LANGUAGE RecordWildCards #-}

import Clash.Shake
import Clash.Shake.Xilinx as Xilinx
import Data.Foldable (forM_, traverse_)
import Development.Shake
import Development.Shake.FilePath

outDir :: FilePath
outDir = "_build"

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_build"} $ do
  useConfig "build.mk"

  let boards =
        [ ("nexys-a7-50t", Xilinx.vivado nexysA750T)
        ]

  phony "clean" $ do
    putNormal $ "Cleaning files in " <> outDir
    removeFilesAfter outDir ["//*"]

  let mkRules circuit = do
        kit@ClashKit {..} <-
          clashRules
            (outDir </> "clash" <> circuit)
            Verilog
            ["src"]
            circuit
            [ "-Wno-partial-type-signatures",
              "-fclash-intwidth=32" -- To play nicely with Spartan 3 and 6
            ]
            $ return ()

        forM_ boards $ \(name, synth) -> do
          let dirName = name <> "-" <> circuit

          SynthKit {..} <- synth kit (outDir </> dirName) ("target" </> dirName) "Top"

          mapM_ (uncurry $ nestedPhony $ dirName) $
            ("bitfile", need [bitfile]) : phonies

  traverse_ mkRules ["Blink", "SevenSegmentMulti"]
