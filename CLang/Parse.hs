module Nova.Parse (parse) where

import Nova.Parse.File
import Nova.Parse.Ops
import Nova.Parse.Synonym
import Nova.Parse.Use
import Nova.Ubi

parse :: [(FilePath, Indent)] -> IO [(String, Module)]
parse files = do
  let (specfiles,novafiles)  = partition (\(path,_) -> path `elem` specialFiles) files
  let ([usefile],specfiles') = partition (\(path,_) -> path == "use")            specfiles
  mods <- parseUse usefile
  let mods' = foldl parseSpecialFile mods $ sortBy (comparing fst) specfiles' -- puts "chain" and "ops" before "tag"
  return $ foldl parseFile setup novafiles

parseSpecialFile :: Setup -> (FilePath, Indent) -> Setup
parseSpecialFile setup (path,Indent ys) = case path of -- Compiler sorts out other entries
                                            "chain"   -> foldit parseChain 
                                            "enum"    -> 
                                            "ops"     ->
                                            "struct"  ->
                                            "synonym" ->
                                            "tag"     ->
                                            "type"    ->
                                            "union"   ->
