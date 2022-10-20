# keser-i2b2

This is a folder with codes to:
- Generate Embedding
- Evaluate Embedding
- Get Evaluation Plots

To run it, simply run the script **run.R**. But make sure you have the following files available:
- Hierarchy file, such as **MultiAxialHierarchy.csv**
- Relation pairs file, such as **AllRelationPairsWithNull.Rdata**
- Input data file, such as **rpdr_code_cooccurrence_victor_2019.csv**

You can tune the dimensions on varaible **dims**, each dimension may take 26-30 mins to run. The evaluation plots are saved as "Summary-XX-YY-ZZ.html", and the original summary data are saved as "Summary-XX-YY-ZZ.Rdata", where XX stands for starting dimension, YY stands for endding dimension and ZZ stands for Step. 

All outputs wiil be saved at folder **out_dir**. 

Make sure all the packages in file **R/get_eval_embed_funcs.R** are installed.
