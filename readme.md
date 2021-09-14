# High-throughput Replica Pinning

This R script will require the next packages alongside with R-version >= 3.6.3:

```r
tidyverse
optparse
openxlsx
```

## Use

Both files, `High_Replica_Pinning.R` and `High_Replica_Pinning_Tools.R` needs to find in the same directory before to execute.

To execute the programm you need to open a **Terminal** in the working directory where the scripts are present. Then you can write the next command:

```bash
Rscript High_Replica_Pinning.R -i <Inputfile> -k <Keyfile_YKO> -t <Keyfile_TS> -p <PositionsFile> -o <Output_Dir> -s <Set>
```

Description:

* Required:
  * \<Inputfile\>     : File with the colony areas data. Every line need to separated by `\n`.
  * \<Keyfile_YKO\>   : Keyfile mutant library for YKO in csv format.
  * \<Keyfile_TS\>    : Keyfile mutant library for TS in csv format.
  * \<PositionsFile\> : File with the Positions to filter.

* Optional:
  * \<Set\>           : Plates set. Only two possible options "A" or "B". By default "A" is selected.
  * \<Output_Dir\>    : Output directory. In case of not adding a directory a new one will be created with the **Output** name.

This generate two excel files, one for **YKO** and **TS**. Each one with the next sheets:

* Raw
* Ordered
* Ranked
* Filtered
* Counting

Alongside with these files you will find a dataset example that can be used like this:

```bash
Rscript High_Replica_Pinning.R -i Data/colonyAreasA_complete.txt -k Data/MATa_YKO_Rothstein_updated.csv -t Data/tsMATaKeyFile-384.csv -p Data/Mata_YKO_Rothstein_Positions.csv -o Test/
```
