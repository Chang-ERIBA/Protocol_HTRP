# High-throughput Replica Pinning

This R script will require the next packages alongside with R-version >= 3.6.3:

```r
tidyverse # version 1.3.1
optparse # version 1.6.6
openxlsx # version 4.2.4
```

## Use

Both files, `High_Replica_Pinning.R` and `High_Replica_Pinning_Tools.R` needs to find in the same directory before to execute.

To execute the programm you need to open a **Terminal** in the working directory where the scripts are present. Then you can write the next command:

```bash
Rscript High_Replica_Pinning.R -i <Inputfile> -k <Keyfile>  -O <Output_Dir> --Filter <filter_value> --Median_NSP <Median_Ratio_NSP> --Median_SP <Median_Ratio_SP>
```

Description:

* Required:
  * \<Inputfile\> : File with the colony areas data. Every line need to separated by `\n`.
  * \<Keyfile\>   : Keyfile mutant library for YKO in csv format.

* Optional:
  * \<filter_value\>    : Numeric value with the threshold to filter the total colonies for Non-Selective Plates. By default the threshold is **12**.
  * \<Median_Ratio_NSP\>: Numeric value between 0-1 to modify the Median used to calculate the colonies of Non-Selective Plates. By default the value is **0.5**.
  * \<Median_Ratio_SP\> : Numeric value between 0-1 to modify the Median used to calculate the colonies of Selective Plates. By default the value is **0.2**.
  * \<Output_Dir\>      : Output directory. In case of not adding a directory a new one will be created with the **Output** name.

This generate an excel files with the next name:
> \<ColonyFileName\>\_\<Median_Ratio_NSP\>\_\<Median_Ratio_SP\>\_\<filter_value\>\_\<Date\>.xlsx

And the next sheets:

* Raw
* Ordered
* Ranked
* Filtered
* Counting

## Example

Alongside with these files you will find a dataset example that can be used like this:

```bash
Rscript High_Replica_Pinning.R -i Data/colonyAreasA_complete.txt -k Data/MATa_YKO_Rothstein_updated.csv -O Test/
```

Or it can be used the **TS** screening plates with other parameters:

```bash
Rscript High_Replica_Pinning.R -i Data/colonyAreasA_TS.txt -k Data/tsMATaKeyFile-384.csv -O Test/ --Filter 15 --Median_NSP 0.6 --Median_SP 0.1
```
