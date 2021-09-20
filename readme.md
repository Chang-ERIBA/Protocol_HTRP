# High-throughput Replica Pinning

This R script will require the next packages alongside with R-version >= 3.6.3 (To install R and R studio follow this [link](https://www.datacamp.com/community/tutorials/installing-R-windows-mac-ubuntu)):

```r
tidyverse # version 1.3.1
optparse # version 1.6.6
openxlsx # version 4.2.4
```

To install packages or update only copy the next command and execute them in R console:

```r
pkgs <- c("tidyverse", "optparse", "openxlsx")
install.packages(pkgs)
```

## Use

Both files, `High_Replica_Pinning.R` and `High_Replica_Pinning_Tools.R` needs to find in the same directory before to execute.

To execute the programm you need to open a **Terminal** in the working directory where the scripts are present. Then you can write the next command:

```bash
Rscript High_Replica_Pinning.R -i <Inputfile> -k <Keyfile>  -O <Output_Dir> --Filter <filterValue> --Median_NSP <MedianRatioNSP> --Median_SP <MedianRatioSP> --rep <Replicates>
```

Description:

* Required:
  * \<Inputfile\> : File with the colony areas data. The plate's names need to have the next format:
    * > \<Set\>\<PlateNumber\>_\<NSP-or-SP\>,\<Replicate\>

  For example, in the Data folder the `colonyAreasA_HR.txt` file a plate has the next name:

  > A10_NSP,1,.tif

  Which means Set A, Plate Number 10, Non-Selective Plate and Replicate 1.

  * \<Keyfile\>   : Key file in csv format. This file need to have the next names in the columns:
    * Plate #
    * Row
    * Column
    * ORF
    * Gene
    * Mutation (optional; if present it would be used to group and calculate Total Colonies.)

* Optional Parameters:
  * \<Replicates\>:  Numeric value to multiply the sum colonies from Non-Selective Plates. By default the value is **1**.
  * \<MedianRatioNSP\>: Numeric value between 0-1 to modify the Median used to calculate the colonies of Non-Selective Plates. By default the value is **0.5**.
  * \<MedianRatioSP\> : Numeric value between 0-1 to modify the Median used to calculate the colonies of Selective Plates. By default the value is **0.2**.
  * \<filterValue\>    : Numeric value with the threshold to filter the total colonies for Non-Selective Plates. By default the threshold is **12**.
  * \<Output_Dir\>      : Output directory. In case of not adding a directory a new one will be created with the **Output** name.

This generate an excel files with the next name:

> \<ColonyFileName\>\_\<MedianRatioNSP\>\_\<MedianRatioSP\>\_\<filterValue\>\_\<Date\>.xlsx

And the next sheets:

* Raw
* Ordered
* Ranked
* Filtered
* Counting

## Example

Alongside with these files you will find a dataset example that can be used like this:

```bash
Rscript High_Replica_Pinning.R -i Data/colonyAreasA_YKO.txt -k Data/MATa_YKO_Rothstein_updated.csv -O Test/
```

Or it can be used the **TS** screening plates with other parameters:

```bash
Rscript High_Replica_Pinning.R -i Data/colonyAreasA_TS.txt -k Data/tsMATaKeyFile-384.csv -O Test/ --Filter 15 --Median_NSP 0.6 --Median_SP 0.1
```
