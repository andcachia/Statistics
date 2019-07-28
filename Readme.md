# Determining Influence of Major Events in Brexit on Financial Markets

This project demonstrates the impact that major news relating to Brexit has on the GBP/EUR market. Google Trends is used  to  identify  major  events  by  applying  the Relative  Strength Index  indicator,  with  a  standard  T  Test  verifying  the  statistical significance  of  the  abnormal  returns  generated  by  the  events.

---

#### Installation Instructions

If curl is not installed on the current pc, please issue the following command in the terminal:

```
sudo apt-get install libcurl4-openssl-dev
```

Once done, please open an R terminal and install the following package:

```
install.packages("TTR")
```

The package `TTR` allows us to make use of teechnical analysis indicators such as the RSI.

The script can then be run from the command line as follows:

```
R < BrexitMarketImpact.R --no-save
```

When run, the script will output the T Statistic bounds as well as the T Value for each financial assets.
It will also create a pdf file within the same directory with all the plots.