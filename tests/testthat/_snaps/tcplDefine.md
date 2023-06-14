# empty returns full dictionary

    Code
      tcplDefine()
    Output
          invitrodb_table     invitrodb_field
       1:             sc0                s0id
       2:             sc0                acid
       3:             sc0                spid
       4:             sc0                apid
       5:             sc0                rowi
       6:             sc0                coli
       7:             sc0                wllt
       8:             sc0                wllq
       9:             sc0                conc
      10:             sc0                rval
      11:             sc0                srcf
      12:             sc1                s1id
      13:             sc1                s0id
      14:             sc1                acid
      15:             sc1                aeid
      16:             sc1                logc
      17:             sc1                bval
      18:             sc1                pval
      19:             sc1                resp
      20:             mc0                m0id
      21:             mc0                acid
      22:             mc0                spid
      23:             mc0                apid
      24:             mc0                rowi
      25:             mc0                coli
      26:             mc0                wllt
      27:             mc0                wllq
      28:             mc0                conc
      29:             mc0                rval
      30:             mc0                srcf
      31:             mc1                m1id
      32:             mc1                m0id
      33:             mc1                acid
      34:             mc1                cndx
      35:             mc1                repi
      36:        chemical                chid
      37:        chemical                casn
      38:        chemical                chnm
      39:        chemical dsstox_substance_id
      40:          sample                spid
      41:          sample                chid
      42:          sample                stkc
      43:          sample           stkc_unit
      44:          sample    tested_conc_unit
          invitrodb_table     invitrodb_field
                                                                                                                                                                                                                                                                                                                          description
       1:                                                                                                                                                                                                                                                                                                                  Level 0 ID
       2:                                                                                                                                                                                                                                                                                                          Assay component ID
       3:                                                                                                                                                                                                                                                                                                                   Sample ID
       4:                                                                                                                                                                                                                                                                                                              Assay plate ID
       5:                                                                                                                                                                                                                                                                                                       Assay plate row index
       6:                                                                                                                                                                                                                                                                                                    Assay plate column index
       7: Well type, where: t, Test compound; c, Gain-of-signal control in multiple concentrations; p, Gain-of-signal control in single concentration; n, Neutral/negative control; m, Loss-of-signal control in multiple concentrations; o, Loss-of-signal control in single concentration; b, Blank well; and v, Viability control.
       8:                                                                                                                                                                                                                                                                                      1 if the well quality was good, else 0
       9:                                                                                                                                                                                                                                                                                                 Concentration is micromolar
      10:                                                                                                                                                                                                                                                                               Raw assay component value/readout from vendor
      11:                                                                                                                                                                                                                                                                             Filename of the source file containing the data
      12:                                                                                                                                                                                                                                                                                                                  Level 1 ID
      13:                                                                                                                                                                                                                                                                                                                  Level 0 ID
      14:                                                                                                                                                                                                                                                                                                          Assay component ID
      15:                                                                                                                                                                                                                                                                                                 Assay component endpoint ID
      16:                                                                                                                                                                                                                                                                                                   Log base 10 concentration
      17:                                                                                                                                                                                                                                                                                                              Baseline value
      18:                                                                                                                                                                                                                                                                                                      Positive control value
      19:                                                                                                                                                                                                                                                                                                   Normalized response value
      20:                                                                                                                                                                                                                                                                                                                  Level 0 ID
      21:                                                                                                                                                                                                                                                                                                          Assay component ID
      22:                                                                                                                                                                                                                                                                                                                   Sample ID
      23:                                                                                                                                                                                                                                                                                                              Assay plate ID
      24:                                                                                                                                                                                                                                                                                                       Assay plate row index
      25:                                                                                                                                                                                                                                                                                                    Assay plate column index
      26: Well type, where: t, Test compound; c, Gain-of-signal control in multiple concentrations; p, Gain-of-signal control in single concentration; n, Neutral/negative control; m, Loss-of-signal control in multiple concentrations; o, Loss-of-signal control in single concentration; b, Blank well; and v, Viability control.
      27:                                                                                                                                                                                                                                                                                      1 if the well quality was good, else 0
      28:                                                                                                                                                                                                                                                                                                 Concentration is micromolar
      29:                                                                                                                                                                                                                                                                               Raw assay component value/readout from vendor
      30:                                                                                                                                                                                                                                                                             Filename of the source file containing the data
      31:                                                                                                                                                                                                                                                                                                                  Level 1 ID
      32:                                                                                                                                                                                                                                                                                                                  Level 0 ID
      33:                                                                                                                                                                                                                                                                                                          Assay component ID
      34:                                                                                                                                                                                                                                                                                                         Concentration index
      35:                                                                                                                                                                                                                                                                                                             Replicate index
      36:                                                                                                                                                                                                                                                                                                                 Chemical ID
      37:                                                                                                                                                                                                                                                                                                         CAS Registry Number
      38:                                                                                                                                                                                                                                                                                                               Chemical name
      39:                                                                                                                                                                                                                                 Unique identifier from U.S. EPA Distributed Structure-Searchable Toxicity (DSSTox) Database
      40:                                                                                                                                                                                                                                                                                                                   Sample ID
      41:                                                                                                                                                                                                                                                                                                                 Chemical ID
      42:                                                                                                                                                                                                                                                                                                         Stock concentration
      43:                                                                                                                                                                                                                                                                                                    Stock concentration unit
      44:                                                                                                                                                                                                                                           The concentration unit for the concentration values in the data-containing tables
                                                                                                                                                                                                                                                                                                                          description

# multiple values returns all necessary definitions

    Code
      tcplDefine(c("chemical", "sample"))
    Output
         invitrodb_table     invitrodb_field
      1:        chemical                chid
      2:        chemical                casn
      3:        chemical                chnm
      4:        chemical dsstox_substance_id
      5:          sample                spid
      6:          sample                chid
      7:          sample                stkc
      8:          sample           stkc_unit
      9:          sample    tested_conc_unit
                                                                                         description
      1:                                                                                 Chemical ID
      2:                                                                         CAS Registry Number
      3:                                                                               Chemical name
      4: Unique identifier from U.S. EPA Distributed Structure-Searchable Toxicity (DSSTox) Database
      5:                                                                                   Sample ID
      6:                                                                                 Chemical ID
      7:                                                                         Stock concentration
      8:                                                                    Stock concentration unit
      9:           The concentration unit for the concentration values in the data-containing tables

---

    Code
      tcplDefine(c("spid", "rval", "wllt"))
    Output
         invitrodb_table invitrodb_field
      1:             sc0            spid
      2:             sc0            wllt
      3:             sc0            rval
      4:             mc0            spid
      5:             mc0            wllt
      6:             mc0            rval
      7:          sample            spid
                                                                                                                                                                                                                                                                                                                         description
      1:                                                                                                                                                                                                                                                                                                                   Sample ID
      2: Well type, where: t, Test compound; c, Gain-of-signal control in multiple concentrations; p, Gain-of-signal control in single concentration; n, Neutral/negative control; m, Loss-of-signal control in multiple concentrations; o, Loss-of-signal control in single concentration; b, Blank well; and v, Viability control.
      3:                                                                                                                                                                                                                                                                               Raw assay component value/readout from vendor
      4:                                                                                                                                                                                                                                                                                                                   Sample ID
      5: Well type, where: t, Test compound; c, Gain-of-signal control in multiple concentrations; p, Gain-of-signal control in single concentration; n, Neutral/negative control; m, Loss-of-signal control in multiple concentrations; o, Loss-of-signal control in single concentration; b, Blank well; and v, Viability control.
      6:                                                                                                                                                                                                                                                                               Raw assay component value/readout from vendor
      7:                                                                                                                                                                                                                                                                                                                   Sample ID

---

    Code
      tcplDefine(c("sc0", "chid"))
    Output
          invitrodb_table invitrodb_field
       1:             sc0            s0id
       2:             sc0            acid
       3:             sc0            spid
       4:             sc0            apid
       5:             sc0            rowi
       6:             sc0            coli
       7:             sc0            wllt
       8:             sc0            wllq
       9:             sc0            conc
      10:             sc0            rval
      11:             sc0            srcf
      12:        chemical            chid
      13:          sample            chid
                                                                                                                                                                                                                                                                                                                          description
       1:                                                                                                                                                                                                                                                                                                                  Level 0 ID
       2:                                                                                                                                                                                                                                                                                                          Assay component ID
       3:                                                                                                                                                                                                                                                                                                                   Sample ID
       4:                                                                                                                                                                                                                                                                                                              Assay plate ID
       5:                                                                                                                                                                                                                                                                                                       Assay plate row index
       6:                                                                                                                                                                                                                                                                                                    Assay plate column index
       7: Well type, where: t, Test compound; c, Gain-of-signal control in multiple concentrations; p, Gain-of-signal control in single concentration; n, Neutral/negative control; m, Loss-of-signal control in multiple concentrations; o, Loss-of-signal control in single concentration; b, Blank well; and v, Viability control.
       8:                                                                                                                                                                                                                                                                                      1 if the well quality was good, else 0
       9:                                                                                                                                                                                                                                                                                                 Concentration is micromolar
      10:                                                                                                                                                                                                                                                                               Raw assay component value/readout from vendor
      11:                                                                                                                                                                                                                                                                             Filename of the source file containing the data
      12:                                                                                                                                                                                                                                                                                                                 Chemical ID
      13:                                                                                                                                                                                                                                                                                                                 Chemical ID

# one value returns all necessary definitions

    Code
      tcplDefine("chemical")
    Output
         invitrodb_table     invitrodb_field
      1:        chemical                chid
      2:        chemical                casn
      3:        chemical                chnm
      4:        chemical dsstox_substance_id
                                                                                         description
      1:                                                                                 Chemical ID
      2:                                                                         CAS Registry Number
      3:                                                                               Chemical name
      4: Unique identifier from U.S. EPA Distributed Structure-Searchable Toxicity (DSSTox) Database

---

    Code
      tcplDefine("spid")
    Output
         invitrodb_table invitrodb_field description
      1:             sc0            spid   Sample ID
      2:             mc0            spid   Sample ID
      3:          sample            spid   Sample ID

