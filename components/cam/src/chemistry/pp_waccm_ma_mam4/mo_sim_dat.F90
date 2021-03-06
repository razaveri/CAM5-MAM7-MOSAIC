
      module mo_sim_dat

      private
      public :: set_sim_dat

      contains

      subroutine set_sim_dat

      use chem_mods,     only : clscnt, cls_rxt_cnt, clsmap, permute, adv_mass, fix_mass, crb_mass
      use chem_mods,     only : diag_map
      use chem_mods,     only : phtcnt, rxt_tag_cnt, rxt_tag_lst, rxt_tag_map
      use chem_mods,     only : pht_alias_lst, pht_alias_mult
      use chem_mods,     only : extfrc_lst, inv_lst, slvd_lst
      use chem_mods,     only : enthalpy_cnt, cph_enthalpy, cph_rid
      use cam_abortutils,only : endrun
      use mo_tracname,   only : solsym
      use chem_mods,     only : frc_from_dataset
      use shr_kind_mod,  only : r8 => shr_kind_r8
      use cam_logfile,   only : iulog

      implicit none

!--------------------------------------------------------------
!      ... local variables
!--------------------------------------------------------------
      integer :: ios

      clscnt(:) = (/     23,     0,     0,    75,     0 /)

      cls_rxt_cnt(:,1) = (/      3,    60,     0,    23 /)
      cls_rxt_cnt(:,4) = (/     31,   121,   146,    75 /)

      solsym(: 98) = (/ 'O3              ','O               ','O2              ','N2O             ','N               ', &
                        'NO              ','NO2             ','NO3             ','HNO3            ','HO2NO2          ', &
                        'N2O5            ','CH4             ','CH3O2           ','CH3OOH          ','CH2O            ', &
                        'CO              ','H2              ','H               ','H2O2            ','CLY             ', &
                        'BRY             ','CL2             ','CLO             ','OCLO            ','CL2O2           ', &
                        'HCL             ','HOCL            ','CLONO2          ','BRCL            ','BRO             ', &
                        'HBR             ','HOBR            ','BRONO2          ','CH3CL           ','CH3BR           ', &
                        'CFC11           ','CFC12           ','CFC113          ','HCFC22          ','CCL4            ', &
                        'CH3CCL3         ','CF3BR           ','CF2CLBR         ','HCFC141B        ','HCFC142B        ', &
                        'CFC114          ','CFC115          ','H1202           ','H2402           ','CHBR3           ', &
                        'CH2BR2          ','COF2            ','COFCL           ','HF              ','F               ', &
                        'CO2             ','OCS             ','S               ','SO              ','SO2             ', &
                        'SO3             ','H2SO4           ','DMS             ','SOAG            ','so4_a1          ', &
                        'pom_a1          ','soa_a1          ','bc_a1           ','dst_a1          ','ncl_a1          ', &
                        'num_a1          ','so4_a2          ','soa_a2          ','ncl_a2          ','num_a2          ', &
                        'dst_a2          ','dst_a3          ','ncl_a3          ','so4_a3          ','num_a3          ', &
                        'pom_a4          ','bc_a4           ','num_a4          ','CL              ','BR              ', &
                        'OH              ','HO2             ','N2p             ','O2p             ','Np              ', &
                        'Op              ','NOp             ','e               ','N2D             ','O2_1S           ', &
                        'O2_1D           ','O1D             ','H2O             ' /)

      adv_mass(: 98) = (/    47.998200_r8,    15.999400_r8,    31.998800_r8,    44.012880_r8,    14.006740_r8, &
                             30.006140_r8,    46.005540_r8,    62.004940_r8,    63.012340_r8,    79.011740_r8, &
                            108.010480_r8,    16.040600_r8,    47.032000_r8,    48.039400_r8,    30.025200_r8, &
                             28.010400_r8,     2.014800_r8,     1.007400_r8,    34.013600_r8,   100.916850_r8, &
                             99.716850_r8,    70.905400_r8,    51.452100_r8,    67.451500_r8,   102.904200_r8, &
                             36.460100_r8,    52.459500_r8,    97.457640_r8,   115.356700_r8,    95.903400_r8, &
                             80.911400_r8,    96.910800_r8,   141.908940_r8,    50.485900_r8,    94.937200_r8, &
                            137.367503_r8,   120.913206_r8,   187.375310_r8,    86.467906_r8,   153.821800_r8, &
                            133.402300_r8,   148.910210_r8,   165.364506_r8,   116.948003_r8,   100.493706_r8, &
                            170.921013_r8,   154.466716_r8,   209.815806_r8,   259.823613_r8,   252.730400_r8, &
                            173.833800_r8,    66.007206_r8,    82.461503_r8,    20.005803_r8,    18.998403_r8, &
                             44.009800_r8,    60.076400_r8,    32.066000_r8,    48.065400_r8,    64.064800_r8, &
                             80.064200_r8,    98.078400_r8,    62.132400_r8,    12.011000_r8,   115.107340_r8, &
                             12.011000_r8,    12.011000_r8,    12.011000_r8,   135.064039_r8,    58.442468_r8, &
                              1.007400_r8,   115.107340_r8,    12.011000_r8,    58.442468_r8,     1.007400_r8, &
                            135.064039_r8,   135.064039_r8,    58.442468_r8,   115.107340_r8,     1.007400_r8, &
                             12.011000_r8,    12.011000_r8,     1.007400_r8,    35.452700_r8,    79.904000_r8, &
                             17.006800_r8,    33.006200_r8,    28.013480_r8,    31.998800_r8,    14.006740_r8, &
                             15.999400_r8,    30.006140_r8, 0.548567E-03_r8,    14.006740_r8,    31.998800_r8, &
                             31.998800_r8,    15.999400_r8,    18.014200_r8 /)

      crb_mass(: 98) = (/     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,    12.011000_r8,    12.011000_r8,    12.011000_r8,    12.011000_r8, &
                             12.011000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,    12.011000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,    12.011000_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,    24.022000_r8,    12.011000_r8,    12.011000_r8, &
                             24.022000_r8,    12.011000_r8,    12.011000_r8,    24.022000_r8,    24.022000_r8, &
                             24.022000_r8,    24.022000_r8,    12.011000_r8,    24.022000_r8,    12.011000_r8, &
                             12.011000_r8,    12.011000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8, &
                             12.011000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,    24.022000_r8,    12.011000_r8,     0.000000_r8, &
                             12.011000_r8,    12.011000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                             12.011000_r8,    12.011000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8,     0.000000_r8, &
                              0.000000_r8,     0.000000_r8,     0.000000_r8 /)

      fix_mass(:  2) = (/ 0.00000000_r8, 28.0134800_r8 /)

      clsmap(: 23,1) = (/   12,   4,  34,  35,  36,  37,  38,  46,  47,  39, &
                            44,  45,  40,  41,  42,  43,  48,  49,  50,  51, &
                            56,  20,  21 /)
      clsmap(: 75,4) = (/    1,   2,  97,   3,  95,  96,  17,  16,   5,   6, &
                             7,  86,   8,   9,  10,  11,  13,  14,  15,  18, &
                            87,  19,  98,  84,  22,  23,  24,  25,  26,  27, &
                            28,  29,  85,  30,  31,  32,  33,  88,  89,  90, &
                            91,  92,  94,  93,  52,  53,  54,  55,  57,  58, &
                            59,  60,  61,  62,  63,  64,  65,  66,  67,  68, &
                            69,  70,  71,  72,  73,  74,  75,  76,  77,  78, &
                            79,  80,  81,  82,  83 /)

      permute(: 75,4) = (/   64,  75,  74,  70,  25,  24,  60,  39,  51,  65, &
                             66,  67,  68,  55,  37,  32,  72,  35,  58,  57, &
                             71,  41,  69,  61,  26,  62,  33,  21,  73,  52, &
                             54,  28,  63,  59,  50,  49,  40,  42,  46,  36, &
                             44,  47,  38,  48,  22,  27,  29,  45,  34,  43, &
                             56,  53,  30,  23,  31,   1,   2,   3,   4,   5, &
                              6,   7,   8,   9,  10,  11,  12,  13,  14,  15, &
                             16,  17,  18,  19,  20 /)

      diag_map(: 75) = (/    1,   2,   3,   4,   5,   6,   7,   8,   9,  10, &
                            11,  12,  13,  14,  15,  16,  17,  18,  19,  20, &
                            21,  24,  27,  30,  33,  35,  37,  41,  44,  48, &
                            52,  58,  64,  69,  77,  83,  90,  97, 102, 106, &
                           114, 122, 132, 140, 150, 158, 167, 179, 189, 197, &
                           209, 219, 227, 235, 247, 258, 272, 284, 307, 327, &
                           349, 377, 399, 423, 447, 475, 513, 539, 565, 597, &
                           623, 643, 668, 693, 735 /)

      extfrc_lst(: 22) = (/ 'NO              ','NO2             ','CO              ','SO2             ','DMS             ', &
                            'so4_a1          ','so4_a2          ','pom_a1          ','pom_a4          ','bc_a1           ', &
                            'bc_a4           ','num_a1          ','num_a2          ','num_a4          ','Op              ', &
                            'O2p             ','Np              ','N2p             ','N2D             ','N               ', &
                            'e               ','OH              ' /)

      frc_from_dataset(: 22) = (/ .true., .true., .true., .true., .true., &
                                  .true., .true., .true., .true., .true., &
                                  .true., .true., .true., .true., .false., &
                                  .false., .false., .false., .false., .false., &
                                  .false., .false. /)

      inv_lst(:  2) = (/ 'M               ', 'N2              ' /)

      slvd_lst(: 14) = (/ 'CL              ', 'BR              ', 'OH              ', 'HO2             ', 'Op              ', &
                          'O2p             ', 'NOp             ', 'Np              ', 'N2p             ', 'e               ', &
                          'O2_1S           ', 'O2_1D           ', 'N2D             ', 'O1D             ' /)

      if( allocated( rxt_tag_lst ) ) then
         deallocate( rxt_tag_lst )
      end if
      allocate( rxt_tag_lst(rxt_tag_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate rxt_tag_lst; error = ',ios
         call endrun
      end if
      if( allocated( rxt_tag_map ) ) then
         deallocate( rxt_tag_map )
      end if
      allocate( rxt_tag_map(rxt_tag_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate rxt_tag_map; error = ',ios
         call endrun
      end if
      rxt_tag_lst(:rxt_tag_cnt) = (/ 'jo2_a           ', 'jo2_b           ', 'jo3_a           ', 'jo3_b           ', &
                                     'jn2o            ', 'jno             ', 'jno_i           ', 'jno2            ', &
                                     'jn2o5_a         ', 'jn2o5_b         ', 'jhno3           ', 'jno3_a          ', &
                                     'jno3_b          ', 'jho2no2_a       ', 'jho2no2_b       ', 'jch3ooh         ', &
                                     'jch2o_a         ', 'jch2o_b         ', 'jh2o_a          ', 'jh2o_b          ', &
                                     'jh2o_c          ', 'jh2o2           ', 'jcl2            ', 'jclo            ', &
                                     'joclo           ', 'jcl2o2          ', 'jhocl           ', 'jhcl            ', &
                                     'jclono2_a       ', 'jclono2_b       ', 'jbrcl           ', 'jbro            ', &
                                     'jhobr           ', 'jhbr            ', 'jbrono2_a       ', 'jbrono2_b       ', &
                                     'jch3cl          ', 'jccl4           ', 'jch3ccl3        ', 'jcfcl3          ', &
                                     'jcf2cl2         ', 'jcfc113         ', 'jcfc114         ', 'jcfc115         ', &
                                     'jhcfc22         ', 'jhcfc141b       ', 'jhcfc142b       ', 'jch3br          ', &
                                     'jcf3br          ', 'jcf2clbr        ', 'jchbr3          ', 'jch2br2         ', &
                                     'jh1202          ', 'jh2402          ', 'jcof2           ', 'jcofcl          ', &
                                     'jhf             ', 'jco2            ', 'jch4_a          ', 'jch4_b          ', &
                                     'jh2so4          ', 'jso2            ', 'jso3            ', 'jocs            ', &
                                     'jso             ', 'jeuv_1          ', 'jeuv_2          ', 'jeuv_3          ', &
                                     'jeuv_4          ', 'jeuv_5          ', 'jeuv_6          ', 'jeuv_7          ', &
                                     'jeuv_8          ', 'jeuv_9          ', 'jeuv_10         ', 'jeuv_11         ', &
                                     'jeuv_12         ', 'jeuv_13         ', 'jeuv_14         ', 'jeuv_15         ', &
                                     'jeuv_16         ', 'jeuv_17         ', 'jeuv_18         ', 'jeuv_19         ', &
                                     'jeuv_20         ', 'jeuv_21         ', 'jeuv_22         ', 'jeuv_23         ', &
                                     'jeuv_24         ', 'jeuv_25         ', 'jeuv_26         ', 'usr_O_O2        ', &
                                     'O_O3            ', 'usr_O_O         ', 'O2_1S_O         ', 'O2_1S_O2        ', &
                                     'O2_1S_N2        ', 'O2_1S_O3        ', 'O2_1S_CO2       ', 'ag2             ', &
                                     'O2_1D_O         ', 'O2_1D_O2        ', 'O2_1D_N2        ', 'ag1             ', &
                                     'O1D_N2          ', 'O1D_O2          ', 'O1D_O2b         ', 'O1D_H2O         ', &
                                     'O1D_N2Oa        ', 'O1D_N2Ob        ', 'O1D_O3          ', 'O1D_CFC11       ', &
                                     'O1D_CFC12       ', 'O1D_CFC113      ', 'O1D_CFC114      ', 'O1D_CFC115      ', &
                                     'O1D_HCFC22      ', 'O1D_HCFC141B    ', 'O1D_HCFC142B    ', 'O1D_CCL4        ', &
                                     'O1D_CH3BR       ', 'O1D_CF2CLBR     ', 'O1D_CF3BR       ', 'O1D_H1202       ', &
                                     'O1D_H2402       ', 'O1D_CHBR3       ', 'O1D_CH2BR2      ', 'O1D_COF2        ', &
                                     'O1D_COFCL       ', 'O1D_CH4a        ', 'O1D_CH4b        ', 'O1D_CH4c        ', &
                                     'O1D_H2          ', 'O1D_HCL         ', 'O1D_HBR         ', 'H_O2            ', &
                                     'H_O3            ', 'H_HO2a          ', 'H_HO2           ', 'H_HO2b          ', &
                                     'OH_O            ', 'OH_O3           ', 'OH_HO2          ', 'OH_OH           ', &
                                     'OH_OH_M         ', 'OH_H2           ', 'OH_H2O2         ', 'H2_O            ', &
                                     'HO2_O           ', 'HO2_O3          ', 'usr_HO2_HO2     ', 'H2O2_O          ', &
                                     'N2D_O2          ', 'N2D_O           ', 'N_OH            ', 'N_O2            ', &
                                     'N_NO            ', 'N_NO2a          ', 'N_NO2b          ', 'N_NO2c          ', &
                                     'NO_O            ', 'NO_HO2          ', 'NO_O3           ', 'NO2_O           ', &
                                     'NO2_O_M         ', 'NO2_O3          ', 'tag_NO2_NO3     ', 'usr_N2O5_M      ', &
                                     'tag_NO2_OH      ', 'usr_HNO3_OH     ', 'NO3_NO          ', 'NO3_O           ', &
                                     'NO3_OH          ', 'NO3_HO2         ', 'tag_NO2_HO2     ', 'HO2NO2_OH       ', &
                                     'usr_HO2NO2_M    ', 'CL_O3           ', 'CL_H2           ', 'CL_H2O2         ', &
                                     'CL_HO2a         ', 'CL_HO2b         ', 'CL_CH2O         ', 'CL_CH4          ', &
                                     'CLO_O           ', 'CLO_OHa         ', 'CLO_OHb         ', 'CLO_HO2         ', &
                                     'CLO_CH3O2       ', 'CLO_NO          ', 'CLO_NO2_M       ', 'CLO_CLOa        ', &
                                     'CLO_CLOb        ', 'CLO_CLOc        ', 'tag_CLO_CLO_M   ', 'usr_CL2O2_M     ', &
                                     'HCL_OH          ', 'HCL_O           ', 'HOCL_O          ', 'HOCL_CL         ', &
                                     'HOCL_OH         ', 'CLONO2_O        ', 'CLONO2_OH       ', 'CLONO2_CL       ', &
                                     'BR_O3           ', 'BR_HO2          ', 'BR_CH2O         ', 'BRO_O           ', &
                                     'BRO_OH          ', 'BRO_HO2         ', 'BRO_NO          ', 'BRO_NO2_M       ', &
                                     'BRO_CLOa        ', 'BRO_CLOb        ', 'BRO_CLOc        ', 'BRO_BRO         ', &
                                     'HBR_OH          ', 'HBR_O           ', 'HOBR_O          ', 'BRONO2_O        ', &
                                     'F_H2O           ', 'F_H2            ', 'F_CH4           ', 'F_HNO3          ', &
                                     'CH3CL_CL        ', 'CH3CL_OH        ', 'CH3CCL3_OH      ', 'HCFC22_OH       ', &
                                     'CH3BR_OH        ', 'CH3BR_CL        ', 'HCFC141B_OH     ', 'HCFC142B_OH     ', &
                                     'CH2BR2_OH       ', 'CHBR3_OH        ', 'CH2BR2_CL       ', 'CHBR3_CL        ', &
                                     'CH4_OH          ', 'usr_CO_OH_b     ', 'CO_OH_M         ', 'CH2O_NO3        ', &
                                     'CH2O_OH         ', 'CH2O_O          ', 'CH3O2_NO        ', 'CH3O2_HO2       ', &
                                     'CH3OOH_OH       ', 'usr_N2O5_aer    ', 'usr_NO3_aer     ', 'usr_NO2_aer     ', &
                                     'usr_HO2_aer     ', 'OCS_O           ', 'OCS_OH          ', 'S_OH            ', &
                                     'S_O2            ', 'S_O3            ', 'SO_OH           ', 'SO_O2           ', &
                                     'SO_O3           ', 'SO_NO2          ', 'SO_CLO          ', 'SO_BRO          ', &
                                     'SO_OCLO         ', 'usr_SO2_OH      ', 'usr_SO3_H2O     ', 'usr_DMS_OH      ', &
                                     'DMS_OHb         ', 'DMS_NO3         ', 'het1            ', 'het2            ', &
                                     'het3            ', 'het4            ', 'het5            ', 'het6            ', &
                                     'het7            ', 'het8            ', 'het9            ', 'het10           ', &
                                     'het11           ', 'het12           ', 'het13           ', 'het14           ', &
                                     'het15           ', 'het16           ', 'het17           ', 'ion_Op_O2       ', &
                                     'ion_Op_N2       ', 'ion_N2p_Oa      ', 'ion_N2p_Ob      ', 'ion_Op_CO2      ', &
                                     'ion_O2p_N       ', 'ion_O2p_NO      ', 'ion_Np_O2a      ', 'ion_Np_O2b      ', &
                                     'ion_Np_O        ', 'ion_N2p_O2      ', 'ion_O2p_N2      ', 'elec1           ', &
                                     'elec2           ', 'elec3           ' /)
      rxt_tag_map(:rxt_tag_cnt) = (/    1,   2,   3,   4,   5,   6,   7,   8,   9,  10, &
                                       11,  12,  13,  14,  15,  16,  17,  18,  19,  20, &
                                       21,  22,  23,  24,  25,  26,  27,  28,  29,  30, &
                                       31,  32,  33,  34,  35,  36,  37,  38,  39,  40, &
                                       41,  42,  43,  44,  45,  46,  47,  48,  49,  50, &
                                       51,  52,  53,  54,  55,  56,  57,  58,  59,  60, &
                                       61,  62,  63,  64,  65,  66,  67,  68,  69,  70, &
                                       71,  72,  73,  74,  75,  76,  77,  78,  79,  80, &
                                       81,  82,  83,  84,  85,  86,  87,  88,  89,  90, &
                                       91,  92,  93,  94,  95,  96,  97,  98,  99, 100, &
                                      101, 102, 103, 104, 105, 106, 107, 108, 109, 110, &
                                      111, 112, 113, 114, 115, 116, 117, 118, 119, 120, &
                                      121, 122, 123, 124, 125, 126, 127, 128, 129, 130, &
                                      131, 132, 133, 134, 135, 136, 137, 138, 139, 140, &
                                      141, 142, 143, 144, 145, 146, 147, 148, 149, 150, &
                                      151, 152, 153, 154, 155, 156, 157, 158, 159, 160, &
                                      161, 162, 163, 164, 165, 166, 167, 168, 169, 170, &
                                      171, 172, 173, 174, 175, 176, 177, 178, 179, 180, &
                                      181, 182, 183, 184, 185, 186, 187, 188, 189, 190, &
                                      191, 192, 193, 194, 195, 196, 197, 198, 199, 200, &
                                      201, 202, 203, 204, 205, 206, 207, 208, 209, 210, &
                                      211, 212, 213, 214, 215, 216, 217, 218, 219, 220, &
                                      221, 222, 223, 224, 225, 226, 227, 228, 229, 230, &
                                      231, 232, 233, 234, 235, 236, 237, 238, 239, 240, &
                                      241, 242, 243, 244, 245, 246, 247, 248, 249, 250, &
                                      251, 252, 253, 254, 255, 256, 257, 258, 259, 260, &
                                      261, 262, 263, 264, 265, 266, 267, 268, 269, 270, &
                                      271, 272, 273, 274, 275, 276, 277, 278, 279, 280, &
                                      281, 282, 283, 284, 285, 286, 287, 288, 289, 290, &
                                      291, 292, 293, 294, 295, 296, 297, 298 /)
      if( allocated( pht_alias_lst ) ) then
         deallocate( pht_alias_lst )
      end if
      allocate( pht_alias_lst(phtcnt,2),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate pht_alias_lst; error = ',ios
         call endrun
      end if
      if( allocated( pht_alias_mult ) ) then
         deallocate( pht_alias_mult )
      end if
      allocate( pht_alias_mult(phtcnt,2),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate pht_alias_mult; error = ',ios
         call endrun
      end if
      pht_alias_lst(:,1) = (/ 'userdefined     ', 'userdefined     ', '                ', '                ', &
                              '                ', 'userdefined     ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ' /)
      pht_alias_lst(:,2) = (/ '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ', 'userdefined     ', &
                              'userdefined     ', 'userdefined     ', 'userdefined     ' /)
      pht_alias_mult(:,1) = (/ 1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8 /)
      pht_alias_mult(:,2) = (/ 1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8 /)
      allocate( cph_enthalpy(enthalpy_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate cph_enthalpy; error = ',ios
         call endrun
      end if
      allocate( cph_rid(enthalpy_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate cph_rid; error = ',ios
         call endrun
      end if
      cph_rid(:)      = (/              92,             93,             94,             95,             96, &
                                        97,             98,            101,            102,            103, &
                                       105,            106,            107,            136,            137, &
                                       139,            141,            142,            143,            149, &
                                       150,            151,            153,            154,            156, &
                                       157,            162,            163,            164,            284, &
                                       285,            286,            289,            290,            291, &
                                       292,            293,            294,            296,            297, &
                                       298 /)
      cph_enthalpy(:) = (/   101.390000_r8,  392.190000_r8,  493.580000_r8,   62.600000_r8,   62.600000_r8, &
                              62.600000_r8,   62.600000_r8,   94.300000_r8,   94.300000_r8,   94.300000_r8, &
                             189.910000_r8,   32.910000_r8,  189.810000_r8,  203.400000_r8,  194.710000_r8, &
                             232.590000_r8,   67.670000_r8,  165.300000_r8,  293.620000_r8,  226.580000_r8, &
                             120.100000_r8,  165.510000_r8,  177.510000_r8,  229.610000_r8,  133.750000_r8, &
                             313.750000_r8,   34.470000_r8,  199.170000_r8,  193.020000_r8,  150.110000_r8, &
                             105.040000_r8,   67.530000_r8,  406.160000_r8,  271.380000_r8,  239.840000_r8, &
                             646.280000_r8,   95.550000_r8,  339.590000_r8,   82.389000_r8,  508.950000_r8, &
                             354.830000_r8 /)

      end subroutine set_sim_dat

      end module mo_sim_dat
