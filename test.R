
source('doOneFunx.R')
res <- replicate( 2, doOne(SNOW,
                                              pr_corn_2012,
                                              pr_soy_2012,
                                              pr_wheat_2012,
                                              pr_rice_2012,
                                              pr_peanuts_2012,
                                              e_domestic,
                                              e_Corn_Belt,
                                              e_Central_Plains,
                                              e_Delta_States,
                                              e_Far_West,
                                              e_Lake_States,
                                              e_Northeast,
                                              e_Northern_Plains,
                                              e_Southeast,
                                              e_Southern_Plains,
                                              e_export,
                                              e_imports,
                                              w_domestic,
                                              w_export,
                                              ss_imports
 ))