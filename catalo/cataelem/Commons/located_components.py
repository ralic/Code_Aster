# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================

from cataelem.Tools.base_objects import LocatedComponents, objects_from_context
import cataelem.Commons.physical_quantities as PHY


# Modes locaux de type ELEM :
#----------------------------------

CGEOM2D = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
                            components=('X', 'Y',))

CGEOM3D = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
                            components=('X', 'Y', 'Z',))

CBORNPI = LocatedComponents(phys=PHY.PILO_R, type='ELEM',
                            components=('A0', 'A1',))

CCACABL = LocatedComponents(phys=PHY.CACABL, type='ELEM',
                            components=('SECT', 'TENS',))

CCAGNBA = LocatedComponents(phys=PHY.CAGNBA, type='ELEM',
                            components=('A1',))

CCAGNP1 = LocatedComponents(phys=PHY.CAGNPO, type='ELEM',
                            components=('A1', 'IY1', 'IZ1', 'AY1', 'AZ1',
                                        'EY1', 'EZ1', 'JX1', 'JG1', 'IYR21',
                                        'IZR21',))

CCAGRPO = LocatedComponents(phys=PHY.CAGEPO, type='ELEM',
                            components=('HY1', 'HZ1', 'EPY1', 'EPZ1', 'HY2',
                                        'HZ2', 'EPY2', 'EPZ2', 'R1', 'EP1',
                                        'R2', 'EP2', 'TSEC',))

CCDTAU = LocatedComponents(phys=PHY.PILO_R, type='ELEM',
                           components=('A0',))

CCINFDI = LocatedComponents(phys=PHY.CINFDI, type='ELEM',
                            components=('REPK', 'REPM', 'REPA', 'SYMK', 'SYMM',
                                        'SYMA', 'DISK', 'DISM', 'DISA', 'ETAK',
                                        'TYDI',))

CCOEFC = LocatedComponents(phys=PHY.IMPE_C, type='ELEM',
                           components=('IMPE',))

CCOEFR = LocatedComponents(phys=PHY.IMPE_R, type='ELEM',
                           components=('IMPE',))

CCOMPO2 = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
                            components=('NBVARI',))

CMLCOMP = LocatedComponents(phys=PHY.MULTCOMP, type='ELEM',
                            components=('SD_COMP',))

CCONFR = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                           components=('X[28]',))

CCONPT = LocatedComponents(phys=PHY.N120_R, type='ELEM',
                           components=('X[34]',))

CCONSTR = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X1',))

CDECENT = LocatedComponents(phys=PHY.NEUT_K24, type='ELEM',
                            components=('Z1',))

CFER1_R = LocatedComponents(phys=PHY.FER1_R, type='ELEM',
                            components=(
                                'TYPCOMB', 'ENROBG', 'CEQUI', 'SIGACI', 'SIGBET',
                            'PIVA', 'PIVB', 'ES',))

CFER2_R = LocatedComponents(phys=PHY.FER2_R, type='ELEM',
                            components=(
                                'DNSXI', 'DNSXS', 'DNSYI', 'DNSYS', 'DNST',
                            'SIGMBE', 'EPSIBE',))

CFISSR = LocatedComponents(phys=PHY.FISS_R, type='ELEM',
                           components=('XA', 'YA', 'XTAN', 'YTAN', 'XNORM', 'YNORM',))

CFLAPLA = LocatedComponents(phys=PHY.FLAPLA, type='ELEM',
                            components=('NOMAIL', 'NOGEOM',))

CFLUXVF = LocatedComponents(phys=PHY.FLUX_F, type='ELEM',
                            components=('FLUX', 'FLUY',))

CFORCEC = LocatedComponents(phys=PHY.FORC_C, type='ELEM',
                            components=('FX', 'FY', 'FZ', 'MX', 'MY',
                                        'MZ', 'REP',))

CFRELEC = LocatedComponents(phys=PHY.FELECR, type='ELEM',
                            components=('X1', 'Y1', 'Z1', 'X2', 'Y2',
                                        'Z2', 'CODE',))

CFREQR = LocatedComponents(phys=PHY.FREQ_R, type='ELEM',
                           components=('FREQ',))

CFTRC = LocatedComponents(phys=PHY.ADRSJEVN, type='ELEM',
                          components=('I[2]',))

CHARMON = LocatedComponents(phys=PHY.HARMON, type='ELEM',
                            components=('NH',))

CHECHPF = LocatedComponents(phys=PHY.COEH_F, type='ELEM',
                            components=('H',))

CIMPEDC = LocatedComponents(phys=PHY.IMPE_C, type='ELEM',
                            components=('IMPE',))

CINSTPR = LocatedComponents(phys=PHY.INST_R, type='ELEM',
                            components=('INST',))

CITERAT = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X1',))

CLISTMA = LocatedComponents(phys=PHY.LISTMA, type='ELEM',
                            components=('LISTMA', 'TRANS',))

CMASDIA = LocatedComponents(phys=PHY.POSI, type='ELEM',
                            components=('POS',))

CMATERC = LocatedComponents(phys=PHY.ADRSJEVE, type='ELEM',
                            components=('I1',))

CNUMMOD = LocatedComponents(phys=PHY.NUMMOD, type='ELEM',
                            components=('NUM',))

COMEG2R = LocatedComponents(phys=PHY.OME2_R, type='ELEM',
                            components=('OMEG2',))

CONDPLA = LocatedComponents(phys=PHY.NEUT_K8, type='ELEM',
                            components=('Z[2]',))

CONDPLR = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X[5]',))

CPESANR = LocatedComponents(phys=PHY.PESA_R, type='ELEM',
                            components=('G', 'AG', 'BG', 'CG',))

CPHASIN_ = LocatedComponents(phys=PHY.VAR2_R, type='ELEM',
                             components=('V[5]',))

CRAYONF = LocatedComponents(phys=PHY.RAYO_F, type='ELEM',
                            components=('SIGMA', 'EPSIL', 'TPINF',))

CRAYONR = LocatedComponents(phys=PHY.RAYO_R, type='ELEM',
                            components=('SIGMA', 'EPSIL', 'TPINF',))

CREFERI = LocatedComponents(phys=PHY.NEUT_I, type='ELEM',
                            components=('X[12]',))

CREFERK = LocatedComponents(phys=PHY.NEUT_K24, type='ELEM',
                            components=('Z[19]',))

CROTATR = LocatedComponents(phys=PHY.ROTA_R, type='ELEM',
                            components=('OME', 'AR', 'BR', 'CR', 'X',
                                        'Y', 'Z',))

CSOURCF = LocatedComponents(phys=PHY.SOUR_F, type='ELEM',
                            components=('SOUR',))

CSOURCR = LocatedComponents(phys=PHY.SOUR_R, type='ELEM',
                            components=('SOUR',))

CSOUSOP = LocatedComponents(phys=PHY.NEUT_K24, type='ELEM',
                            components=('Z1',))

CSUROPT = LocatedComponents(phys=PHY.NEUT_K24, type='ELEM',
                            components=('Z1',))

CTEMPEF = LocatedComponents(phys=PHY.TEMP_F, type='ELEM',
                            components=('TEMP',))

CTEREFE = LocatedComponents(phys=PHY.TEMP_R, type='ELEM',
                            components=('TEMP',))

CTYPEPI = LocatedComponents(phys=PHY.PILO_K, type='ELEM',
                            components=('TYPE',))

CT_EXTR = LocatedComponents(phys=PHY.TEMP_R, type='ELEM',
                            components=('TEMP', 'TEMP_INF', 'TEMP_SUP',))

CVENTCX = LocatedComponents(phys=PHY.VENTCX_F, type='ELEM',
                            components=('FCXP',))

E102NEUT = LocatedComponents(phys=PHY.N816_R, type='ELEM',
                             components=('X[102]',))

E10NEUTI = LocatedComponents(phys=PHY.N120_I, type='ELEM',
                             components=('X[10]',))

E128NEUI = LocatedComponents(phys=PHY.N512_I, type='ELEM',
                             components=('X[128]',))

E12NEUTR = LocatedComponents(phys=PHY.N132_R, type='ELEM',
                             components=('X[12]',))

E132NEUR = LocatedComponents(phys=PHY.N132_R, type='ELEM',
                             components=('X[132]',))

E144NEUI = LocatedComponents(phys=PHY.N1280I, type='ELEM',
                             components=('X[144]',))

E14NEUTR = LocatedComponents(phys=PHY.N816_R, type='ELEM',
                             components=('X[14]',))

E15NEUTR = LocatedComponents(phys=PHY.N480_R, type='ELEM',
                             components=('X[15]',))

E162NEUR = LocatedComponents(phys=PHY.N2448R, type='ELEM',
                             components=('X[162]',))

E170NEUT = LocatedComponents(phys=PHY.N1360R, type='ELEM',
                             components=('X[170]',))

E18NEUI = LocatedComponents(phys=PHY.N120_I, type='ELEM',
                            components=('X[18]',))

E198NEUT = LocatedComponents(phys=PHY.N792_R, type='ELEM',
                             components=('X[198]',))

E1NEUK8 = LocatedComponents(phys=PHY.NEUT_K8, type='ELEM',
                            components=('Z1',))

E1NEUTI = LocatedComponents(phys=PHY.NEUT_I, type='ELEM',
                            components=('X1',))

E1NEUTR = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X1',))

E220NEUR = LocatedComponents(phys=PHY.N480_R, type='ELEM',
                             components=('X[220]',))

E22NEUTR = LocatedComponents(phys=PHY.N792_R, type='ELEM',
                             components=('X[22]',))

E24NEUTR = LocatedComponents(phys=PHY.N132_R, type='ELEM',
                             components=('X[24]',))

E28NEUTR = LocatedComponents(phys=PHY.N2448R, type='ELEM',
                             components=('X[28]',))

E2NEUTI = LocatedComponents(phys=PHY.N512_I, type='ELEM',
                            components=('X[2]',))

E2NEUTR = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X[2]',))

E306NEUT = LocatedComponents(phys=PHY.N2448R, type='ELEM',
                             components=('X[306]',))

E320NEUI = LocatedComponents(phys=PHY.N1280I, type='ELEM',
                             components=('X[320]',))

E32NEUTI = LocatedComponents(phys=PHY.N512_I, type='ELEM',
                             components=('X[32]',))

E32NEUTR = LocatedComponents(phys=PHY.N2448R, type='ELEM',
                             components=('X[32]',))

E35NEUTR = LocatedComponents(phys=PHY.N1360R, type='ELEM',
                             components=('X[35]',))

E36NEUI = LocatedComponents(phys=PHY.N1280I, type='ELEM',
                            components=('X[36]',))

E3NEUTI = LocatedComponents(phys=PHY.N120_I, type='ELEM',
                            components=('X[3]',))

E3NEUTR = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X[3]',))

E40NEUTR = LocatedComponents(phys=PHY.N1360R, type='ELEM',
                             components=('X[40]',))

E4NEUTR = LocatedComponents(phys=PHY.N792_R, type='ELEM',
                            components=('X[4]',))

E512NEUI = LocatedComponents(phys=PHY.N1280I, type='ELEM',
                             components=('X[512]',))

E54NEUTI = LocatedComponents(phys=PHY.N720_I, type='ELEM',
                             components=('X[54]',))

E54NEUTR = LocatedComponents(phys=PHY.N816_R, type='ELEM',
                             components=('X[54]',))

E55NEUTR = LocatedComponents(phys=PHY.N480_R, type='ELEM',
                             components=('X[55]',))

E60NEUTR = LocatedComponents(phys=PHY.N480_R, type='ELEM',
                             components=('X[60]',))

E6NEUTR = LocatedComponents(phys=PHY.N132_R, type='ELEM',
                            components=('X[6]',))

E72NEUI = LocatedComponents(phys=PHY.N1280I, type='ELEM',
                            components=('X[72]',))

E88NEUTR = LocatedComponents(phys=PHY.N792_R, type='ELEM',
                             components=('X[88]',))

E8NEUI = LocatedComponents(phys=PHY.N960_I, type='ELEM',
                           components=('X[8]',))

E90NEUTI = LocatedComponents(phys=PHY.N720_I, type='ELEM',
                             components=('X[90]',))

E9NEUTI = LocatedComponents(phys=PHY.N720_I, type='ELEM',
                            components=('X[9]',))

ECAFIEL = LocatedComponents(phys=PHY.CAFI_R, type='ELEM',
                            components=('YG', 'ZG', 'AIRE', 'YP', 'ZP',
                                        'GX', 'NUMGR',))

ECARAGE = LocatedComponents(phys=PHY.MASS_R, type='ELEM',
                            components=('M', 'CDGX', 'CDGY', 'CDGZ', 'IXX',
                                        'IYY', 'IZZ', 'IXY', 'IXZ', 'IYZ',
                                        'IXR2', 'IYR2',))

ECHALIM = LocatedComponents(phys=PHY.CHLI_R, type='ELEM',
                            components=('CHLI[3]',))

ECODRET = LocatedComponents(phys=PHY.CODE_I, type='ELEM',
                            components=('IRET',))

ECOOR1R = LocatedComponents(phys=PHY.N120_R, type='ELEM',
                            components=('X[81]',))

ECOURAN = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X1',))

EDCEL_I = LocatedComponents(phys=PHY.DCEL_I, type='ELEM',
                            components=('NPG_DYN', 'NCMP_DYN',))

EENECNO = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
                            components=('TOTALE', 'DX', 'DY', 'DZ', 'DRX',
                                        'DRY', 'DRZ',))

EENEDNO = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
                            components=('TOTALE', 'TRAC_COM', 'TORSION', 'FLEX_Y', 'FLEX_Z',))

EERREURT = LocatedComponents(phys=PHY.ERRE_R, type='ELEM',
                             components=(
                                 'ERTABS', 'ERTREL', 'TERMNO', 'TERMVO', 'TERMV2',
                             'TERMV1', 'TERMSA', 'TERMS2', 'TERMS1', 'TERMFL',
                             'TERMF2', 'TERMF1', 'TERMEC', 'TERME2', 'TERME1',))

EGTHETA = LocatedComponents(phys=PHY.G, type='ELEM',
                            components=('GTHETA',))

EHECHPR = LocatedComponents(phys=PHY.COEH_R, type='ELEM',
                            components=('H',))

EMASSINE = LocatedComponents(phys=PHY.MASS_R, type='ELEM',
                             components=('M', 'CDGX', 'CDGY', 'CDGZ', 'IXX',
                                         'IYY', 'IZZ', 'IXY', 'IXZ', 'IYZ',))

EMNEUT_I = LocatedComponents(phys=PHY.NEUT_I, type='ELEM',
                             components=('X1',))

ENONLIN = LocatedComponents(phys=PHY.NEUT_I, type='ELEM',
                            components=('X1',))

ENORME = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                           components=('X1',))

EPJSIGM = LocatedComponents(phys=PHY.SIEF_R, type='ELEM',
                            components=(
                                'SIG_NX', 'SIG_NY', 'SIG_NZ', 'SIG_N', 'SIG_TX',
                            'SIG_TY', 'SIG_TZ', 'SIG_T1X', 'SIG_T1Y', 'SIG_T1Z',
                            'SIG_T1', 'SIG_T2X', 'SIG_T2Y', 'SIG_T2Z', 'SIG_T2',))

EREFE1K = LocatedComponents(phys=PHY.NEUT_K8, type='ELEM',
                            components=('Z1',))

ERICTRA = LocatedComponents(phys=PHY.RICE_TRA, type='ELEM',
                            components=('TRIAX', 'RSR0', 'VOLU', 'NUMEMA', 'DEPSEQ',))

ESINGUL = LocatedComponents(phys=PHY.SING_R, type='ELEM',
                            components=('DEGRE', 'RAPPORT', 'TAILLE',))

ET_EXTR = LocatedComponents(phys=PHY.TEMP_R, type='ELEM',
                            components=('TEMP',))

EVOISIN = LocatedComponents(phys=PHY.VOISIN, type='ELEM',
                            components=('V0', 'V[6]', 'T0', 'T[6]',))

EWEIBUL = LocatedComponents(phys=PHY.WEIBULL, type='ELEM',
                            components=('DSIGWB',))

FISCO_I = LocatedComponents(phys=PHY.NEUT_I, type='ELEM',
                            components=('X[2]',))

I1NEUT_I = LocatedComponents(phys=PHY.NEUT_I, type='ELEM',
                             components=('X1',))

I3NEUT_I = LocatedComponents(phys=PHY.NEUT_I, type='ELEM',
                             components=('X[3]',))

MALPHAR = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X1',))

MDDLIMC = LocatedComponents(phys=PHY.DDLI_C, type='ELEM',
                            components=('C',))

MDDLIMF = LocatedComponents(phys=PHY.DDLI_F, type='ELEM',
                            components=('C',))

MDDLIMR = LocatedComponents(phys=PHY.DDLI_R, type='ELEM',
                            components=('C',))

MDDLMUR = LocatedComponents(phys=PHY.DDLM_R, type='ELEM',
                            components=('A1',))

MTEMPSR = LocatedComponents(phys=PHY.INST_R, type='ELEM',
                            components=('INST',))

N40NEUI = LocatedComponents(phys=PHY.N120_I, type='ELEM',
                            components=('X[40]',))

N80NEUI = LocatedComponents(phys=PHY.N120_I, type='ELEM',
                            components=('X[80]',))

NFAMILK = LocatedComponents(phys=PHY.NEUT_K8, type='ELEM',
                            components=('Z1',))

NINFORR = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X[26]',))


# Modes locaux de type ELNO :
#----------------------------------
DDL_NOZ1 = LocatedComponents(phys=PHY.SIZZ_R, type='ELNO',
                             components=('SIZZ',))

E1NNEUT = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                            components=('X1',))

E3NEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                             components=('X[3]',))

ECOEQNO = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
                            components=(
                                'VMIS', 'TRESCA', 'PRIN_[3]', 'VMIS_SG', 'VECT_1_X',
                            'VECT_1_Y', 'VECT_1_Z', 'VECT_2_X', 'VECT_2_Y', 'VECT_2_Z',
                            'VECT_3_X', 'VECT_3_Y', 'VECT_3_Z', 'TRSIG', 'TRIAX',))

ECONTPO = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
                            components=('SN', 'SVY', 'SVZ', 'SMT', 'SMFY',
                                        'SMFZ',))

ECOPILN = LocatedComponents(phys=PHY.PILO_R, type='ELNO',
                            components=('A0', 'A[3]', 'ETA',))

EDEFGNC = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
                            components=('EXX', 'EYY', 'EXY', 'KXX', 'KYY',
                                        'KXY', 'GAX', 'GAY',))

EDEFPNO = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
                            components=('EPXX', 'EPYY', 'EPZZ', 'EPXY',))

EDERANO = LocatedComponents(phys=PHY.DERA_R, type='ELNO',
                            components=(
                                'DCHA_V', 'DCHA_T', 'IND_DCHA', 'VAL_DCHA', 'X11',
                            'X22', 'X33', 'X12', 'X13', 'X23',
                            'RADI_V', 'ERR_RADI',))

EDFEQNO = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
                            components=(
                                'INVA_2', 'PRIN_[3]', 'INVA_2SG', 'VECT_1_X', 'VECT_1_Y',
                            'VECT_1_Z', 'VECT_2_X', 'VECT_2_Y', 'VECT_2_Z', 'VECT_3_X',
                            'VECT_3_Y', 'VECT_3_Z',))

EDOMGNO = LocatedComponents(phys=PHY.DOMA_R, type='ELNO',
                            components=('DOMA',))

EDURTNO = LocatedComponents(phys=PHY.DURT_R, type='ELNO',
                            components=('HV',))

EEINST_R = LocatedComponents(phys=PHY.INST_R, type='ELNO',
                             components=('INST',))

EENEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELNO',
                             components=('X[30]',))

EENEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                             components=('X[30]',))

EERRENOT = LocatedComponents(phys=PHY.ERRE_R, type='ELNO',
                             components=(
                                 'ERTABS', 'ERTREL', 'TERMNO', 'TERMVO', 'TERMV2',
                             'TERMV1', 'TERMSA', 'TERMS2', 'TERMS1', 'TERMFL',
                             'TERMF2', 'TERMF1', 'TERMEC', 'TERME2', 'TERME1',))

EHYDRNO = LocatedComponents(phys=PHY.HYDR_R, type='ELNO',
                            components=('HYDR',))

ENINST_R = LocatedComponents(phys=PHY.INST_R, type='ELNO',
                             components=('INST',))

ENNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELNO',
                             components=('X[30]',))

ENNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                             components=('X[30]',))

ENOMIMA = LocatedComponents(phys=PHY.SPMX_R, type='ELNO',
                            components=(
                                'VAL', 'NUCOU', 'NUSECT', 'NUFIBR', 'POSIC',
                            'POSIS',))

EPHASNO_ = LocatedComponents(phys=PHY.VARI_R, type='ELNO',
                             components=('VARI',))

EPRACNO = LocatedComponents(phys=PHY.PRAC_R, type='ELNO',
                            components=('PRES_R', 'PRES_I', 'DB',))

EPRMENO = LocatedComponents(phys=PHY.PRME_R, type='ELNO',
                            components=('DB',))

ESIMXNC = LocatedComponents(phys=PHY.SIEFMX_C, type='ELNO',
                            components=('SIXXMIN', 'SIXXMAX',))

ESIMXNO = LocatedComponents(phys=PHY.SIEFMX_R, type='ELNO',
                            components=('SIXXMIN', 'SIXXMAX',))

ESINGNO = LocatedComponents(phys=PHY.SING_R, type='ELNO',
                            components=('DEGRE', 'RAPPORT', 'TAILLE',))

ETRIANO = LocatedComponents(phys=PHY.ENDO_R, type='ELNO',
                            components=('TRIAX', 'SI_ENDO', 'COENDO', 'DOM_LEM',))

FISNO_I = LocatedComponents(phys=PHY.NEUT_I, type='ELNO',
                            components=('X1',))

N1NEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                             components=('X1',))

N2NEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                             components=('X[2]',))

N3NEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                             components=('X[3]',))

N5NEUTI = LocatedComponents(phys=PHY.N120_I, type='ELNO',
                            components=('X[5]',))

N6NEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                             components=('X[6]',))

N9NEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELNO',
                             components=('X[9]',))

NCONTNO = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY',))

NTEMPER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
                            components=('TEMP_MIL', 'TEMP_INF', 'TEMP_SUP',))

ZVARINO = LocatedComponents(phys=PHY.VARI_R, type='ELNO',
                            components=('VARI',))


# Modes locaux de type ELGA :
#----------------------------------
E1GNEUT = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
                            components=('X1',))

EDERAPG = LocatedComponents(phys=PHY.DERA_R, type='ELGA', location='RIGI',
                            components=(
                                'DCHA_V', 'DCHA_T', 'IND_DCHA', 'VAL_DCHA', 'X11',
                            'X22', 'X33', 'X12', 'X13', 'X23',
                            'RADI_V', 'ERR_RADI',))

EDOMGGA = LocatedComponents(phys=PHY.DOMA_R, type='ELGA', location='RIGI',
                            components=('DOMA',))

EEFGEGAC = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
                             components=('N', 'VY', 'VZ', 'MT', 'MFY',
                                         'MFZ',))

EEFGEGAR = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
                             components=('N', 'VY', 'VZ', 'MT', 'MFY',
                                         'MFZ',))

EGGEMA_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='MATER',
                             components=('X', 'Y', 'Z',))

EGINDLO = LocatedComponents(phys=PHY.INDL_R, type='ELGA', location='RIGI',
                            components=('INDICE', 'DIR[4]',))

EGINST_R = LocatedComponents(phys=PHY.INST_R, type='ELGA', location='RIGI',
                             components=('INST',))

EGNEUT1R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
                             components=('X1',))

EHYDRMA = LocatedComponents(phys=PHY.HYDR_R, type='ELGA', location='MATER',
                            components=('HYDR',))

EIMPEDR = LocatedComponents(phys=PHY.IMPE_R, type='ELGA', location='RIGI',
                            components=('IMPE',))

EONDEPR = LocatedComponents(phys=PHY.ONDE_R, type='ELGA', location='RIGI',
                            components=('PRES',))

EPDILPG = LocatedComponents(phys=PHY.PDIL_R, type='ELGA', location='RIGI',
                            components=('A1_LC2',))

EPRESGA = LocatedComponents(phys=PHY.PRES_R, type='ELGA', location='RIGI',
                            components=('PRES',))

ESOURCF = LocatedComponents(phys=PHY.SOUR_F, type='ELGA', location='RIGI',
                            components=('VNOR',))

ETEMPMA = LocatedComponents(phys=PHY.TEMP_R, type='ELGA', location='MATER',
                            components=('TEMP',))

ETEMPPG = LocatedComponents(phys=PHY.TEMP_R, type='ELGA', location='RIGI',
                            components=('TEMP',))

ETRIAPG = LocatedComponents(phys=PHY.ENDO_R, type='ELGA', location='RIGI',
                            components=('TRIAX', 'SI_ENDO', 'COENDO', 'DOM_LEM',))

EVARC_R = LocatedComponents(phys=PHY.VARC_R, type='ELGA', location='RIGI',
                            components=('TEMP', 'HYDR', 'SECH', 'IRRA', 'CORR',
                                        'PTOT', 'DIVU', 'NEUT[2]',))

EVITENC = LocatedComponents(phys=PHY.VNOR_C, type='ELGA', location='RIGI',
                            components=('VNOR',))

G27NEUTR = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
                             components=('X[27]',))

GGEOMER = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
                            components=('X', 'Y', 'Z',))

ZVARCNO = LocatedComponents(phys=PHY.VARI_R, type='ELNO',
                            components=('VARI',))

ZVARCPG = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='MATER',
                            components=('VARI',))


# store all LocatedComponents objects
MODES = objects_from_context(globals(), LocatedComponents)
