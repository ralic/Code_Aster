      SUBROUTINE ASPID1 ( UNITE, EPT1, DET1, D1, D2, EPT2, DET2, ZMAX,
     +                    H, ALPHA, JEU, EPC, DEC, XMAX, TYPMAI,
     +                    THETA, A, C, EPS, RC0, NS, NC, NT,
     +                    POSI , NDT, FETIRF, FETIRP, TFISS, ZETA,
     +                    ITYPSO, DPENE, NIVMAG )
      IMPLICIT   NONE
      INTEGER             UNITE, NS, NC, NT, NDT, ITYPSO, NIVMAG
      REAL*8              EPT1, DET1, D1, D2, EPT2, DET2, ZMAX, H,
     +                    ALPHA, JEU, EPC, DEC, XMAX, THETA, A, C,
     +                    EPS, RC0, FETIRF, FETIRP, ZETA, DPENE
      CHARACTER*8         TYPMAI,POSI,TFISS
      CHARACTER*80        NOM
      INTEGER             LNOM,MAJ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 21/10/2002   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE  CRP_21
C     MACR_ASPIC_CALC
C
C     ECRIT DANS UN FICHIER  LES DONNEES UTILISATEUR
C
C     ------------------------------------------------------------------
      CHARACTER*43   POIVIR
C
      POIVIR = ' ;                                         '
      MAJ = 1
      CALL REPDEX(MAJ,LNOM,NOM) 
C
      WRITE (UNITE,1000)
     +'****************************************************************'
      WRITE (UNITE,1000)
     +'opti echo 0 ;                                                   '
      WRITE (UNITE,1010) 'epT1   = ' , EPT1  , POIVIR
      WRITE (UNITE,1010) 'DeT1   = ' , DET1  , POIVIR
      WRITE (UNITE,1010) 'd1     = ' , D1    , POIVIR
      WRITE (UNITE,1010) 'd2     = ' , D2    , POIVIR
      WRITE (UNITE,1010) 'epT2   = ' , EPT2  , POIVIR
      WRITE (UNITE,1010) 'DeT2   = ' , DET2  , POIVIR
      WRITE (UNITE,1010) 'Zmax   = ' , ZMAX  , POIVIR
      WRITE (UNITE,1012) 'type_s = ' , ITYPSO , POIVIR
      WRITE (UNITE,1010) 'd_pene = ' , DPENE , POIVIR
      WRITE (UNITE,1010) 'h      = ' , H     , POIVIR
      WRITE (UNITE,1010) 'angl_s = ' , ALPHA , POIVIR
      WRITE (UNITE,1010) 'jeu    = ' , JEU   , POIVIR
      WRITE (UNITE,1010) 'epC    = ' , EPC   , POIVIR
      WRITE (UNITE,1010) 'DeC    = ' , DEC   , POIVIR
      WRITE (UNITE,1010) 'Xmax   = ' , XMAX  , POIVIR
      WRITE (UNITE,1020) 'typmai =  MOT ' , TYPMAI , POIVIR
      WRITE (UNITE,1010) 'theta  = ' , THETA , POIVIR
      WRITE (UNITE,1010) 'a      = ' , A     , POIVIR
      WRITE (UNITE,1010) 'c      = ' , C     , POIVIR
      WRITE (UNITE,1010) 'zeta   = ' , ZETA  , POIVIR
      WRITE (UNITE,1010) 'eps    = ' , EPS   , POIVIR
      WRITE (UNITE,1010) 'rc0    = ' , RC0   , POIVIR
      WRITE (UNITE,1015) 'ns     = ' , NS    , POIVIR
      WRITE (UNITE,1015) 'nc     = ' , NC    , POIVIR
      WRITE (UNITE,1015) 'nt     = ' , NT    , POIVIR
      WRITE (UNITE,1020) 'dir_fiss = MOT ' , POSI  , POIVIR
      WRITE (UNITE,1020) 'pos_fiss = MOT ' , TFISS , POIVIR
      WRITE (UNITE,1015) 'ndt    = ' , NDT , POIVIR
      WRITE (UNITE,1010) 'f_etir_f = ' , FETIRF , POIVIR
      WRITE (UNITE,1010) 'f_etir_p = ' , FETIRP , POIVIR
      WRITE (UNITE,1020) 'typ_eque = MOT ' , 'FISS_LON' , POIVIR
      WRITE (UNITE,1015) 'nivmag = ' , NIVMAG , POIVIR
      WRITE (UNITE,1000)
     +'*                                                               '
      WRITE (UNITE,1040)
     +'opti donn'
C
C      WRITE (UNITE,1040)
C     +' ''/home06/f1bhhaj/uaster/ASCOUF1/aspic_v2.datg'';'
C
      WRITE (UNITE,1040)
     + '''' //  NOM(1:LNOM) // 'aspic_v2.datg''; '
C
 1000 FORMAT( A64 )
 1010 FORMAT( A24 , 1PE12.5 , A28 )
 1012 FORMAT( A9 , I1      , A54 )
 1015 FORMAT( A24 ,   I12   , A28 )
 1020 FORMAT( A24, A8      , A32 )
 1022 FORMAT( A24, A4      , A36 )
 1030 FORMAT( A10, A8      , A46 )
 1035 FORMAT( A10, A1      , A53 )
 1040 FORMAT( A )
C
      END
