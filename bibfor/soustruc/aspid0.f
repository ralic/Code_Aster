      SUBROUTINE ASPID0 ( UNITE, EPT1, DET1, D1, D2, EPT2, DET2, ZMAX,
     +                    H, ALPHA, JEU, EPC, DEC, XMAX, TYPMAI,
     +                    THETA, TYPELE,ITYPSO,DPENE,NIVMAG)
      IMPLICIT   NONE
      INTEGER             UNITE, ITYPSO, NIVMAG
      REAL*8              EPT1, DET1, D1, D2, EPT2, DET2, ZMAX, H, 
     +                    ALPHA, JEU, EPC, DEC, XMAX, THETA, DPENE
      CHARACTER*4         TYPELE
      CHARACTER*8         TYPMAI
      CHARACTER*80        NOM
      INTEGER             LNOM,MAJ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 26/09/2000   AUTEUR CIBHHBC B.CIREE 
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
      WRITE (UNITE,1022) 'typele =  MOT ' , TYPELE , POIVIR
      WRITE (UNITE,1020) 'typ_eque = MOT ' , 'SAINE' , POIVIR
      WRITE (UNITE,1015) 'nivmag = ' , NIVMAG , POIVIR
      WRITE (UNITE,1000)
     +'*                                                               '
C      WRITE (UNITE,1040)
C     +'opti donn'
C      WRITE (UNITE,1040)
C     +' ''/exterieurs/cibhhbc/ASPIC9/DEVELO/aspic.datg '';        '
      WRITE (UNITE,1040)
     +'opti donn '
      WRITE (UNITE,1040)
     + '''' //  NOM(1:LNOM) // 'aspic.datg''; '
C
 1000 FORMAT( A64 )
 1010 FORMAT( A9 , 1PE12.5 , A43 )
 1012 FORMAT( A9 , I1      , A54 )
 1015 FORMAT( A9 , I12     , A43 )
 1020 FORMAT( A14, A8      , A42 )
 1022 FORMAT( A14, A4      , A46 )
 1030 FORMAT( A10, A8      , A46 )
 1040 FORMAT( A )
C
      END
