      SUBROUTINE CALCN(NDT, NDI, S, B, VECN)
C
      IMPLICIT      NONE
      INTEGER       NDT, NDI
      REAL*8        B, S(*), VECN(*)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/03/2002   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DE N ------------------------------------------------
C ======================================================================
C IN  : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
C --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
C --- : S      : DEVIATEUR DES CONTRAINTES -----------------------------
C --- : B      : PARAMETRE DU CALCUL DE LA NORMALE ---------------------
C OUT : VECN   : N = (B*S/SII+I)/SQRT(B**2+3) --------------------------
C ======================================================================
      INTEGER II
      REAL*8  SII, RACINE, UN, TROIS
C ======================================================================
C --- INITIALISATION DE PARAMETRE --------------------------------------
C ======================================================================
      PARAMETER       ( UN      =   1.0D0  )
      PARAMETER       ( TROIS   =   3.0D0  )
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      CALL     PSCAL(NDT, S, S, SII)
      SII    = SQRT(SII)
C ======================================================================
C --- CALCUL DE N ------------------------------------------------------
C ======================================================================
      RACINE = SQRT(B*B + TROIS)
      DO 10 II=1,NDT
         VECN(II) = B*S(II)/(SII*RACINE)
 10   CONTINUE
      DO 20 II=1,NDI
         VECN(II) = VECN(II) + UN / RACINE
 20   CONTINUE
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
