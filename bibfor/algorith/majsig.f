      SUBROUTINE MAJSIG( MATERF, SE, SEQ, I1E, ALPHA, DP, PLAS, SIG)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/03/2011   AUTEUR BOTTONI M.BOTTONI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C =====================================================================
      IMPLICIT      NONE
      REAL*8        MATERF(5,2),DP,SE(*),SEQ,I1E,ALPHA,SIG(6)
C =====================================================================
C --- MISE A JOUR DES CONTRAINTES -------------------------------------
C =====================================================================
      INTEGER     II, NDT, NDI
      REAL*8      YOUNG, NU, TROISK, DEUXMU, TROIS, DEUX, UN
      REAL*8      I1, DEV(6), PLAS
      PARAMETER ( TROIS  =  3.0D0 )
      PARAMETER ( DEUX   =  2.0D0 )
      PARAMETER ( UN     =  1.0D0 )
C =====================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
C --- AFFECTATION DES VARIABLES ---------------------------------------
C =====================================================================
      YOUNG  = MATERF(1,1)
      NU     = MATERF(2,1)
      TROISK = YOUNG / (UN-DEUX*NU)
      DEUXMU = YOUNG / (UN+NU)
C =====================================================================
C --- MISE A JOUR DU DEVIATEUR ----------------------------------------
C =====================================================================
      IF (PLAS.EQ.2.0D0) THEN
         DO 10 II = 1, NDT
            DEV(II) = 0.0D0
 10      CONTINUE
      ELSE
         DO 20 II = 1, NDT
            DEV(II) = SE(II)*(UN-TROIS*DEUXMU/DEUX*DP/SEQ)
 20      CONTINUE
      ENDIF

C =====================================================================
C --- MISE A JOUR DU PREMIER INVARIANT --------------------------------
C =====================================================================
      I1 = I1E - TROIS*TROISK*ALPHA*DP
C =====================================================================
C --- MISE A JOUR DU VECTEUR DE CONTRAINTES ---------------------------
C =====================================================================
      DO 30 II = 1, NDT
         SIG(II) = DEV(II)
 30   CONTINUE
      DO 40 II = 1, NDI
         SIG(II) = SIG(II) + I1/TROIS
 40   CONTINUE
C =====================================================================
      END
