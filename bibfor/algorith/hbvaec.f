      SUBROUTINE HBVAEC(GAMMA,NBMAT,MATERF,PARAME)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      INTEGER       NBMAT
      REAL*8        GAMMA,MATERF(NBMAT,2),PARAME(4)
C ======================================================================
C --- HOEK BROWN : CALCUL DES FONCTIONS DE LA VARIABLE D ECROUISSAGE ---
C --- PARAME : S*SIG_C**2, M*SIG_C, B, PHI -----------------------------
C ======================================================================
C IN  GAMMA  VALEUR DE LA VARIABLE D ECROUISSAGE -----------------------
C IN  NBMAT  NOMBRE DE DONNEES MATERIAU --------------------------------
C IN  MATERF DONNEES MATERIAU ------------------------------------------
C OUT PARAME PARAMETRES D ECROUISSAGE S*SIGC2, M*SIGC, B, PPHI ---------
C ======================================================================
      REAL*8       AUX2,AUX3,AUX4,AUX5
      REAL*8       GRUP,GRES,SEND,SRUP,MEND,MRUP,PPHI1
      REAL*8       BRES,AP,DP,CP,PPHI2,PPHI0
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU --------------------------------
C ======================================================================
      GRUP   = MATERF(1,2)
      GRES   = MATERF(2,2)
      MEND   = MATERF(5,2)
      MRUP   = MATERF(6,2)
      SEND   = MATERF(3,2)
      SRUP   = MATERF(4,2)
      BRES   = MATERF(10,2)
      AP     = MATERF(11,2)
      DP     = MATERF(12,2)
      CP     = MATERF(13,2)
      PPHI1  = MATERF(9,2)
      PPHI2  = MATERF(15,2)
      PPHI0  = MATERF(16,2)
C ======================================================================
      IF (GAMMA.LT.GRUP) THEN
         AUX2 = GAMMA*(SRUP-SEND)/GRUP + SEND
         AUX3 = GAMMA*(MRUP-MEND)/GRUP + MEND
         AUX4 = 0.D0
         AUX5 = (PPHI1-PPHI0)*GAMMA/GRUP + PPHI0
C ======================================================================
      ELSE IF (GAMMA.LT.GRES) THEN
         AUX2 = SRUP
         AUX3 = MRUP
         AUX4 = AP*GAMMA**2 + DP*GAMMA + CP
         AUX5 = GAMMA*(PPHI2-PPHI1)/(GRES-GRUP)+
     &                (PPHI1*GRES-PPHI2*GRUP)/(GRES-GRUP)
C ======================================================================
      ELSE
         AUX2 = SRUP
         AUX3 = MRUP
         AUX4 = BRES
         AUX5 = PPHI2
      ENDIF
C ======================================================================
C --- STOCKAGE ---------------------------------------------------------
C ======================================================================
      PARAME(1) = AUX2
      PARAME(2) = AUX3
      PARAME(3) = AUX4
      PARAME(4) = AUX5
C ======================================================================
      END
