      SUBROUTINE CALCQ(S, GAMCJS, PREF, EPSSIG, Q, CODRET)
C
      IMPLICIT      NONE
      INTEGER       CODRET
      REAL*8        S(6), GAMCJS, PREF, EPSSIG, Q(6)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/10/2009   AUTEUR FERNANDES R.FERNANDES 
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
C --- BUT : CALCUL DE Q = DG/DSIG --------------------------------------
C ======================================================================
C IN  : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
C --- : S      : DEVIATEUR DES CONTRAINTES -----------------------------
C --- : GAMCJS : PARAMETRE DU MODELE -----------------------------------
C --- : PREF   : PRESSION ATMOSPHERIQUE --------------------------------
C --- : EPSSIG : EPSILON -----------------------------------------------
C OUT : Q      : DG/DSIG = 1/H(T)**5* ----------------------------------
C ------------ :    ((1+GAMCJS*RCOS3T/2)*S/SII + -----------------------
C ------------ :  + GAMCJS*RAC(54)/(6*SII**2)*DEV(D(DET(S))/D(S))) -----
C ======================================================================
      INTEGER II, NDT, NDI
      REAL*8  SII, T(6), DEVT(6), INVH5, FACT1, FACT2
      REAL*8  RHLODE, HLODE, COS3T, RCOS3T
      REAL*8  UN, DEUX, CINQ, SIX, DDOT, R8PREM
C ======================================================================
C --- INITIALISATION DE PARAMETRE --------------------------------------
C ======================================================================
      PARAMETER       (  UN     =  1.0D0  )
      PARAMETER       (  DEUX   =  2.0D0  )
      PARAMETER       (  CINQ   =  5.0D0  )
      PARAMETER       (  SIX    =  6.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
      CODRET = 0
C ======================================================================
C --- CALCUL DES VARIABLES UTILES --------------------------------------
C ======================================================================
      SII=DDOT(NDT,S,1,S,1)
      SII    = SQRT(SII)
      CALL     CJST(S,T)
      CALL     LCDEVI(T,DEVT)
      RCOS3T = COS3T(S, PREF, EPSSIG)
      RHLODE = HLODE(GAMCJS, RCOS3T)
      INVH5  = UN/(RHLODE**CINQ)
C ======================================================================
C --- VARIABLES INTERMEDIAIRES -----------------------------------------
C ======================================================================
      IF (SII.LT.R8PREM()) GOTO 100
      FACT1  = INVH5*(UN+GAMCJS*RCOS3T/DEUX)/SII
      FACT2  = INVH5*GAMCJS*SQRT(54.0D0)/(SIX*SII*SII)
C ======================================================================
C --- CALCUL FINAL -----------------------------------------------------
C ======================================================================
      DO 10 II=1,NDT
         Q(II) = FACT1*S(II)+FACT2*DEVT(II)
 10   CONTINUE
      GOTO 200
C ======================================================================
 100  CONTINUE
      CODRET = 2
 200  CONTINUE
      CALL JEDEMA ()
C ======================================================================
      END
