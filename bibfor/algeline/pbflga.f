      SUBROUTINE PBFLGA(UMOY,HMOY,RMOY,LONG,CF0,FSVR,ICOQ,IMOD,NBM,
     &                  TCOEF,S1,S2,LAMBDA,KCALCU,CONDIT,GAMMA)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
C RESOLUTION DU PROBLEME FLUIDE INSTATIONNAIRE : CALCUL DE GAMMA(3)
C DANS LE CAS OU UMOY <> 0
C APPELANT : PBFLUI
C-----------------------------------------------------------------------
C  IN : UMOY   : VITESSE DE L'ECOULEMENT MOYEN
C  IN : HMOY   : JEU ANNULAIRE MOYEN
C  IN : RMOY   : RAYON MOYEN
C  IN : LONG   : LONGUEUR DU DOMAINE DE RECOUVREMENT DES DEUX COQUES
C  IN : CF0    : COEFFICIENT DE FROTTEMENT VISQUEUX
C  IN : FSVR   : OBJET .FSVR DU CONCEPT TYPE_FLUI_STRU
C  IN : ICOQ   : INDICE CARACTERISANT LA COQUE SUR LAQUELLE ON TRAVAILLE
C                ICOQ=1 COQUE INTERNE  ICOQ=2 COQUE EXTERNE
C  IN : IMOD   : INDICE DU MODE CONSIDERE
C  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
C  IN : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
C  IN : S1     : PARTIE REELLE     DE LA FREQUENCE COMPLEXE
C  IN : S2     : PARTIE IMAGINAIRE DE LA FREQUENCE COMPLEXE
C  IN : LAMBDA : VALEURS PROPRES DE L'OPERATEUR DIFFERENTIEL
C  IN : KCALCU : MATRICE RECTANGULAIRE A COEFFICIENTS CONSTANTS
C                PERMETTANT DE CALCULER UNE SOLUTION PARTICULIERE DU
C                PROBLEME FLUIDE INSTATIONNAIRE, LORSQUE UMOY <> 0
C OUT : CONDIT : COEFFICIENTS DE PRECONDITIONNEMENT
C OUT : GAMMA  : COEFFICIENTS DE LA COMBINAISON LINEAIRE DONNANT LA
C                SOLUTION GENERALE DU PROBLEME FLUIDE INSTATIONNAIRE
C                (DECOMPOSITION SUR UNE FAMILLE D'EXPONENTIELLES)
C                LORSQUE UMOY <> 0
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      REAL*8       UMOY,HMOY,RMOY,LONG,CF0,FSVR(7)
      INTEGER      ICOQ,IMOD,NBM
      REAL*8       TCOEF(10,NBM),S1,S2
      COMPLEX*16   LAMBDA(3),KCALCU(3,4)
      REAL*8       CONDIT(3)
      COMPLEX*16   GAMMA(3)
C
      REAL*8       DEFAXE
      REAL*8       LN
      COMPLEX*16   EI,R,T
C
      COMPLEX*16   PBFLKZ
C
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IMATA ,IRET ,ITAB ,J 
      REAL*8 CDE ,CDEP ,CDS ,CDSP ,REELI ,RHOF ,U 
      REAL*8 V ,W ,X 
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
      RHOF = FSVR(1)
      CDE  = FSVR(4)
      CDEP = FSVR(5)
      CDS  = FSVR(6)
      CDSP = FSVR(7)
C
      U = 1.D0+CDE
      V = 1.D0-CDS
      R = -1.D0*DCMPLX(S1/UMOY+(CF0/HMOY),S2/UMOY)
      W = -0.5D0*CDEP*UMOY*DEFAXE(ICOQ,IMOD,0.D0,LONG,NBM,TCOEF)
      X = 0.5D0*CDSP*UMOY*DEFAXE(ICOQ,IMOD,LONG,LONG,NBM,TCOEF)
C
      ITAB = 0
      IF (ICOQ.EQ.2) ITAB = 5
      LN = TCOEF(1+ITAB,IMOD)
      GAMMA(1) = PBFLKZ(2,0.D0,LONG,LN,KCALCU)/RMOY
      GAMMA(2) = -1.D0*U*PBFLKZ(1,0.D0,LONG,LN,KCALCU)
     &           - PBFLKZ(3,0.D0,LONG,LN,KCALCU)/(RHOF*UMOY) + DCMPLX(W)
      GAMMA(3) = -1.D0*V*PBFLKZ(1,LONG,LONG,LN,KCALCU)
     &           - PBFLKZ(3,LONG,LONG,LN,KCALCU)/(RHOF*UMOY) + DCMPLX(X)
      DO 10 I = 1,3
        REELI = DBLE(LAMBDA(I))
        IF (REELI.GT.0.D0) THEN
          CONDIT(I) = 1.D0
        ELSE
          CONDIT(I) = 0.D0
        ENDIF
  10  CONTINUE
C
      CALL WKVECT('&&PBFLGA.TEMP.MATA','V V C',3*3,IMATA)
      DO 20 J = 1,3
        EI = DCMPLX(EXP(-1.D0*CONDIT(J)*LAMBDA(J)*LONG))
        ZC(IMATA+3*(J-1)) = LAMBDA(J)*EI
        T = LAMBDA(J)*(R-LAMBDA(J))*RMOY*RMOY
        ZC(IMATA+3*(J-1)+1) = (T+DCMPLX(U))*EI
        EI = DCMPLX(EXP((1.D0-CONDIT(J))*LAMBDA(J)*LONG))
        ZC(IMATA+3*(J-1)+2) = (T+DCMPLX(V))*EI
  20  CONTINUE
C
      CALL LCSOLZ(ZC(IMATA),GAMMA,3,3,1,IRET)
      IF (IRET.NE.0) CALL U2MESS('F','ALGELINE3_17')
C
      CALL JEDETR('&&PBFLGA.TEMP.MATA')
      CALL JEDEMA()
      END
