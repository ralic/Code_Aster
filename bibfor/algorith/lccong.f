      SUBROUTINE LCCONG(NR,ITMAX,TOLER,ITER,R,RINI,YD,DY,IRTET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     CONTROLE DE LA CONVERGENCE DU NEWTON LOCAL DE LETK
C                   - CONTROLE DU NOMBRE D ITERATIONS
C                   - CONTROLE DE LA PRECISION DE CONVERGENCEC
C     ----------------------------------------------------------------
C     IN  ITMAX  :  NB MAXI D ITERATIONS LOCALES
C         TOLER  :  TOLERANCE A CONVERGENCE
C         ITER   :  NUMERO ITERATION COURANTE
C         NR     :  DIMENSION R
C         R      :  RESIDU DU SYSTEME NL A L'ITERATION COURANTE
C         RINI   :  RESIDU DU SYSTEME NL A LA 1ERE ITERATION
C         YD     :  SOLUTION A DEBUT DU PAS DE TEMPS
C         DY     :  INCREMENT DE SOLUTION
C
C     OUT IRET = 0:  CONVERGENCE
C         IRET = 1:  ITERATION SUIVANTE
C         IRET = 2:  RE-INTEGRATION
C         IRET = 3:  REDECOUPAGE DU PAS DE TEMPS
C     ----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NR,ITMAX,ITER,IRTET,I
      REAL*8  TOLER,R(NR),R8PREM,E1,E2,E1INI,E2INI,ERRR(2),RINI(*)
      REAL*8  YD(*),DY(*),ERR
C     ----------------------------------------------------------------
C === ==================================================================
C --- CALCUL DE LA NORME DE RINI ET DE R(Y)
C === ==================================================================

      E1=0.D0
      E1INI=0.D0
      DO 101 I = 1,6
         E1 = MAX(E1, ABS(R(I)))
         E1INI = MAX(E1INI, ABS(RINI(I)))
 101  CONTINUE
C     R8PREM CAR R HOMOGENE A DES DEFORMATIONS
      ERRR(1)=E1
      IF (E1INI.GT.R8PREM()) THEN
         ERRR(1)=E1/E1INI
      ENDIF

      E2=0.D0
      E2INI=0.D0
      DO 102 I = 7,NR
         E2 = MAX(E2, ABS(R(I)))
         E2INI = MAX(E2INI, ABS(YD(I)+DY(I)))
 102  CONTINUE

      ERRR(2)=E2
      IF (E2INI.GT.R8PREM()) THEN
         ERRR(2)=E2/E2INI
      ENDIF

C     MAX DES 6 PREMIERS TERMES ET DES SUIVANTS
      ERR=MAX(ERRR(1),ERRR(2))

C === =================================================================
C --- TEST DE CONVERGENCE PAR RAPPORT A TOLER
C === =================================================================
      IF ( ERR .LT. TOLER ) THEN
         IRTET = 0
         GOTO 9999
      ENDIF

C === ==================================================================
C --- SI NON CONVERGENCE: TEST DU N°ITERATION
C === ==================================================================
      IF(ITER.LT.ITMAX)THEN
        IRTET = 1
      ELSE
        IRTET = 3
      ENDIF

 9999 CONTINUE

      END
