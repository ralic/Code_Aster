        SUBROUTINE LCMMCV (YD, DY,  DDY,    NR,    ITMAX, TOLER, ITER,
     &               R,RINI,EPSTR, IRTETI)
C RESPONSABLE JMBHH01 J.M.PROIX
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/05/2011   AUTEUR PROIX J-M.PROIX 
C TOLE CRS_1404
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
C       ----------------------------------------------------------------
C       MONOCRISTAL        : CONTROLE DE LA CONVERGENCE
C                                  DE LA CONFORMITE DE LA SOLUTION DP
C                                  ET DE LA RE-INTEGRATION
C                                  ET DU REDECOUPAGE DU PAS DE TEMPS
C                                  SUR LA NORME DU RESIDU
C       ----------------------------------------------------------------
C       IN
C            DY     :  VECTEUR SOLUTION DY = ( DSIG DVINT)
C            DDY    :  VECTEUR CORRECTION SUR LA SOLUTION
C            NR     :  DIMENSION DY DDY
C            ITMAX  :  NB MAXI D ITERATIONS LOCALES
C            TOLER  :  TOLERANCE A CONVERGENCE
C            ITER   :  NUMERO ITERATION COURANTE
C            R      :  R(Y) RESIDU A L'ITERATION COURANTE
C            RINI   :  R(Y0) RESIDU A L'ITERATION 1
C       OUT  IRTETI  :  =0 CONVERGENCE
C                       =1 ITERATIONS SUPPLEMENTAIRE (ITER<ITMAX)
C                       =3 ITMAX ATTEINT REDECOUPAGE LOCAL
C                       =4 ITMAX ATTEINT  REDECOUPAGE GLOBAL
C       ----------------------------------------------------------------
        INTEGER         ITMAX,  ITER,  NR,IRTETI,NDT,NDI,I
        REAL*8     TOLER,YD(NR),DDY(NR), DY(NR), R(NR),RINI(NR),EPSTR(6)
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
        REAL*8          ERRDY(NR), ERRR(NR),E1,E2,E1INI,E2INI,R8PREM
C       ----------------------------------------------------------------
C
C -   EVALUATION  DE L'ERREUR  EN RESIDU (DEFORMATIONS)

C     TYP  : TYPE D'ERREUR A CALCULER
C          0 =  MAX | DDY /DY |     < EPS
C          1 = || DDY || / || DY || < EPS non si termes d'ordres /=
C          2 = || DDYi / DYi ||     < EPS
C      CALL LCVERR ( RINI, R, 6, 1, ERRR  )

      E1=0.D0
      E2=0.D0
      E1INI=0.D0
      E2INI=0.D0
      DO 101 I = 1,6
         E1 = MAX(E1, ABS(R(I)))
C         E1INI = MAX(E1INI, ABS(RINI(I)))
         E1INI = MAX(E1INI, ABS(EPSTR(I)))
 101  CONTINUE

      ERRR(1)=E1
      IF (E1INI.GT.R8PREM()) THEN
         ERRR(1)=E1/E1INI
      ENDIF

      DO 102 I = 7,NR
         E2 = MAX(E2, ABS(R(I)))
C         E2INI = MAX(E2INI, ABS(RINI(I)))
         E2INI = MAX(E2INI, ABS(YD(I)+DY(I)))
 102  CONTINUE

      ERRR(2)=E2
      IF (E2INI.GT.R8PREM()) THEN
         ERRR(2)=E2/E2INI
      ENDIF

C     MAX DES 6 PREMIERS TERMES ET DES SUIVANTS
      ERRR(1)=MAX(ERRR(1),ERRR(2))

C     ERREUR SUR LA SOLUTION

      ERRDY(1)=0.D0
      ERRDY(3)=0.D0
      DO 103 I = 1,6
         ERRDY(1) = MAX(ERRDY(1), ABS(DDY(I)))
         ERRDY(3) = MAX(ERRDY(3), ABS(DY(I)))
  103 CONTINUE
      IF (ERRDY(3).GT.R8PREM()) THEN
          ERRDY(1)=ERRDY(1)/ERRDY(3)
      ENDIF
      ERRDY(2)=0.D0
      ERRDY(4)=0.D0
      DO 104 I = NDT+1,NR
         ERRDY(2) = MAX(ERRDY(2), ABS(DDY(I)))
         ERRDY(4) = MAX(ERRDY(3), ABS(DY(I)))
  104 CONTINUE
      IF (ERRDY(4).GT.R8PREM()) THEN
          ERRDY(2)=ERRDY(2)/ERRDY(4)
      ENDIF

C     MAX DES 6 PREMIERS TERMES ET DES SUIVANTS
      ERRDY(1)=MAX(ERRDY(1),ERRDY(2))



      IF ( ITER .LE. ITMAX ) THEN
C -             CONVERGENCE
          IF ( ERRR(1) .LE. TOLER ) THEN
             IRTETI = 0
             GOTO 9999
          ENDIF
          IF ( ERRDY(1) .LE. TOLER ) THEN
             IRTETI = 0
             GOTO 9999
          ENDIF

C -     NON CONVERGENCE ITERATION SUIVANTE
          IRTETI = 1
          GOTO 9999
      ELSE
C -     NB ITERATION MAXIMUM ATTEINT SANS CONVERGENCE
         IRTETI=3
      ENDIF
C
 9999 CONTINUE
      END
