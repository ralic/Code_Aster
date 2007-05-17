      SUBROUTINE GFGUID ( IT, Z, DZ, FPTG1, FFTG1, FLUID, GEOM1,
     &                    CFPCD1, POINTE, RUGOSI, X, Z0 )
      IMPLICIT NONE
      INTEGER  IT, POINTE(2)
      REAL*8   FPTG1, FFTG1, FLUID(8), GEOM1(15), CFPCD1(6),
     &         RUGOSI(8), Z, DZ, X(5), Z0
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2007   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     CALCUL DE LA FORCE FLUIDE EXERCEE SUR UN CRAYON EN CHUTE
C     DANS UN TUBE GUIDE EN FONCTION
C        DE SA PROFONDEUR D'ENFONCEMENT  Z  DANS LE TUBE GUIDE
C        DE SA VITESSE DE CHUTE  DZ
C
C-----------------------------------------------------------------------
      INTEGER    LWA,IND,IRET,EPS,EPS0,NMAX,I
      INTEGER    VALI(2)
      PARAMETER  (LWA=100)

      REAL*8  A, A0, A1, AC, ROC, CD1, CD2, DH, NUC, EPSLON, P0, P1,
     &        S, T, LAMEQ, LAMEQ1, C1, C2, D, DIFF, TOL, AA0, P2, CD0,
     &        KCF1, KCF2, CF1, CF2, DCRAY, DTG, LAM, HRUGC, HRUGTG,
     &        GFCORR, XX(3), WA(LWA), FVEC(3), F(5),
     &        PI, R8PI, R8PREM, ZERO, DEMI, UN
      REAL*8  VALR(7)
C     ------------------------------------------------------------------
C
      ROC = FLUID(1)
      NUC = FLUID(4)
      P0  = FLUID(5)
      P1  = FLUID(6)
      P2  = FLUID(7)
C
      DCRAY = GEOM1(1)
      DTG   = GEOM1(2)
      A     = GEOM1(4)
      A0    = GEOM1(5)
      A1    = GEOM1(6)
      AA0   = GEOM1(7)
      AC    = GEOM1(8)
      DH    = GEOM1(10)
C
      CD0 = CFPCD1(1)
      CD1 = CFPCD1(2)
      CD2 = CFPCD1(3)
C
      EPS  = POINTE(1)
      EPS0 = POINTE(2)
C
      HRUGC  = RUGOSI(1)
      HRUGTG = RUGOSI(3)
C
      EPSLON = 1.0D-10
      TOL  = SQRT(R8PREM())
      NMAX = 200
      ZERO = 0.0D0
      DEMI = 0.5D0
      UN   = 1.0D0
      PI   = R8PI()
C
C     X(1) = U  : VITESSE ANNULAIRE DU FLUIDE
C     X(2) = U1 : VITESSE DE SORTIE DU FLUIDE PAR DES ORIFICES
C     X(3) = U0 : VITESSE DE SORTIE DU FLUIDE PAR LE TROU DE LA VIS
C     X(4) = PC : PRESSION A L'EXTREMITE INFERIEURE DU CRAYON
C     X(5) = P  : PRESSION DU FLUIDE A L'ALTITUDE Z
C
C----------------------------------------------------------------------
C     CALCUL DU REGIME D'ECOULEMENT DANS L'ESPACE ANNULAIRE POUR LE
C     CALCUL DU COEFFICIENT DE PERTES DE CHARGE SINGULIERE LAM
C----------------------------------------------------------------------
C
      CALL GFCFRV ( (X(1)+DZ)*DH/NUC,  HRUGC/DH, CF1 )
      CALL GFCFRV (      X(1)*DH/NUC, HRUGTG/DH, CF2 )
      KCF1 = GFCORR ( (X(1)+DZ)*DH/NUC )
      KCF2 = GFCORR (      X(1)*DH/NUC )
C
      IND = 0
 11   CONTINUE
      IND  =  IND + 1
      IF ( X(1) .EQ. ZERO ) THEN
         LAMEQ  =  ZERO
      ELSE
         LAMEQ  =  4*( KCF2*CF2/(UN+DCRAY/DTG) +
     &                 KCF1*CF1/(UN+DTG/DCRAY)*(UN+DZ/X(1))**2)
      ENDIF
C
C----------------------------------------------------------------------
C      TEST SUR LA CONFIGURATION D'ECOULEMENT (ENTRANTE OU SORTANTE)
C                    POUR LE CALCUL DU EPSLON
C----------------------------------------------------------------------
C
      T = ( UN + CD2 + LAMEQ*(Z+Z0)/DH ) * (AC/A)**2 * DZ**2
C
      IF ( T .LE. (2*(P1-P2)/ROC) ) THEN
         EPS = -1
      ELSE
         EPS = +1
      ENDIF
C
C----------------------------------------------------------------------
C                CALCUL ANALYTIQUE DE LA VITESSE ANNULAIRE U
C----------------------------------------------------------------------
C
      C1 = UN + CD2 + LAMEQ*(Z+Z0)/DH
      C2 = UN + EPS*CD1
      D = 2*C1*(P1-P2)/ROC + C2*(A/A1)**2*( (AC/A)**2*C1*DZ**2 -
     &                                                  2*(P1-P2)/ROC )
C
C     VERIFICATION QUE LE JEU DE DONNEE CONDUIT A UNE SOLUTION POUR LE
C     MODELE DE FORCE FLUIDE DANS LES TUBES GUIDES D'ASSEMBLAGES
C     COMBUSTIBLES
C
      X(1) = (-A*AC/A1**2*DZ*C2 + SQRT(D))/(C1-C2*(A/A1)**2)
C
      CALL GFCFRV ( (X(1)+DZ)*DH/NUC,  HRUGC/DH, CF1 )
      CALL GFCFRV (      X(1)*DH/NUC, HRUGTG/DH, CF2 )
      KCF1 = GFCORR( (X(1)+DZ)*DH/NUC )
      KCF2 = GFCORR(      X(1)*DH/NUC )
C
      LAMEQ1 = 4*( KCF2*CF2/(UN+DCRAY/DTG) +
     &             KCF1*CF1/(UN+DTG/DCRAY)*(UN+DZ/X(1))**2 )
C
      IF ( (ABS(LAMEQ-LAMEQ1).GT.EPSLON) .AND. (IND.LT.NMAX) ) THEN
         GOTO 11
      ELSE
         LAMEQ = LAMEQ1
         IF ( IND .GE. NMAX ) THEN
            CALL U2MESG('A','GRAPPEFLUIDE_7',0,' ',0,0,1,Z)
         ENDIF
      ENDIF
C
C----------------------------------------------------------------------
C                CALCUL DES AUTRES INCONNUES DU SYSTEME
C----------------------------------------------------------------------
C
      X(2) = (AC*DZ-A*X(1))/A1/EPS
C
      X(4) = P1 + DEMI*ROC*X(2)**2*(UN+EPS*CD1) - DEMI*ROC*DZ**2
C
      X(5) = P2 + DEMI*ROC*X(1)**2*(LAMEQ*(Z+Z0)/DH+CD2)
C
C----------------------------------------------------------------------
C        CALCUL DE LA VITESSE DU FLUIDE TRAVERSANT LA VIS EPAULEE
C----------------------------------------------------------------------
C
      DIFF = X(4) + DEMI*ROC*DZ**2 - P0
C
      IF ( DIFF .GE. ZERO ) THEN
         EPS0 = +1
      ELSE
         EPS0 = -1
      ENDIF
C
      X(3) = SQRT( ABS( 2*( X(4) + DEMI*ROC*DZ**2-P0 )/ROC/((A0/AA0)**2
     &                           + EPS0*CD0 ) ) )
C
C----------------------------------------------------------------------
C                      CALCUL DU SYSTEME COMPLET
C----------------------------------------------------------------------
C
      IRET = 0
      XX(1) = X(1)
      XX(2) = X(2)
      XX(3) = X(3)
C
      CALL GFRESG ( X, FVEC, LAMEQ, FLUID, GEOM1, CFPCD1,
     &                                           EPS, EPS0, Z0, Z, DZ )
C
      CALL HYBRD2 ( 3, XX, FVEC, TOL, WA, LWA, IRET ,LAMEQ, FLUID,
     &               GEOM1, CFPCD1, EPS, EPS0, Z0, Z, DZ )
      IF ( IRET .NE. 1) THEN
         VALI (1) = IRET
         VALI (2) = IT
         CALL U2MESG('A', 'GRAPPEFLUIDE_4',0,' ',2,VALI,1,Z)
      ENDIF
C
      X(1) = XX(1)
      X(2) = XX(2)
      X(3) = XX(3)
      X(4) = P1 + DEMI*ROC*X(2)**2*(UN+EPS*CD1) - DEMI*ROC*DZ**2
      X(5) = P2 + DEMI*ROC*X(1)**2*(LAMEQ*(Z+Z0)/DH+CD2)
C
C--------------------------------------------------------------------
C      CALCUL DE L'ERREUR RESIDUELLE DANS LA RESOLUTION DU SYSTEME
C--------------------------------------------------------------------
C
      F(1) = AC*DZ - A*X(1) - A1*EPS*X(2) - A0*EPS0*X(3)
C
      F(2) = X(4) + DEMI*ROC*DZ**2 - P0 - DEMI*ROC*((A0/AA0)**2
     &                                   + EPS0*CD0)*X(3)**2
C
      F(3) = X(4) + DEMI*ROC*DZ**2 - X(5) - DEMI*ROC*X(1)**2
C
      F(4) = X(4) + DEMI*ROC*DZ**2 - P1
     &                              - DEMI*ROC*X(2)**2*(1.D0+EPS*CD1)
C
      F(5) = X(5) - P2 - DEMI*ROC*X(1)**2*(LAMEQ*(Z+Z0)/DH+CD2)
C
      S = 0.D0
      DO 10 I = 1 , 5
         S = S + ABS(F(I))
 10   CONTINUE
C
      IF ( S .GT. 1.0D-3 )  THEN
         VALR (1) = S
         VALR (2) = F(1)
         VALR (3) = F(2)
         VALR (4) = F(3)
         VALR (5) = F(4)
         VALR (6) = F(5)
         VALR (6) = Z
         CALL U2MESG('A', 'GRAPPEFLUIDE_6',0,' ',0,0,7,VALR)
      ENDIF
C
C----------------------------------------------------------------------
C          CALCUL DE LA FORCE FLUIDE APPLIQUEE SUR UN CRAYON
C----------------------------------------------------------------------
C
      CALL GFCFRV ( (X(1)+DZ)*DH/NUC, HRUGC/DH, CF1 )
      KCF1 = GFCORR( (X(1)+DZ)*DH/NUC )
      LAM  = 4*KCF1*CF1
C
      FPTG1  =  ( X(5) + 2*X(4) - 3*P2 ) / 3*AC
      FFTG1  =  LAM*ROC*(Z+Z0)*SQRT(PI*AC)/4*(X(1)+DZ)**2
C
      POINTE(1) = EPS
      POINTE(2) = EPS0
C
      END
