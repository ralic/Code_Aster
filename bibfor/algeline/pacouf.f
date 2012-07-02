      SUBROUTINE PACOUF(X,FVECT,VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,
     +                  VECR4,VECR5,VECI1,VGAP,INDIC,NBM,NMODE)
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
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      CHARACTER*8   TYPFLU
      REAL*8        XSI0
      REAL*8        AMOR(*),VGAP,FVECT(2),X(2),MASG(*),MI
      REAL*8        VECR1(*),VECR2(*),VECR3(*),VECR4(*),VECR5(*)
      INTEGER       VECI1(*)
C
      COMPLEX*16    FONCT,Z,XKF
      LOGICAL       ZRIGI
      REAL*8        DEPI,R8DEPI
C
C ----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
      INTEGER INDIC ,NBM ,NMODE 
      REAL*8 PULSC ,XCF ,XCS ,XKS ,XMF ,XMS 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      DEPI = R8DEPI()
C
      PULSC = SQRT(X(1)*X(1)+X(2)*X(2))
      MI = MASG(NMODE)
      XSI0 = AMOR(NMODE) / (2.0D0*DEPI*MI*AMOR(NBM+NMODE))
      XCS = AMOR(NMODE)
      XKS = DEPI*DEPI*AMOR(NBM+NMODE)*AMOR(NBM+NMODE)*MI
      XMS = MASG(NMODE)
      ZRIGI = .TRUE.
C     ZRIGI=TRUE SIGNIFIE QU'ON CALCULE LES TERMES DE RAIDEUR ET
C     D'AMORTISSEMENT
      XMF = 0.D0
      XKF = DCMPLX(0.D0,0.D0)
      XCF = 0.D0
C
C --- CALCUL DES COEFFICIENTS DE MASSE, RAIDEUR ET AMORTISSEMENT ---
C --- AJOUTE                                                     ---
C
      CALL COEFMO(TYPFLU,ZRIGI,NBM,NMODE,INDIC,X,PULSC,VGAP,XSI0,
     &            VECI1,VECR1,VECR2,VECR3,VECR4,VECR5,
     &            XMF,XKF,XCF)
C
C --- CALCUL DE LA FONCTION ---
C
      Z = DCMPLX(X(1),X(2))
      FONCT = (XMS+XMF)*Z*Z + (XCS+XCF)*Z + (XKS+XKF)
      FVECT(1) = DBLE(FONCT)
      FVECT(2) = DIMAG(FONCT)
C
      CALL JEDEMA()
      END
