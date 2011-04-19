      SUBROUTINE NMTAMA(FAMI,KPG,KSP,IMATE,INSTAM,INSTAP,MATM,MAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

      IMPLICIT NONE

      INTEGER KPG,KSP,IMATE
      CHARACTER*(*)  FAMI
      REAL*8  INSTAM,INSTAP,MATM(3),MAT(14)

C ----------------------------------------------------------------------
C TAHERI :  LECTURE DES CARACTERISTIQUES DU MATERIAU
C ----------------------------------------------------------------------
C IN   FAMI  FAMILLE DU POINT DE GAUSS
C IN  KPG    POINT DE GAUSS
C IN   KSP   SOUS-POINT DE GAUSS
C IN  IMATE  ADRESSE DU MATERIAU CODE
C IN  INSTAM INSTANT -
C IN  INSTAP INSTANT +
C OUT MATM   CARACTERISTIQUES (ELASTIQUES) EN T-
C OUT MAT    CARACTERISTIQUES (ELASTIQUES, PLASTIQUES, VISQUEUSES) EN T+
C             1 = TROISK            (ELASTICITE)
C             2 = DEUXMU            (ELASTICITE)
C             3 = ALPHA             (THERMIQUE)
C             4 = R_0               (ECROUISSAGE)
C             5 = ALPHA             (ECROUISSAGE)
C             6 = M                 (ECROUISSAGE)
C             7 = A                 (ECROUISSAGE)
C             8 = B                 (ECROUISSAGE)
C             9 = C1                (ECROUISSAGE)
C            10 = C_INF             (ECROUISSAGE)
C            11 = S                 (ECROUISSAGE)
C            12 = 1/N               (VISCOSITE)
C            13 = K/(DT)**1/N       (VISCOSITE)
C            14 = UN_SUR_M          (VISCOSITE)
C ----------------------------------------------------------------------

      LOGICAL     VISCO
      CHARACTER*8 NOM(14)
      INTEGER OK(14)
      REAL*8      E,NU

      DATA NOM / 'E','NU','ALPHA',
     &           'R_0','ALPHA','M','A','B','C1','C_INF','S',
     &           'N','UN_SUR_K','UN_SUR_M' /


C - LECTURE DES CARACTERISTIQUES ELASTIQUES DU MATERIAU (T- ET T+)

      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,
     &            2,NOM(1),MATM(1),OK(1),2)
      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,
     &            1,NOM(3),MATM(3),OK(3),0)
      IF (OK(3).NE.0) MATM(3) = 0.D0
      E       = MATM(1)
      NU      = MATM(2)
      MATM(1) = E/(1.D0-2.D0*NU)
      MATM(2) = E/(1.D0+NU)

      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,
     &            2,NOM(1),MAT(1),OK(1),2)
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,
     &            1,NOM(3),MAT(3),OK(3),0)
      IF (OK(3).NE.0) MAT(3) = 0.D0
      E      = MAT(1)
      NU     = MAT(2)
      MAT(1) = E/(1.D0-2.D0*NU)
      MAT(2) = E/(1.D0+NU)


C - LECTURE DES CARACTERISTIQUES D'ECROUISSAGE (T+)
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','TAHERI',0,' ',0.D0,
     &            8,NOM(4),MAT(4),OK(4),2)
      MAT(7) = MAT(7) * (2.D0/3.D0)**MAT(5)

C LECTURE DES CARACTERISTIQUES DE VISCOSITE (TEMPS +)
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','LEMAITRE',0,' ',0.D0,
     &            3,NOM(12),MAT(12),OK(12),0)
      VISCO = OK(12).EQ.0

      IF (VISCO) THEN
        IF (MAT(12).EQ.0.D0)
     &    CALL U2MESS('F','ALGORITH8_32')
        MAT(12) = 1.D0 / MAT(12)

        IF (MAT(13).EQ.0.D0)
     &    CALL U2MESS('F','ALGORITH8_33')
        MAT(13) = 1.D0 / MAT(13) / (INSTAP-INSTAM)**MAT(12)

        IF (OK(14).NE.0) MAT(14) = 0.D0

      ELSE
        MAT(12) = 1.D0
        MAT(13) = 0.D0
        MAT(14) = 1.D0
      END IF


      END
