      SUBROUTINE TE0153(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     CALCULE LES MATRICES ELEMENTAIRES DES ELEMENTS DE BARRE
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'RIGI_MECA'      : CALCUL DE LA MATRICE DE RAIDEUR
C        'MASS_MECA'      : CALCUL DE LA MATRICE DE MASSE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_BARRE' : ELEMENT BARRE
C        'MECA_2D_BARRE' : ELEMENT BARRE
C
C
      INTEGER CODRES
      CHARACTER*8  NOMAIL
      CHARACTER*16 CH16
      REAL*8       E, RHO, PGL(3,3), MAT(21),MATR(21)
      REAL*8       A, XL, XRIG, XMAS,MATP(6,6),MAT2DM(4,4),MAT2DV(10)
      INTEGER      IADZI,IAZK24
C     ------------------------------------------------------------------
C
CC     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
C-----------------------------------------------------------------------
      INTEGER I ,IACCE ,IMATE ,J ,LMAT ,LORIEN ,LSECT 
      INTEGER LVEC ,LX ,NC ,NNO 
      REAL*8 R8B 
C-----------------------------------------------------------------------
      CALL JEVECH ('PCAGNBA', 'L',LSECT)
      A  =  ZR(LSECT)
      NNO = 2
      NC  = 3
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER', 'L',LX)
      LX = LX - 1
      IF (NOMTE.EQ.'MECA_BARRE') THEN
        CALL LONELE( ZR(LX),3,XL)
C
      ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
        CALL LONELE(ZR(LX),2,XL)
C
      ENDIF
C
      IF( XL .EQ. 0.D0 ) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF
C
C     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
C
      IF (OPTION.EQ.'M_GAMMA')THEN
        CALL JEVECH ('PVECTUR', 'E', LVEC)
        CALL JEVECH('PACCELR','L',IACCE)
      ELSE
        CALL JEVECH ('PMATUUR', 'E', LMAT)
      ENDIF

      DO 20 I = 1,21
        MAT(I) = 0.D0
 20   CONTINUE
C
C     --- CALCUL DES MATRICES ELEMENTAIRES ----
      CALL JEVECH ('PMATERC', 'L', IMATE)
      IF ( OPTION.EQ.'RIGI_MECA'  ) THEN
         CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ','ELAS',0,' ',R8B,
     &               1,'E',E,CODRES,1)
         XRIG = E * A / XL
         MAT( 1) =  XRIG
         MAT( 7) = -XRIG
         MAT(10) =  XRIG
C
      ELSE IF ( OPTION.EQ.'MASS_MECA' .OR.
     &          OPTION.EQ.'M_GAMMA') THEN
         CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ','ELAS',0,' ',R8B,
     &               1,'RHO',RHO,CODRES,1)
         DO 40 I=1,21
             MATR(I) = 0.D0
 40      CONTINUE

         XMAS = RHO * A * XL / 6.D0
         MAT( 1) = XMAS * 2.D0
         MAT( 3) = XMAS * 2.D0
         MAT( 6) = XMAS * 2.D0
         MAT( 10) = XMAS * 2.D0
         MAT( 15) = XMAS * 2.D0
         MAT( 21) = XMAS * 2.D0

         MAT( 7)  = XMAS
         MAT( 12) = XMAS
         MAT( 18) = XMAS
C
      ELSE IF ( (OPTION.EQ.'MASS_MECA_DIAG') .OR.
     &          (OPTION.EQ.'MASS_MECA_EXPLI')) THEN
         CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ','ELAS',0,' ',R8B,
     &               1,'RHO',RHO,CODRES,1)
         XMAS = RHO * A * XL / 2.D0
         MAT( 1) = XMAS
         MAT( 3) = XMAS
         MAT( 6) = XMAS
         MAT(10) = XMAS
         MAT(15) = XMAS
         MAT(21) = XMAS
C
      ELSE
         CH16 = OPTION
         CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      ENDIF
C
C     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
C
      CALL MATROT ( ZR(LORIEN) , PGL )
      CALL UTPSLG ( NNO, NC, PGL, MAT, MATR )

      IF (OPTION.EQ.'M_GAMMA')THEN
       IF (NOMTE.EQ.'MECA_BARRE')THEN
         DO 45 I=1,6
           DO 46 J=1,6
            MATP(I,J)=0.D+0
  46       CONTINUE
  45     CONTINUE
         CALL VECMA(MATR,21,MATP,6)
         CALL PMAVEC('ZERO',6,MATP,ZR(IACCE),ZR(LVEC))
       ELSE
         MAT2DV(1)   = MATR(1)
         MAT2DV(2)   = MATR(2)
         MAT2DV(3)   = MATR(3)
         MAT2DV(4)   = MATR(7)
         MAT2DV(5)   = MATR(8)
         MAT2DV(6)   = MATR(10)
         MAT2DV(7)   = MATR(11)
         MAT2DV(8)   = MATR(12)
         MAT2DV(9)   = MATR(14)
         MAT2DV(10)  = MATR(15)
         DO 47 I=1,4
           DO 48 J=1,4
            MAT2DM(I,J)=0.D+0
  48       CONTINUE
  47     CONTINUE
         CALL VECMA(MAT2DV,10,MAT2DM,4)
         CALL PMAVEC('ZERO',4,MAT2DM,ZR(IACCE),ZR(LVEC))
       ENDIF
      ELSE
C
C ECRITURE DANS LE VECTEUR PMATTUR SUIVANT L'ELEMENT
C
        IF (NOMTE.EQ.'MECA_BARRE') THEN
            DO 30 I=1,21
              ZR(LMAT+I-1) = MATR(I)
 30         CONTINUE
        ELSE IF (NOMTE.EQ.'MECA_2D_BARRE') THEN
            ZR(LMAT)     = MATR(1)
            ZR(LMAT+1)   = MATR(2)
            ZR(LMAT+2)   = MATR(3)
            ZR(LMAT+3)   = MATR(7)
            ZR(LMAT+4)   = MATR(8)
            ZR(LMAT+5)   = MATR(10)
            ZR(LMAT+6)   = MATR(11)
            ZR(LMAT+7)   = MATR(12)
            ZR(LMAT+8)   = MATR(14)
            ZR(LMAT+9)   = MATR(15)
        ENDIF
      ENDIF




C
      END
