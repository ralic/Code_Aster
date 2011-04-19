      SUBROUTINE NMCHAM (FAMI,KPG,KSP,IMATE,COMPOR,
     &                   MATEL,MAT,NBVAR,MEMO,VISC,COEF)
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE PROIX J-M.PROIX
C.======================================================================
      IMPLICIT NONE
C
C      NMCHAM   -- COEFFICIENTS MATERIAU DES LOIS DE COMPORTEMENT
C                  'VMIS_CINx_CHAB'  'VISC_CINx_CHAB'
C                  'VMIS_CIN2_MEMO'  'VISC_CIN2_MEMO'
C
C     ARGUMENT  E/S  TYPE         ROLE
C     IMATE     IN    I    ADRESSE DU MATERIAU CODE
C     COMPOR    IN    K16  COMPOR(1) NOM DU COMPORTEMENT
C     MAT       OUT   R    COEF MATERIAU
C
C ---- ARGUMENTS
      INTEGER       IMATE,NBVAR,KPG,KSP,MEMO,VISC,IRET
      CHARACTER*16  COMPOR(3)
      REAL*8        MAT(16),MATEL(4)
      CHARACTER*(*) FAMI
C ---- VARIABLES LOCALES
      REAL*8      COEF,VALRES(10),C2INF,GAMM20
      REAL*8      R0,RINF,B,CINF,K,W,GAMMA0
      REAL*8      UN,AINF,KVI,VALDEN,UNSKVI
      INTEGER ICODRE(10)
      CHARACTER*8 NOMRES(10),NOMEMO(4)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      NBVAR=0
      IF ( COMPOR(1)(6:9) .EQ. 'CIN1' ) THEN
         NBVAR=1
      ELSEIF ( COMPOR(1)(6:9) .EQ. 'CIN2' ) THEN
         NBVAR=2
      ELSE
         CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      ENDIF
      IF ( COMPOR(1)(1:4) .EQ. 'VMIS' ) THEN
         VISC=0
      ELSEIF ( COMPOR(1)(1:4) .EQ. 'VISC' ) THEN
         VISC=1
      ELSE
         CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      ENDIF
      IF ( COMPOR(1)(11:14) .EQ. 'CHAB' ) THEN
         MEMO=0
      ELSEIF ( COMPOR(1)(11:14) .EQ. 'MEMO' ) THEN
         MEMO=1
      ELSE
         CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      ENDIF
C
C --- INITIALISATIONS :
C     ===============
      UN     = 1.0D0

      CALL VERIFT(FAMI,KPG,KSP,'T',IMATE,'ELAS',1,COEF,IRET)
C
C --- RECUPERATION DES CARACTERISTIQUES ELASTIQUES :
C     ============================================
      NOMRES(1)='E'
      NOMRES(2)='NU'
C
C ---  CARACTERISTIQUES A L'INSTANT PRECEDENT :
C      --------------------------------------
      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES(1),MATEL(1),ICODRE(1), 1)
C
C ---  CARACTERISTIQUES A L'INSTANT ACTUEL :
C      -----------------------------------

      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES(1),MATEL(3),ICODRE(1), 1)
C
C --- RECUPERATION DES CARACTERISTIQUES D'ECROUISSAGE :
C     ===============================================
      NOMRES(1) = 'R_0'
      NOMRES(2) = 'R_I'
      NOMRES(3) = 'B'
      NOMRES(5) = 'K'
      NOMRES(6) = 'W'
      NOMRES(8) = 'A_I'
      IF (NBVAR.EQ.1) THEN
         NOMRES(4) = 'C_I'
         NOMRES(7) = 'G_0'
         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','CIN1_CHAB',0,' ',0.D0,
     &            8,NOMRES,VALRES,ICODRE,1)
      ELSEIF (NBVAR.EQ.2) THEN
         NOMRES(4) = 'C1_I'
         NOMRES(7) = 'G1_0'
         NOMRES(9) = 'C2_I'
         NOMRES(10)= 'G2_0'

         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','CIN2_CHAB',0,' ',0.D0,
     &            10,NOMRES,VALRES,ICODRE,1)
      ENDIF
       R0     = VALRES(1)
       RINF   = VALRES(2)
       B      = VALRES(3)
       CINF   = VALRES(4)
       K      = VALRES(5)
       W      = VALRES(6)
       GAMMA0 = VALRES(7)
       AINF   = VALRES(8)
       C2INF = 0.D0
       GAMM20 = 0.D0
       IF (NBVAR.EQ.2) THEN
          C2INF  = VALRES(9)
          GAMM20 = VALRES(10)
       ENDIF
C
       MAT(1) = R0
       MAT(2) = RINF
       MAT(3) = B
       MAT(4) = CINF
       MAT(5) = K
       MAT(6) = W
       MAT(7) = GAMMA0
       MAT(8) = AINF
      IF (NBVAR.EQ.2) THEN
       MAT(9) = C2INF
       MAT(10) = GAMM20
      ELSE
       MAT(9) = 0.D0
       MAT(10) = 0.D0
      ENDIF

      IF (VISC.EQ.1) THEN
C
C ---    RECUPERATION DES CARACTERISTIQUES VISQUEUSES :
C        ============================================
         NOMRES(1) = 'N'
         NOMRES(2) = 'UN_SUR_K'
         NOMRES(3) = 'UN_SUR_M'
         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','LEMAITRE',0,' ',0.D0,3
     &                ,NOMRES,VALRES,ICODRE,1)
C
         IF (ICODRE(1).EQ.0) THEN
           VALDEN = VALRES(1)
           UNSKVI = VALRES(2)
           IF (VALDEN.LE.0.D0) THEN
             CALL U2MESS('F','ALGORITH6_67')
           ENDIF
           IF (UNSKVI.EQ.0.D0) THEN
             CALL U2MESS('F','ALGORITH6_68')
           ENDIF
           KVI = UN/UNSKVI
           IF (VALRES(3).NE.0.D0) THEN
             CALL U2MESS('F','ALGORITH6_69')
           ENDIF
         ELSE
           CALL U2MESS('F','COMPOR1_32')
         ENDIF
      ELSE
        VALDEN = UN
        KVI = 0.D0
      ENDIF
      MAT(11)=VALDEN
      MAT(12)=KVI
      MAT(13)=0.D0
      MAT(14)=0.D0
      MAT(15)=0.D0
      MAT(16)=0.D0

      IF (MEMO.EQ.1) THEN
C ---    RECUPERATION DES CARACTERISTIQUES MEMOIRE :
C        ============================================
         NOMEMO(1) = 'ETA     '
         NOMEMO(2) = 'Q_0     '
         NOMEMO(3) = 'Q_M     '
         NOMEMO(4) = 'MU      '
         CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ', 'MEMO_ECRO',0,' ',
     &              0.D0,4,NOMEMO,MAT(13),ICODRE, 1)

      ENDIF
      END
