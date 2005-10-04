      SUBROUTINE EXISD(TYPESD,NOMSD,IRET)
      IMPLICIT NONE
      INTEGER IRET
      CHARACTER*(*) TYPESD,NOMSD
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/10/2005   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE                            VABHHTS J.PELLET
C A_UTIL
C ----------------------------------------------------------------------
C  BUT : DETERMINER SI UNE SD EXISTE
C  IN   TYPESD : TYPE DE LA STRUCTURE DE DONNEE A TESTER
C         / 'CARTE'        /'CHAM_NO'      /'CHAM_ELEM'   /'RESUELEM'
C         / 'CHAM_ELEM_S'  /'CHAM_NO_S'
C         / 'CHAMP' (CHAPEAU AUX CHAM_NO/CHAM_ELEM/CARTE/RESUELEM)
C         / 'CHAMP_GD' (CHAPEAU DESUET AUX CHAM_NO/CHAM_ELEM/...)
C         / 'TABLE'
C         / 'RESULTAT'
C         / 'FONCTION'
C         / 'MODELE' 
C         /'MAILLAGE'
C         /'NUME_DDL'
C         /'MATR_ASSE'
C       NOMSD   : NOM DE LA STRUCTURE DE DONNEES A TESTER

C  OUT:  IRET   : 0 -> LA SD N'EXISTE PAS
C                 1 -> LA SD EXISTE
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER I1,I2,I3,I4,I5,I6
      CHARACTER*8 CH8
      CHARACTER*16 TYP2SD
      CHARACTER*19 CH
C -DEB------------------------------------------------------------------

      CALL JEMARQ()
      TYP2SD = TYPESD


      IF (TYP2SD.EQ.'MAILLAGE') THEN
C     ------------------------------
        CH8 = NOMSD
        CALL JEEXIN(CH8//'.DIME',I1)
        CALL JEEXIN(CH8//'.NOMNOE',I2)
        IF (I1*I2.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'MODELE') THEN
C     ------------------------------
        CH8 = NOMSD
        CALL JEEXIN(CH8//'.MAILLE',I1)
        CALL JEEXIN(CH8//'.NOEUD',I2)
        CALL JEEXIN(CH8//'.MODELE    .LIEL',I3)
        IF (I1*I2*I3.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'CARTE') THEN
C     ------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.NOMA',I1)
        CALL JEEXIN(CH//'.DESC',I2)
        CALL JEEXIN(CH//'.VALE',I3)
        IF (I1*I2*I3.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'CHAM_NO') THEN
C     ------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.REFE',I1)
        CALL JEEXIN(CH//'.DESC',I2)
        CALL JEEXIN(CH//'.VALE',I3)
        IF (I1*I2*I3.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'CHAM_ELEM') THEN
C     ------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.CELD',I1)
        CALL JEEXIN(CH//'.CELV',I2)
        IF (I1*I2.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'RESUELEM') THEN
C     ------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.DESC',I1)
        CALL JEEXIN(CH//'.RESL',I2)
        CALL JEEXIN(CH//'.NOLI',I3)
        IF (I1*I2*I3.NE.0) GO TO 20


      ELSE IF ((TYP2SD.EQ.'CHAMP').OR.(TYP2SD.EQ.'CHAMP_GD')) THEN
C     -------------------------------------------------------------
        CH = NOMSD

C       -- CHAM_ELEM ?
        CALL JEEXIN(CH//'.CELD',I1)
        CALL JEEXIN(CH//'.CELV',I2)
        IF (I1*I2.NE.0) GO TO 20

C       -- CHAM_NO OU CARTE ?
        CALL JEEXIN(CH//'.DESC',I1)
        CALL JEEXIN(CH//'.VALE',I2)
        IF (I1*I2.NE.0) GO TO 20

C       -- RESUELEM ?
        CALL JEEXIN(CH//'.DESC',I1)
        CALL JEEXIN(CH//'.RESL',I2)
        CALL JEEXIN(CH//'.NOLI',I3)
        IF (I1*I2*I3.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'CHAM_NO_S') THEN
C     ------------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.CNSD',I1)
        CALL JEEXIN(CH//'.CNSV',I2)
        CALL JEEXIN(CH//'.CNSL',I3)
        IF (I1*I2*I3.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'CHAM_ELEM_S') THEN
C     --------------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.CESD',I1)
        CALL JEEXIN(CH//'.CESV',I2)
        CALL JEEXIN(CH//'.CESL',I3)
        IF (I1*I2*I3.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'TABLE') THEN
C     --------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.TBBA',I1)
        CALL JEEXIN(CH//'.TBNP',I2)
        CALL JEEXIN(CH//'.TBLP',I3)
        IF (I1*I2*I3.NE.0) GO TO 20


      ELSE IF (TYP2SD.EQ.'RESULTAT') THEN
C     -----------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.DESC',I1)
        CALL JEEXIN(CH//'.NOVA',I2)
        CALL JEEXIN(CH//'.TAVA',I3)
        CALL JEEXIN(CH//'.ORDR',I4)
        CALL JEEXIN(CH//'.TACH',I5)
        IF (I1*I2*I3*I4*I5.NE.0) GO TO 20

      ELSE IF (TYP2SD.EQ.'LIGREL') THEN
C     -----------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.LIEL',I1)
        CALL JEEXIN(CH//'.NOMA',I2)
        CALL JEEXIN(CH//'.NBNO',I3)
        IF (I1*I2*I3.NE.0) GO TO 20

      ELSE IF (TYP2SD.EQ.'FONCTION') THEN
C     -----------------------------------
        CH = NOMSD
        CALL JEEXIN(CH//'.PROL',I1)
        IF (I1.NE.0) GO TO 20

      ELSE IF (TYP2SD.EQ.'MATR_ASSE') THEN
C     -----------------------------------
        CH = NOMSD
C        CALL JEEXIN(CH//'.CONL',I1)
        CALL JEEXIN(CH//'.REFA',I2)
        CALL JEEXIN(CH//'.VALE',I3)
        IF (I1*I2*I3.NE.0) GO TO 20

      ELSE IF (TYP2SD.EQ.'NUME_DDL') THEN
C     -----------------------------------
        CH = NOMSD
        CALL JEEXIN(CH(1:14)//'.NUME.DEEQ',I1)
        CALL JEEXIN(CH(1:14)//'.NUME.DELG',I2)
        CALL JEEXIN(CH(1:14)//'.NUME.LILI',I3)
        CALL JEEXIN(CH(1:14)//'.NUME.NUEQ',I4)
        IF (I1*I2*I3*I4.NE.0) GO TO 20

      ELSE
        CALL UTMESS('F','EXISD',' LE MOT CLE :'//TYP2SD//
     &              'N EST PAS AUTORISE.')
      END IF

   10 CONTINUE
      IRET = 0
      GO TO 30

   20 CONTINUE
      IRET = 1

   30 CONTINUE
      CALL JEDEMA()
      END
