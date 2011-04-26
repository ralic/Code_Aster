      SUBROUTINE DISMLI(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C     --     DISMOI(INTERF_DYNA)
C     ARGUMENTS:
C     ----------
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI
      CHARACTER*32  REPK
      CHARACTER*14 NOMOB
      CHARACTER*(*) REPKZ, NOMOBZ
C ----------------------------------------------------------------------
C    IN:
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE INTERF_DYNA (K14)
C    OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C
C ----------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C
C---------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------
C
C
C
C
      CALL JEMARQ()
      NOMOB = NOMOBZ
      REPK=' '

      IF  (QUESTI(1:10).EQ.'NOM_MAILLA') THEN
         CALL JEVEUO(NOMOB(1:8)//'.IDC_REFE','L',LLREF)
         REPK(1:8)=ZK24(LLREF)
      ELSE IF (QUESTI(1:12).EQ.'NOM_NUME_DDL') THEN
         CALL JEVEUO(NOMOB(1:8)//'.IDC_REFE','L',LLREF)
         REPK(1:19)=ZK24(LLREF+1)
      ELSE IF (QUESTI.EQ.'NOM_MODE_CYCL') THEN
         CALL JEVEUO(NOMOB(1:8)//'.IDC_REFE','L',LLREF)
         REPK(1:8)=ZK24(LLREF+2)
      ELSE IF (QUESTI(1:5).EQ.'NB_EC') THEN
         CALL JEVEUO(NOMOB(1:8)//'.IDC_DESC','L',LLDES)
         REPI=ZI(LLDES+1)
      ELSE IF (QUESTI(1:10).EQ.'NB_CMP_MAX') THEN
         CALL JEVEUO(NOMOB(1:8)//'.IDC_DESC','L',LLDES)
         REPI=ZI(LLDES+2)
      ELSE IF (QUESTI(1:6).EQ.'NUM_GD') THEN
         CALL JEVEUO(NOMOB(1:8)//'.IDC_DESC','L',LLDES)
         REPI=ZI(LLDES+3)
      ELSE
         IERD=1
      END IF
C
      REPKZ  = REPK
      CALL JEDEMA()
      END
