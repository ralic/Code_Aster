      SUBROUTINE RVINFO ( IFM, IOCC, I1, I2, C, SDCHEF )
      IMPLICIT   NONE
      INTEGER             IFM, IOCC, I1, I2
      CHARACTER*1         C
      CHARACTER*16        SDCHEF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF DEBUG DATE 26/04/2011 AUTEUR COURTOIS M.COURTOIS
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
C     ------------------------------------------------------------------
C     AFFICHAGE INFO SUR LE POST COURRANT
C     ------------------------------------------------------------------
C IN  SDCHEF : K : SD DES CHAMPS EFFECTIF
C IN  C      : K : INDICATEUR D' ERREUR
C IN  IOCC   : I : INDICE OCCURENCE
C IN  I1,I2  : I : REPERAGE CHAMPS
C     ------------------------------------------------------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM
C
      INTEGER       ADRVAL, VALI, ADRACC
      REAL*8        VALR
      CHARACTER*8   ACCES
      CHARACTER*24  NOMVAL, NOMACC
C
C=======================================================================
C
      CALL JEMARQ()
      NOMVAL = SDCHEF//'.VALACCE'
      NOMACC = SDCHEF//'.TYPACCE'
      IF ( C .EQ. 'R' ) THEN
         CALL JEVEUO (JEXNUM(NOMACC,IOCC),'L',ADRACC)
         CALL JEVEUO (JEXNUM(NOMVAL,IOCC),'L',ADRVAL)
         ACCES = ZK8(ADRACC + I1-1)
      ELSE
         CALL JEVEUO(NOMVAL,'L',ADRVAL)
         CALL JEVEUO(NOMACC,'L',ADRACC)
         ACCES = ZK8(ADRACC)
      ENDIF
C
      WRITE(IFM,*)
      WRITE(IFM,*)'--- POST_TRAITEMENT NUMERO : ',IOCC,
     +            ' - CHAMP NUMERO           : ',I2
      IF ( (ACCES(1:1).EQ.'O') .OR. (ACCES(1:1).EQ.'M') ) THEN
         VALI = ZI(ADRVAL + I1-1)
         IF ( ACCES(1:1) .EQ. 'O' ) THEN
            WRITE(IFM,*)' NUME_ORDRE           : ',VALI
         ELSE
            WRITE(IFM,*)' NUME_MODE            : ',VALI
         ENDIF
      ELSE IF ( (ACCES(1:1).EQ.'F') .OR. (ACCES(1:1).EQ.'I') ) THEN
         VALR = ZR(ADRVAL + I1-1)
         IF ( ACCES(1:1) .EQ. 'I' ) THEN
            WRITE(IFM,*)' INSTANT                : ',VALR
         ELSE
            WRITE(IFM,*)' FREQUENCE              : ',VALR
         ENDIF
      ELSE
      ENDIF
C
      IF ( C .EQ. 'E' ) WRITE(IFM,*)' CHAMP INEXISTANT '
C
      CALL JEDEMA()
      END
