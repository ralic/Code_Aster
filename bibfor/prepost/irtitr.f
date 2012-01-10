      SUBROUTINE IRTITR(CHAM,NOPASE,NOMA,FORM,IFI,TITRE)
      IMPLICIT NONE
      INTEGER           IFI
      CHARACTER*(*)     CHAM,NOPASE,NOMA,FORM
      CHARACTER*80      TITRE
C     ------------------------------------------------------------------
C MODIF PREPOST  DATE 09/01/2012   AUTEUR SELLENET N.SELLENET 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C     IMPRESSION D'UN TITRE
C     ------------------------------------------------------------------
C IN  CHAM   : K8  : NOM DU CONCEPT
C IN  NOMMA  : K8  : NOM DU MAILLAGE
C IN  FORM   : K8  : FORMAT D'ECRITURE
C IN  IFI    : IS  : UNITE LOGIQUE D'ECRITURE
C OUT TITRE  : K80 : TITRE
C     ------------------------------------------------------------------
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C     ------------------------------------------------------------------
      INTEGER       VERS,UTIL,NIVO,NBTITR,JTITR
      INTEGER       I, IER, IRET
C
      CHARACTER*1   K1BID
      CHARACTER*8   NOMMA
      CHARACTER*16  DATE,TYPRES
      CHARACTER*19  CHAM19
      CHARACTER*24  DATEUR
      CHARACTER*80  TITSUP(7),STITR
      LOGICAL       LEXP
C
      CALL JEMARQ()
      NOMMA=NOMA
      CHAM19 = CHAM
      TITRE = ' '
C
C     SOUS-TITRE POUR LES CAS DE SENSIBILITE
      IF ( NOPASE.NE.' ' ) THEN
        WRITE (STITR,'(1X,A,1X,A)')
     &                        '... SENSIBILITE AU PARAMETRE',NOPASE
      ENDIF
C
C     --- SI CHAM19 != ' ', ALORS IL S'AGIT DE L'IMPRESSION D'UN CHAMP
C         (RESULTAT OU CHAM_GD) ET NON D'UN MAILLAGE.
C         LE TITRE EST ALORS ECRIT DANS LE K80 TITRE
      IF (CHAM19.NE.' ') THEN
        CALL JEEXIN(CHAM19//'.TITR',IER)
        IF(IER.NE.0) THEN
          CALL JEVEUO(CHAM19//'.TITR','L',JTITR)
          CALL JELIRA(CHAM19//'.TITR','LONMAX',NBTITR,K1BID)
          TITRE=ZK80(JTITR)
          IF(FORM.EQ.'RESULTAT')  THEN
            WRITE(IFI,'(1X,A)') (ZK80(JTITR+I-1),I=1,NBTITR)
            IF ( NOPASE.NE.' ' ) THEN
              WRITE (IFI,'(A)') STITR
            ENDIF
          ENDIF
        ELSE
          CALL GETTCO(CHAM,TYPRES)
          WRITE (TITRE,'(1X,A,2X,A,2X,A,1X,A)')
     &                         'CONCEPT ',CHAM,'DE TYPE ',TYPRES
          IF(FORM.EQ.'RESULTAT')  THEN
            WRITE (IFI,'(A)') TITRE
            IF ( NOPASE.NE.' ' ) THEN
              WRITE (IFI,'(A)') STITR
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C     --- IMPRESSION AU FORMAT 'IDEAS': ECRITURE D'UN TITRE
      IF(FORM(1:5).EQ.'IDEAS') THEN
C
C        --- ECRITURE DU TITRE ---
         DO 1 I = 1,7
            TITSUP(I) = ' '
    1    CONTINUE
         IF(NOMMA.NE.' ') THEN
C          - L'IMPRESSION DU MAILLAGE A ETE DEMANDEE
           CALL JEEXIN(NOMMA//'           .TITR',IRET)
           IF (IRET.NE.0) THEN
             CALL JEVEUO(NOMMA//'           .TITR','L',JTITR)
             CALL JELIRA(NOMMA//'           .TITR','LONMAX',NBTITR,
     &                   K1BID)
             DO 2 I = 1,MIN(6,NBTITR)
               TITSUP(I+1) = ZK80(JTITR-1+I)
    2        CONTINUE
           ENDIF
         ENDIF
         CALL ENLIRD(DATEUR)
         CALL VERSIO(VERS,UTIL,NIVO,DATE,LEXP)
C                     12345678901234567890
         TITSUP(1) = ' ASTER V00.00.00 DU '
         WRITE (TITSUP(1) (9:16),'(I2,''.'',I2,''.'',I2)')
     &         VERS,UTIL,NIVO
         TITSUP(1) = TITSUP(1) (1:20)//DATE(1:10)//'  RESULTAT DU '
         TITSUP(1) (45:69) = DATEUR
         TITSUP(4) = ' '
C
         WRITE (IFI,'(A)') '    -1'
         WRITE (IFI,'(A)') '   151   %TITRE '
         DO 11 I = 1,7
           WRITE (IFI,'(A)') TITSUP(I)
   11    CONTINUE
         WRITE (IFI,'(A)') '    -1'
      ENDIF
C
      CALL JEDEMA()
      END
