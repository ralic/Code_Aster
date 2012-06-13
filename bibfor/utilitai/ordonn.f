      SUBROUTINE ORDONN(NOMFON,IRET)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19 NOMFON
      INTEGER      IRET
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------
C RESPONSABLE MCOURTOI M.COURTOIS
C ----------------------------------------------------------------------
C     1. DECLENCHE UNE ERREUR <F> SI LES ABSCISSES NE SONT PAS MONOTONES
C        AU SENS LARGE, APPEL DE FOVERF POUR CELA
C     2. ORDONNE LES POINTS PAR ABSCISSES CROISSANTES,
C        SI ELLES SONT DECROISSANTES
C ----------------------------------------------------------------------
C IN     : NOMFON : FONCTION A VERIFIER
C             (LES VALEURS SONT EN IN/OUT)
C IN     : IRET   : SI 1, ON S'ARRETE EN <F> SI ABSC. NON MONOTONES
C                   SI 0, ON FORCE LE TRI
C ----------------------------------------------------------------------
      INTEGER      IVAL,LPROL
      INTEGER      IPAR,NBPARA
      INTEGER      NBVAL,NBPTS,IER,I
      LOGICAL      ISNAP,INV
      CHARACTER*1  CODMES
      CHARACTER*16 TYPFON
      CHARACTER*24 CHVAL,CHBID
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CHVAL=NOMFON//'.VALE'
      CALL JEVEUO(NOMFON//'.PROL','L',LPROL)
      TYPFON = ZK24(LPROL)(1:16)
C
      ISNAP = .FALSE.
      INV = .FALSE.
      CODMES = ' '
C     --------------------------------------------
      IF(TYPFON.EQ.'CONSTANT')THEN
C        ON N'A RIEN A FAIRE
C     --------------------------------------------
      ELSEIF(TYPFON.EQ.'INTERPRE')THEN
C        ON N'A RIEN A FAIRE
C     --------------------------------------------
      ELSEIF(TYPFON.EQ.'FONCTION')THEN
         CALL JEVEUO(CHVAL,'E',IVAL)
         CALL JELIRA(CHVAL,'LONUTI',NBVAL,CHBID)
         NBPTS=NBVAL/2
         IF(NBPTS.EQ.1) GOTO 999
         IER=0
         CALL FOVERF(ZR(IVAL),NBPTS,IER)
         IF(IER.EQ.0)THEN
            IF(IRET.EQ.1)THEN
               CODMES = 'F'
               GOTO 999
            ELSE
               CODMES = 'A'
               CALL UTTRIF(ZR(IVAL),NBPTS,TYPFON)
            ENDIF
         ELSEIF(IER.EQ.-2)THEN
            CODMES = 'A'
            INV = .TRUE.
            CALL ORDON1(ZR(IVAL),NBPTS)
         ELSEIF(IER.EQ.1 .OR. IER.EQ.-1) THEN
            CODMES = 'F'
            GOTO 999
         ENDIF
C     --------------------------------------------
      ELSEIF(TYPFON.EQ.'FONCT_C')THEN
         CALL JEVEUO(CHVAL,'E',IVAL)
         CALL JELIRA(CHVAL,'LONUTI',NBVAL,CHBID)
         NBPTS=NBVAL/3
         IF(NBPTS.EQ.1) GOTO 999
         IER=0
         CALL FOVERF(ZR(IVAL),NBPTS,IER)
         IF(IER.EQ.0)THEN
            IF(IRET.EQ.1)THEN
               CODMES = 'F'
               GOTO 999
            ELSE
               CODMES = 'A'
               CALL UTTRIF(ZR(IVAL),NBPTS,TYPFON)
            ENDIF
         ELSEIF(IER.EQ.-2)THEN
            CODMES = 'A'
            INV = .TRUE.
            CALL ORDON2(ZR(IVAL),NBPTS)
         ELSEIF(IER.EQ.1 .OR. IER.EQ.-1) THEN
            CODMES = 'F'
            GOTO 999
         ENDIF
C     --------------------------------------------
      ELSEIF(TYPFON.EQ.'NAPPE')THEN
         ISNAP=.TRUE.
         CALL JEVEUO(NOMFON//'.PARA','E',IPAR)
         CALL JELIRA(NOMFON//'.PARA','LONUTI',NBPARA,CHBID)
         IER=0
         CALL FOVERF(ZR(IPAR),NBPARA,IER)
         IF(IER.LE.0)THEN
C           LES PARAMETRES NE SONT PAS CROISSANTS (SENS LARGE)
            CALL ORDONP(NOMFON)
            CALL U2MESS('A','UTILITAI3_37')
         ENDIF
C        VERIFIER CHAQUE FONCTION COMME CI-DESSUS
         TYPFON='FONCTION'
         DO 100 I=1,NBPARA
            CALL JELIRA(JEXNUM(CHVAL,I),'LONMAX',NBVAL,CHBID)
            CALL JEVEUO(JEXNUM(CHVAL,I),'E',IVAL)
            NBPTS=NBVAL/2
            IF(NBPTS.EQ.1) GOTO 99
            IER=0
            CALL FOVERF(ZR(IVAL),NBPTS,IER)
            IF(IER.EQ.0)THEN
               IF(IRET.EQ.1)THEN
                  CODMES = 'F'
                  GOTO 999
               ELSE
                  CODMES = 'A'
                  CALL UTTRIF(ZR(IVAL),NBPTS,TYPFON)
               ENDIF
            ELSEIF(IER.EQ.-2)THEN
               CODMES = 'A'
               INV = .TRUE.
               CALL ORDON1(ZR(IVAL),NBPTS)
            ELSEIF(IER.EQ.1 .OR. IER.EQ.-1) THEN
               CODMES = 'F'
               GOTO 999
            ENDIF
  99        CONTINUE
 100     CONTINUE
C     --------------------------------------------
      ELSE
       CALL U2MESS('F','UTILITAI3_38')
      ENDIF
C
 999  CONTINUE
C
      IF (.NOT. ISNAP) THEN
         CALL U2MESK(CODMES//'+', 'FONCT0_62', 1, NOMFON)
      ELSE
         CALL U2MESK(CODMES//'+', 'FONCT0_63', 1, NOMFON)
      ENDIF
      IF ( CODMES .EQ. 'F' ) THEN
         CALL U2MESS(CODMES, 'FONCT0_64')
      ELSEIF ( CODMES .EQ. 'A' .AND. .NOT. INV ) THEN
         CALL U2MESS(CODMES, 'FONCT0_65')
      ELSEIF ( CODMES .EQ. 'A' .AND. INV ) THEN
         CALL U2MESS(CODMES, 'FONCT0_66')
      ENDIF
      CALL JEDEMA()
      END
