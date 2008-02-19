      SUBROUTINE ORDONN(NOMFON,NOMCMD,IRET)
      IMPLICIT NONE
      CHARACTER*19 NOMFON
      CHARACTER*16 NOMCMD
      INTEGER      IRET
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C IN     : NOMCMD : COMMANDE APPELANTE (POUR MESSAGE D'ERREUR)
C IN     : IRET   : SI 1, ON S'ARRETE EN <F> SI ABSC. NON MONOTONES
C                   SI 0, ON FORCE LE TRI
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       ZI
      COMMON/IVARJE/ZI(1)
      REAL*8        ZR
      COMMON/RVARJE/ZR(1)
      COMPLEX*16    ZC
      COMMON/CVARJE/ZC(1)
      LOGICAL       ZL
      COMMON/LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16         ZK16
      CHARACTER*24                 ZK24
      CHARACTER*32                         ZK32
      CHARACTER*80                                 ZK80
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
      INTEGER      IVAL,LPROL,IMES
      INTEGER      IN1,NBF,IPAR,NBPARA
      INTEGER      NBVAL,NBPTS,NS2,IER,I,INV
      REAL*8       XT
      CHARACTER*16 TYPFON
      CHARACTER*24 CHVAL,CHBID
      CHARACTER*24 VALK(3)
      CHARACTER*32 JEXNUM,MESS
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IMES=0
      CHVAL=NOMFON//'.VALE'
      CALL JEVEUO(NOMFON//'.PROL','L',LPROL)
      TYPFON = ZK24(LPROL)
C
C     --------------------------------------------
      IF(TYPFON.EQ.'CONSTANT')THEN
C        ON N'A RIEN A FAIRE
C     --------------------------------------------
      ELSEIF(TYPFON.EQ.'INTERPRE')THEN
C        ON N'A RIEN A FAIRE
C     --------------------------------------------
      ELSEIF(TYPFON.EQ.'FONCTION')THEN
         MESS='DE LA FONCTION'
         CALL JEVEUO(CHVAL,'E',IVAL)
         CALL JELIRA(CHVAL,'LONUTI',NBVAL,CHBID)
         NBPTS=NBVAL/2
         IF(NBPTS.EQ.1) GOTO 999
         IER=0
         CALL FOVERF(ZR(IVAL),NBPTS,IER)
         IF(IER.EQ.0)THEN
            IF(IRET.EQ.1)THEN
               IMES=4
               GOTO 999
            ELSE
               IMES=2
               CALL UTTRIF(ZR(IVAL),NBPTS,TYPFON)
            ENDIF
         ELSEIF(IER.LT.0)THEN
            IMES=1
            CALL ORDON1(ZR(IVAL),NBPTS)
         ENDIF
C     --------------------------------------------
      ELSEIF(TYPFON.EQ.'FONCT_C')THEN
         MESS='DE LA FONCTION'
         CALL JEVEUO(CHVAL,'E',IVAL)
         CALL JELIRA(CHVAL,'LONUTI',NBVAL,CHBID)
         NBPTS=NBVAL/3
         IF(NBPTS.EQ.1) GOTO 999
         IER=0
         CALL FOVERF(ZR(IVAL),NBPTS,IER)
         IF(IER.EQ.0)THEN
            IF(IRET.EQ.1)THEN
               IMES=4
               GOTO 999
            ELSE
               IMES=2
               CALL UTTRIF(ZR(IVAL),NBPTS,TYPFON)
            ENDIF
         ELSEIF(IER.LT.0)THEN
            IMES=1
            CALL ORDON2(ZR(IVAL),NBPTS)
         ENDIF
C     --------------------------------------------
      ELSEIF(TYPFON.EQ.'NAPPE')THEN
         MESS='D UNE FONCTION DE LA NAPPE'
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
                  IMES=4
                  GOTO 999
               ELSE
                  IMES=2
                  CALL UTTRIF(ZR(IVAL),NBPTS,TYPFON)
               ENDIF
            ELSEIF(IER.LT.0)THEN
               IMES=1
               CALL ORDON1(ZR(IVAL),NBPTS)
            ENDIF
  99        CONTINUE
 100     CONTINUE
C     --------------------------------------------
      ELSE
       CALL U2MESS('F','UTILITAI3_38')
      ENDIF
C
 999  CONTINUE
      IF(IMES.EQ.4)THEN
         VALK (1) = MESS
         VALK (2) = NOMFON
         VALK (3) = CHBID
         CALL U2MESG('F', 'UTILITAI6_57',3,VALK,0,0,0,0.D0)
      ELSEIF(IMES.EQ.2)THEN
         VALK (1) = MESS
         VALK (2) = NOMFON
         VALK (3) = CHBID
         CALL U2MESG('A', 'UTILITAI6_58',2,VALK,0,0,0,0.D0)
      ELSEIF(IMES.EQ.1)THEN
         VALK (1) = MESS
         VALK (2) = NOMFON
         VALK (3) = CHBID
         CALL U2MESG('A', 'UTILITAI6_59',2,VALK,0,0,0,0.D0)
      ENDIF
      CALL JEDEMA()
      END
