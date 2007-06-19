      SUBROUTINE OP0134 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/06/2007   AUTEUR PELLET J.PELLET 
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
C     CALCUL D'UNE FONCTION INTERPRETEE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IFM, NIV, N1, NF, NN, NBVALP, NBVALF, LVALP,
     &             LVALF, LNOVA, NBNOVA, LPROL
      REAL*8       RVAL
      LOGICAL      COMPL
      CHARACTER*8  K8B
      CHARACTER*16 NOMCMD, TYPRES, NOPARP, NOPARF
      CHARACTER*19 NOMFON, NOMFIN, LISTP, LISTF, TYPCO
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IER = 0
      CALL INFMAJ
      CALL INFNIV ( IFM, NIV )
C
      CALL GETRES ( NOMFON, TYPRES, NOMCMD )
C
      CALL GETVID ( ' ', 'FONCTION', 1,1,1, NOMFIN, N1 )
      CALL GETTCO ( NOMFIN, TYPCO )
C
C --- LISTE DES VALEURS DU PARAMETRE
C
      CALL GETVR8 ( ' ', 'VALE_PARA', 1,1,0, RVAL, N1 )
      IF ( N1 .NE. 0 ) THEN
         NBVALP  = -N1
         CALL WKVECT ( '&&OP0134.VALP', 'V V R', NBVALP, LVALP )
         CALL GETVR8 ( ' ', 'VALE_PARA', 1,1,NBVALP, ZR(LVALP), N1 )
      ELSE
         CALL GETVID ( ' ', 'LIST_PARA', 1,1,1, LISTP, N1 )
         CALL JEVEUO ( LISTP//'.VALE', 'L', LVALP)
         CALL JELIRA ( LISTP//'.VALE', 'LONUTI', NBVALP, K8B )
      ENDIF
C
C --- NAPPE OU FONCTION
C
      COMPL = .FALSE.
      IF (TYPCO(1:7).EQ.'FORMULE') THEN
         IF (TYPCO(1:9).EQ.'FORMULE_C')  COMPL = .TRUE.
         CALL JELIRA ( NOMFIN//'.NOVA', 'LONUTI', NBNOVA, K8B )
         CALL JEVEUO ( NOMFIN//'.NOVA', 'L', LNOVA )
         IF ( NBNOVA .EQ. 1 ) THEN
            NOPARP = ZK8(LNOVA)
         ELSEIF ( NBNOVA .EQ. 2 ) THEN
            NOPARP = ZK8(LNOVA)
            NOPARF = ZK8(LNOVA+1)
         ENDIF
C
      ELSEIF (TYPCO(1:8).EQ.'FONCTION') THEN
         IF (TYPCO(1:10).EQ.'FONCTION_C')  COMPL = .TRUE.
         NBNOVA = 1
         CALL JEVEUO ( NOMFIN//'.PROL', 'L', LPROL )
         NOPARP = ZK16(LPROL+2)
C
      ELSEIF (TYPCO(1:5).EQ.'NAPPE') THEN
         NBNOVA = 2
         CALL JEVEUO ( NOMFIN//'.PROL', 'L', LPROL )
         NOPARP = ZK16(LPROL+2)
         NOPARF = ZK16(LPROL+5)
C
      ELSEIF (TYPCO(1:10).EQ.'PARA_SENSI') THEN
         NBNOVA = 1
         CALL JEVEUO ( NOMFIN//'.PROL', 'L', LPROL )
         NOPARP = ZK16(LPROL+2)
         NOPARF = ZK16(LPROL+5)
C
      ENDIF
C
C
      IF ( NBNOVA .EQ. 1 ) THEN
C ------------------------------------------------------------------
C                 FONCTION
C ------------------------------------------------------------------
         CALL CALCFO ( COMPL, NOMFIN, NOMFON, NBVALP, ZR(LVALP) ,NOPARP)
C
      ELSEIF ( NBNOVA .EQ. 2 ) THEN
C ------------------------------------------------------------------
C                 NAPPE
C ------------------------------------------------------------------
         CALL GETVR8 ( ' ', 'VALE_PARA_FONC', 1,1,0, RVAL, N1 )
         IF ( N1 .NE. 0 ) THEN
            NBVALF  = -N1
            CALL WKVECT ( '&&OP0134.VALF', 'V V R', NBVALF, LVALF )
            CALL GETVR8 ( ' ', 'VALE_PARA_FONC', 1,1,NBVALF,
     &                                               ZR(LVALF), N1 )
         ELSE
            CALL GETVID ( ' ', 'LIST_PARA_FONC', 1,1,1, LISTF, N1 )
            CALL JEVEUO ( LISTF//'.VALE', 'L', LVALF)
            CALL JELIRA ( LISTF//'.VALE', 'LONUTI', NBVALF, K8B )
         ENDIF
C
         CALL  CALCNA ( NOMFIN, NOMFON, NBVALP, ZR(LVALP), NOPARP,
     &                  NBVALF, ZR(LVALF), NOPARF )
C
      ELSE
C
         CALL U2MESS('F','UTILITAI2_82')
C
      ENDIF
C
C --- SURCHARGE EVENTUELLE DU .PROL
C
      CALL FOATTR( ' ', 1, NOMFON )
C
C --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
C     ET REMISE DES ABSCISSES EN ORDRE CROISSANT
C
      CALL ORDONN ( NOMFON, NOMCMD, 0 )
C
      CALL TITRE
      IF (NIV.GT.1) CALL FOIMPR (NOMFON,NIV,IFM,0,LISTP)
C
      CALL JEDEMA()
      END
