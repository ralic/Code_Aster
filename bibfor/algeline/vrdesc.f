      SUBROUTINE VRDESC ( OBJET1, OBJET2, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       OBJET1, OBJET2
      INTEGER                             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     VERIFICATION QUE DEUX OBJETS ONT MEME DOMAINE DE DEFINITION
C         ==> COMPARAISON DES "DESC"
C     ------------------------------------------------------------------
C IN  : OBJET1  : NOM DU 1-ER OBJET
C IN  : OBJET2  : NOM DU 2-ND OBJET
C OUT : IER     : IS   : CODE RETOUR
C                = 0 PAS D'ERREUR
C                > 0 NOMBRE DE DESCRIPTEURS DIFFERENTS
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
C
      INTEGER      NBVAL, IVAL1, IVAL2
      CHARACTER*8   CBID
      CHARACTER*19  NOM1, NOM2
      CHARACTER*24  DESC1, DESC2
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER   = 0
      NOM1  = OBJET1
      NOM2  = OBJET2
      DESC1 = NOM1//'.DESC'

      CALL JEEXIN(DESC1,IBID)
      IF (IBID.GT.0) THEN
        DESC1 = NOM1//'.DESC'
        DESC2 = NOM2//'.DESC'
      ELSE
        DESC1 = NOM1//'.CELD'
        DESC2 = NOM2//'.CELD'
      END IF

C
C     --- RECUPERATION DES LONGUEURS DES TABLEAUX DE REFERENCE ---
      CALL JELIRA(DESC1,'LONMAX',IVAL1,CBID)
      CALL JELIRA(DESC2,'LONMAX',IVAL2,CBID)
      IF ( IVAL1 .NE. IVAL2 ) THEN
         IER   = IER + ABS(IVAL1-IVAL2)
         NBVAL = MIN(IVAL1,IVAL2)
      ELSE
         NBVAL = IVAL1
      ENDIF
C
C     --- RECUPERATION DES TABLEAUX D'INFORMATIONS DE REFERENCE ---
      CALL JEVEUO(DESC1,'L',IDESC1)
      CALL JEVEUO(DESC2,'L',IDESC2)
C
C     --- CONTROLE DES REFERENCES ---
      DO 10 IVAL=0,NBVAL-1
         IF (ZI(IDESC1+IVAL).NE.ZI(IDESC2+IVAL)) IER = IER + 1
  10  CONTINUE
C
C     --- LIBERATION (AVEC I/O EN DIFFERE) ---
C
      CALL JEDEMA()
      END
