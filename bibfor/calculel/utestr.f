      SUBROUTINE UTESTR(CHAM19,NONOEU,NOCMP,REFI,REFR,REFC,TYPRES,
     +                                           EPSI,CRIT,IFIC,SSIGNE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*19      CHAM19
      CHARACTER*17             NONOEU
      CHARACTER*8                     NOCMP
      REAL*8                                     REFR
      COMPLEX*16                                      REFC
      CHARACTER*1                                          TYPRES
      REAL*8                                          EPSI
      CHARACTER*(*)                                        CRIT, SSIGNE
      INTEGER                               REFI,               IFIC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
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
C     ENTREES:
C        CHAM19 : NOM DU CHAM_NO DONT ON DESIRE VERIFIER 1 COMPOSANTE
C        NONOEU : NOM DU NOEUD A TESTER
C        NOCMP  : NOM DU DDL A TESTER SUR LE NOEUD NONOEU
C        REFR   : VALEUR REELLE ATTENDUE SUR LE DDL DU NOEUD
C        REFC   : VALEUR COMPLEXE ATTENDUE SUR LE DDL DU NOEUD
C        CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
C        EPSI   : PRECISION ESPEREE
C        IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C     SORTIES:
C      LISTING ...
C ----------------------------------------------------------------------
C     FONCTIONS EXTERNES:
      LOGICAL    EXISDG
      INTEGER    NBEC
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16           ZK16
      CHARACTER*24                   ZK24
      CHARACTER*32                           ZK32
      CHARACTER*80                                   ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C     VARIABLES LOCALES:
      CHARACTER*8  NOGD
      CHARACTER*1  TYPE
      INTEGER      GD, IADG, VALI
      REAL*8       VALR
      COMPLEX*16   VALC
      CHARACTER*4  TESTOK
      CHARACTER*8  NOMMA
      CHARACTER*19 PRCHNO
      CHARACTER*24 NOLILI
      CHARACTER*1 K1BID
C
C
      CALL JEMARQ()
      TESTOK = 'NOOK'
C
      CALL DISMOI('F','NOM_GD',CHAM19,'CHAM_NO',IBID,NOGD,IERD)
C
      CALL JEVEUO(CHAM19//'.REFE','L',IAREFE)
      NOMMA = ZK24(IAREFE-1+1)(1:8)
      PRCHNO = ZK24(IAREFE-1+2)(1:19)
C
      CALL JELIRA(CHAM19//'.VALE','TYPE',IBID,TYPE)
      IF ( TYPE.NE.TYPRES ) THEN
         WRITE(IFIC,*) TESTOK,' LE CHAMP '//CHAM19// ' EST A ',
     +                  'VALEURS DE TYPE  "'//TYPE//'"  ET LA VALEUR DE'
     +                 , ' REFERENCE DE TYPE  "'//TYPRES//'".'
          GO TO 9999
      ELSEIF (TYPE.NE.'R' .AND. TYPE.NE.'C') THEN
          WRITE(IFIC,*) TESTOK,' LES CHAMPS DE  '//
     +                  'TYPE  "'//TYPE//'"   SONT INTERDITS'
          GO TO 9999
      END IF
      CALL JEVEUO(CHAM19//'.VALE','L',IAVALE)
C
      CALL JEVEUO(CHAM19//'.DESC','L',IADESC)
      GD  = ZI(IADESC-1+1)
      NUM = ZI(IADESC-1+2)
      NEC = NBEC(GD)
C
C     -- ON RECHERCHE LE NUMERO CORRESPONDANT A NOCMP:
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K1BID)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',IANCMP)
      ICMP = INDIK8(ZK8(IANCMP),NOCMP,1,NCMPMX)
      IF (ICMP.EQ.0) THEN
          WRITE (IFIC,*) TESTOK,' LE DDL :',NOCMP,
     +                   'N EXISTE PAS DANS LA GRANDEUR:',NOGD
          GO TO 9999
      END IF
C
C        -- RECUPERATION DU NUMERO DU NOEUD:
          CALL JENONU(JEXNOM(NOMMA//'.NOMNOE',NONOEU(1:8)),INO)
          IF (INO.EQ.0) THEN
              WRITE (IFIC,*) TESTOK,' ON NE TROUVE PAS LE NOEUD',
     +          ' : ',NONOEU(1:8)
              GO TO 9999
          END IF
C
C     --SI LE CHAMP EST A REPRESENTATION CONSTANTE:
C
      IF (NUM.LT.0) THEN
          NCMP = -NUM
C
C        -- ON COMPTE LES CMP PRESENTES SUR LE NOEUD AVANT ICMP: (+1)
          IDECAL = 0
          DO 2 IICMP = 1,ICMP
              IF (EXISDG(ZI(IADESC+2),IICMP))   IDECAL = IDECAL + 1
    2     CONTINUE
C
          IF (EXISDG(ZI(IADESC+2),ICMP)) THEN
              IF (TYPE .EQ. 'R' ) THEN
                  VALR = ZR(IAVALE-1+(INO-1)*NCMP+IDECAL)
              ELSEIF (TYPE .EQ. 'I' ) THEN
                  VALI = ZI(IAVALE-1+(INO-1)*NCMP+IDECAL)
              ELSEIF (TYPE .EQ. 'C' ) THEN
                  VALC = ZC(IAVALE-1+(INO-1)*NCMP+IDECAL)
              ENDIF
              CALL UTITES(NOCMP,'         '//NONOEU(1:8),TYPE,
     +             REFI,REFR,REFC,VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
          ELSE
              WRITE (IFIC,*) TESTOK,' ON NE TROUVE PAS LE DDL'
          END IF
      ELSE
C        --SI LE CHAMP EST DECRIT PAR 1 "PRNO":
C
          CALL JENUNO(JEXNUM(PRCHNO//'.LILI',1),NOLILI)
          CALL JELIRA(JEXNUM(PRCHNO//'.PRNO',1),'LONMAX',
     +                IBID,K1BID)
          IF (IBID.EQ.0) THEN
              WRITE (IFIC,*) TESTOK,' : 2'
              GO TO 9999
          END IF
          CALL JEVEUO(JEXNUM(PRCHNO//'.PRNO',1),'L',IAPRNO)
          CALL JEVEUO(PRCHNO//'.NUEQ','L',IANUEQ)
C
C        IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
C        NCMP : NOMBRE DE COMPOSANTES PRESENTES SUR LE NOEUD
C        IADG : DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD INO
          IVAL = ZI(IAPRNO-1+ (INO-1)* (NEC+2)+1)
          NCMP = ZI(IAPRNO-1+ (INO-1)* (NEC+2)+2)
          IADG = IAPRNO - 1 + (INO-1)* (NEC+2) + 3
          IF (NCMP.EQ.0) THEN
              WRITE (IFIC,*) TESTOK,' : 3'
              GO TO 9999
          END IF
C
C        -- ON COMPTE LES CMP PRESENTES SUR LE NOEUD AVANT ICMP:
          IDECAL = 0
          DO 1,IICMP = 1,ICMP
              IF (EXISDG(ZI(IADG),IICMP))   IDECAL = IDECAL + 1
    1     CONTINUE
C
          IF (EXISDG(ZI(IADG),ICMP)) THEN
              IF (TYPE .EQ. 'R' ) THEN
                  VALR = ZR(IAVALE-1+ZI(IANUEQ-1+IVAL-1+IDECAL))
              ELSEIF (TYPE .EQ. 'I' ) THEN
                  VALI = ZI(IAVALE-1+ZI(IANUEQ-1+IVAL-1+IDECAL))
              ELSEIF (TYPE .EQ. 'C' ) THEN
                  VALC = ZC(IAVALE-1+ZI(IANUEQ-1+IVAL-1+IDECAL))
              ENDIF
              CALL UTITES(NOCMP,'         '//NONOEU(1:8),TYPE,
     +             REFI,REFR,REFC,VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
          ELSE
              WRITE (IFIC,*) TESTOK,' ON NE TROUVE PAS LE DDL'
          END IF
      END IF
 9999 CONTINUE
      CALL JEDEMA()
      END
