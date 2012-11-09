      SUBROUTINE CTNOTB (NBNO,MESNOE,NOMA,NBVAL,NKCHA,NKCMP,TOUCMP,
     &                   NBCMP,TYPAC,NDIM,NRVAL,RESU,NOMTB,NSYMB,
     &                   NIVAL,NIORD)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER      NBCMP,NBNO,NDIM,NBVAL
      CHARACTER*8  TYPAC,NOMA,RESU,NOMTB
      CHARACTER*16 NSYMB
      CHARACTER*24 NKCHA,NKCMP,MESNOE,NIVAL,NRVAL,NIORD
      LOGICAL      TOUCMP
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
C
C        BUT : REMPLISSAGE DE LA TABLE POUR UN CHAM_NO
C
C        IN     : NKCHA (K24)  : OBJET DES NOMS DE CHAMP
C                 RESU  (K8)   : NOM DU RESULTAT (SI RESULTAT,SINON ' ')
C                 NKCMP  (K24) : OBJET DES NOMS DE COMPOSANTES
C                 TOUCMP (L)   : INDIQUE SI TOUT_CMP EST RENSEIGNE
C                 NBCMP (I)    : NOMBRE DE COMPOSANTES LORSQUE
C                                NOM_CMP EST RENSEIGNE, 0 SINON
C                 TYPAC (K8)   : ACCES (ORDRE,MODE,FREQ,INST)
C                 NBVAL (I)    : NOMBRE DE VALEURS D'ACCES
C                 NOMA   (K8)  : NOM DU MAILLAGE
C                 MESNOE (K24) : OBJET DES NOMS DE NOEUD
C                 NRVAL (K16)  : OBJET DES VALEURS D'ACCES (REELS)
C                 NIVAL (K16)  : OBJET DES VALEURS D'ACCES (ENTIERS)
C                 NIORD (K16)  : NOM D'OBJET DES NUMEROS D'ORDRE
C                 NSYMB (K16)  : NOM SYMBOLIQUE DU CHAMP
C                 NBNO  (I)    : NOMBRE DE NOEUDS UTILISATEURS
C
C        IN/OUT : NOMTB (K24)  : OBJET TABLE
C
C ----------------------------------------------------------------------

      INTEGER JCMP,JKCHA,JLNO,JRVAL,JIVAL,JNIORD,JCOOR,I,JCNSV,NBNOX
      INTEGER JCNSL,JCNSD,JCNSC,NBCMPX,N,JVAL,JKVAL,INO,INDNO,INDIIS
      INTEGER KCP,ICMP,INDCMP,INDIK8,NI,NR,NK,JI,JR,JK,KK,NBPARA
      INTEGER JPARAK,J,IBID
      COMPLEX*16 CBID
      CHARACTER*8 KNO
      CHARACTER*19 CHAMNS
C     ------------------------------------------------------------------

      CALL JEMARQ()
C
C --- 0. INITIALISATIONS
      CHAMNS = '&&CTNOTB.CNS       '
      CALL JEVEUO(NKCMP,'L',JCMP)
      CALL JEVEUO(NKCHA,'L',JKCHA)
      CALL JEVEUO(MESNOE,'L',JLNO )
      CALL JEVEUO(NRVAL,'L',JRVAL)
      CALL JEVEUO(NIVAL,'L',JIVAL)
      CALL JEVEUO(NIORD,'L',JNIORD)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)

C     TABLEAU DES VALEURS ENTIERES DE LA TABLE: ZI(JI)
C     TABLEAU DES VALEURS REELES DE LA TABLE: ZR(JR)
C     TABLEAU DES VALEURS CARACTERES DE LA TABLE: ZK16(JK)
C     POUR DES RAISONS DE PERF, CES TABLEAUX ONT ETE SORTIS DE
C     LA BOUCLE, D'OU DES DIMENSIONS EN DUR (NOMBRE SUFFISANT)
      CALL WKVECT('&&CTNOTB.TABLE_VALR','V V R',50,JR)
      CALL WKVECT('&&CTNOTB.TABLE_VALI','V V I',50,JI)
      CALL WKVECT('&&CTNOTB.TABLE_VALK','V V K16',50,JK)

      DO 100 I=1,NBVAL

          IF(ZK24(JKCHA+I-1)(1:18).NE.'&&CHAMP_INEXISTANT')THEN


C            -- PASSAGE CHAM_NO => CHAM_NO_S
              CALL CNOCNS(ZK24(JKCHA+I-1),'V',CHAMNS)
              CALL JEVEUO(CHAMNS//'.CNSV','L',JCNSV)
              CALL JEVEUO(CHAMNS//'.CNSL','L',JCNSL)
              CALL JEVEUO(CHAMNS//'.CNSD','L',JCNSD)
              CALL JEVEUO(CHAMNS//'.CNSC','L',JCNSC)

C             NOMBRE DE NOEUDS MAX DU CHAMP: NBNOX
              NBNOX=ZI(JCNSD)
C             NOMBRE DE COMPOSANTES MAX DU CHAMP : NBCMPX
              NBCMPX=ZI(JCNSD+1)

C             NOMBRE DE COMPOSANTES DESIREES : N
              IF(TOUCMP)THEN
                  N=NBCMPX
              ELSE
                  N=NBCMP
              ENDIF

C             TABLEAU DES VALEURS DES COMPOSANTES DESIREES: ZR(JVAL)
              CALL JEDETR('&&CTNOTB.VAL_CMP')
              CALL WKVECT('&&CTNOTB.VAL_CMP','V V R',N,JVAL)
C
C             TABLEAU DES NOMS DE COMPOSANTES DESIREES : ZK8(JKVAL)
              CALL JEDETR('&&CTNOTB.NOM_CMP')
              CALL WKVECT('&&CTNOTB.NOM_CMP','V V K8',N,JKVAL)

C             -- ON PARCOURT LES NOEUDS MAX,
              DO 110 INO=1,NBNOX

C               - SI LE NOEUD FAIT PARTIE DES NOEUDS DESIRES,
C               ON POURSUIT, SINON ON VA AU NOEUD SUIVANT:
                INDNO=INDIIS(ZI(JLNO),INO,1,NBNO)
                IF(INDNO.EQ.0)GOTO 110
                KCP=0

C              - ON PARCOURT LES COMPOSANTES:
                DO 120 ICMP=1,NBCMPX
                   IF(.NOT.TOUCMP)THEN
C                    -SI LA COMPOSANTE FAIT PARTIE DES COMPOSANTES
C                     DESIREES, ON POURSUIT, SINON ON VA A LA
C                     COMPOSANTE SUIVANTE
                      INDCMP=INDIK8(ZK8(JCMP),ZK8(JCNSC+ICMP-1),
     &                              1,NBCMP)
                      IF(INDCMP.EQ.0)GOTO 120
                   ENDIF
C                  - SI LE CHAMP A UNE VALEUR, ON POURSUIT ET ON
C                  STOCKE LE NOM ET LA VALEUR DE COMPOSANTE :
                   IF(.NOT.ZL(JCNSL+NBCMPX*(INO-1)+ICMP-1))GOTO 120
                   KCP=KCP+1
                   ZR(JVAL+KCP-1)=ZR(JCNSV+NBCMPX*(INO-1)+ICMP-1)
                   ZK8(JKVAL+KCP-1)=ZK8(JCNSC+ICMP-1)
 120            CONTINUE
C
C               SOIT NI LE NOMBRE DE VALEURS ENTIERES DE LA TABLE
C               SOIT NR LE NOMBRE DE VALEURS REELES DE LA TABLE
C               SOIT NK LE NOMBRE DE VALEURS CARACTERES DE LA TABLE

                NR=NDIM+KCP
                NI=1
                NK=3
                IF(RESU.NE.' ')THEN
                    IF(TYPAC.EQ.'FREQ' .OR. TYPAC.EQ.'INST')THEN
                       NR=NR+1
                    ELSEIF(TYPAC.EQ.'MODE')THEN
                       NI=NI+1
                    ENDIF
                ELSE
                    NI=0
                    NK=2
                ENDIF


C               ON REMPLIT LES TABLEAUX ZI(JI),ZR(JR),ZK16(JK)
                KK=0
                IF(TYPAC.EQ.'FREQ' .OR. TYPAC.EQ.'INST')THEN
                   ZR(JR+KK)=ZR(JRVAL+I-1)
                   KK=KK+1
                ENDIF
                DO 121 J=1,NDIM
                   ZR(JR+KK)=ZR(JCOOR+3*(INO-1)+J-1)
                   KK=KK+1
 121            CONTINUE
                DO 122 J=1,KCP
                   ZR(JR+KK)=ZR(JVAL+J-1)
                   KK=KK+1
 122            CONTINUE

                KK=0
                IF(RESU.EQ.' ')THEN
                   ZK16(JK+KK)=ZK24(JKCHA+I-1)(1:16)
                   KK=KK+1
                ELSE
                   ZK16(JK+KK)=RESU
                   KK=KK+1
                   ZK16(JK+KK)=NSYMB
                   KK=KK+1
                   ZI(JI)=ZI(JNIORD+I-1)
                   IF(TYPAC.EQ.'MODE')ZI(JI+1)=ZI(JIVAL+I-1)
                ENDIF
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),KNO)
                ZK16(JK+KK)=KNO


C               TABLEAU DES NOMS DE PARAMETRES DE LA TABLE: ZK16(JPARAK)
                NBPARA=NR+NI+NK
                CALL JEDETR('&&CTNOTB.TABLE_PARAK')
                CALL WKVECT('&&CTNOTB.TABLE_PARAK','V V K16',NBPARA,
     &                          JPARAK)

C               ON REMPLIT ZK16(JPARAK)
                KK=0
                IF(RESU.EQ.' ')THEN
                   ZK16(JPARAK+KK)='CHAM_GD'
                   KK=KK+1
                ELSE
                   ZK16(JPARAK+KK)='RESULTAT'
                   KK=KK+1
                   ZK16(JPARAK+KK)='NOM_CHAM'
                   KK=KK+1
                   IF(TYPAC.NE.'ORDRE')THEN
                      ZK16(JPARAK+KK)=TYPAC
                      KK=KK+1
                   ENDIF
                   ZK16(JPARAK+KK)='NUME_ORDRE'
                   KK=KK+1
                ENDIF
                ZK16(JPARAK+KK)='NOEUD'
                KK=KK+1
                ZK16(JPARAK+KK)='COOR_X'
                KK=KK+1
                IF(NDIM.GE.2)THEN
                    ZK16(JPARAK+KK)='COOR_Y'
                    KK=KK+1
                ENDIF
                IF(NDIM.EQ.3)THEN
                    ZK16(JPARAK+KK)='COOR_Z'
                    KK=KK+1
                ENDIF
                DO 123 J=1,KCP
                    ZK16(JPARAK+KK)=ZK8(JKVAL+J-1)
                    KK=KK+1
 123            CONTINUE


C               ON AJOUTE LA LIGNE A LA TABLE
                IF(RESU.EQ.' ')THEN
                   CALL TBAJLI(NOMTB,NBPARA,ZK16(JPARAK),IBID,ZR(JR),
     &                          CBID,ZK16(JK),0)
                ELSE
                   CALL TBAJLI(NOMTB,NBPARA,ZK16(JPARAK),ZI(JI),ZR(JR),
     &                          CBID,ZK16(JK),0)
                ENDIF
C
 110          CONTINUE

          ENDIF

 100  CONTINUE

      CALL JEDETR('&&CTNOTB.TABLE_VALR')
      CALL JEDETR('&&CTNOTB.TABLE_VALI')
      CALL JEDETR('&&CTNOTB.TABLE_VALK')


      CALL JEDEMA()

      END
