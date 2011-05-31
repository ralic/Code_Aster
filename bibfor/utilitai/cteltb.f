      SUBROUTINE CTELTB (NBMA,MESMAI,NOMA,NBVAL,NKCHA,NKCMP,TOUCMP,
     &                   NBCMP,TYPAC,NDIM,NRVAL,RESU,NOMTB,NSYMB,
     &                   CHPGS,TYCH,NIVAL,NIORD)
      IMPLICIT   NONE
      INTEGER      NBCMP,NDIM,NBVAL,NBMA
      CHARACTER*4  TYCH
      CHARACTER*8  TYPAC,NOMA,RESU,NOMTB
      CHARACTER*16 NSYMB
      CHARACTER*19 CHPGS
      CHARACTER*24 NKCHA,NKCMP,MESMAI,NIVAL,NRVAL,NIORD
      LOGICAL      TOUCMP
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 30/05/2011   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
C
C        BUT : REMPLISSAGE DE LA TABLE POUR UN CHAM_ELEM
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
C                 MESMAI (K24) : OBJET DES NOMS DE MAILLE
C                 NRVAL (K16)  : OBJET DES VALEURS D'ACCES (REELS)
C                 NIVAL (K16)  : OBJET DES VALEURS D'ACCES (ENTIERS)
C                 NIORD (K16)  : NOM D'OBJET DES NUMEROS D'ORDRE
C                 NSYMB (K16)  : NOM SYMBOLIQUE DU CHAMP
C                 TYCH  (K4)   : TYPE DE CHAMP (ELNO OU ELGA)
C                 CHPGS (K19)  : CHAMP DES COORD DES POINTS DE GAUSS
C                 NBMA  (I)    : NOMBRE DE MAILLES UTILISATEUR
C
C        IN/OUT : NOMTB (K24)  : OBJET TABLE
C
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32,JEXNUM,JEXATR
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      INTEGER JCMP,JKCHA,JLMA,JRVAL,JIVAL,JNIORD,JCOOR,JCONX1,JCONX2
      INTEGER JCPGV,JCPGL,JCPGD,I,J,JCESV,JCESL,JCESD,JCESC,NBMAX,NBCMPX
      INTEGER N,JVAL,JKVAL,IMA,IPT,ISPT,ICMP,INDMA,INDIIS,INDIK8,NBPT,KK
      INTEGER NBCMPT,NBSPT,INOT,KCP,INDCMP,IAD,NI,NK,NR,JI,JR,JK
      INTEGER NBPARA,JPARAK
      CHARACTER*8 KMA,KNO
      COMPLEX*16 CBID
      CHARACTER*19 CHAMES




C     ------------------------------------------------------------------

      CALL JEMARQ()
C
C --- 0. INITIALISATIONS
C      -----------------
      CHAMES = '&&CTELTB.CES       '
      CALL JEVEUO(NKCMP,'L',JCMP)
      CALL JEVEUO(NKCHA,'L',JKCHA)
      CALL JEVEUO(MESMAI,'L',JLMA )
      CALL JEVEUO(NRVAL,'L',JRVAL)
      CALL JEVEUO(NIVAL,'L',JIVAL)
      CALL JEVEUO(NIORD,'L',JNIORD)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      IF(TYCH.EQ.'ELGA')THEN
          CALL JEVEUO(CHPGS//'.CESV','L',JCPGV)
          CALL JEVEUO(CHPGS//'.CESL','L',JCPGL)
          CALL JEVEUO(CHPGS//'.CESD','L',JCPGD)
      ENDIF
C     TABLEAU D'ENTIERS DE LA TABLE: ZI(JI)
C     TABLEAU DE REELS DE LA TABLE: ZR(JR)
C     TABLEAU DE CARACTERES DE LA TABLE: ZK16(JK)
C     POUR DES RAISONS DE PERF, CES TABLEAUX ONT ETE SORTIS DE
C     LA BOUCLE, D'OU DES DIMENSIONS EN DUR (NOMBRE SUFFISANT)
      CALL WKVECT('&&CTELTB.TABLE_VALR','V V R',50,JR)
      CALL WKVECT('&&CTELTB.TABLE_VALI','V V I',50,JI)
      CALL WKVECT('&&CTELTB.TABLE_VALK','V V K16',50,JK)
C
C --- 1. LECTURE DES CHAMPS ET REMPLISSAGE DE LA TABLE
C      -----------------------------------------------

      DO 100 I=1,NBVAL

          IF(ZK24(JKCHA+I-1)(1:18).NE.'&&CHAMP_INEXISTANT')THEN


C            -- PASSAGE CHAM_ELEM => CHAM_ELEM_S
              CALL CELCES(ZK24(JKCHA+I-1),'V',CHAMES)
              CALL JEVEUO(CHAMES//'.CESV','L',JCESV)
              CALL JEVEUO(CHAMES//'.CESL','L',JCESL)
              CALL JEVEUO(CHAMES//'.CESD','L',JCESD)
              CALL JEVEUO(CHAMES//'.CESC','L',JCESC)
C
C             NOMBRE DE MAILLES MAX DU CHAMP : NBMAX
              NBMAX=ZI(JCESD)

C             NOMBRE DE COMPOSANTES MAX DU CHAMP : NBCMPX
              NBCMPX=ZI(JCESD+1)

C             NOMBRE DE COMPOSANTES DESIREES : N
              IF(TOUCMP)THEN
                 N=NBCMPX
              ELSE
                 N=NBCMP
              ENDIF
C
C             TABLEAU DES VALEURS DES COMPOSANTES DESIREES: ZR(JVAL)
              CALL JEDETR('&&CTELTB.VAL_CMP')
              CALL WKVECT('&&CTELTB.VAL_CMP','V V R',N,JVAL)
C
C             TABLEAU DES NOMS DE COMPOSANTES DESIREES : ZK8(JKVAL)
              CALL JEDETR('&&CTELTB.NOM_CMP')
              CALL WKVECT('&&CTELTB.NOM_CMP','V V K8',N,JKVAL)
C
C            -- ON PARCOURT LES MAILLES
              DO 210 IMA=1,NBMAX
C
C               - SI LA MAILLE FAIT PARTIE DES MAILLES DESIREES,
C               ON POURSUIT, SINON ON VA A LA MAILLE SUIVANTE:
                INDMA=INDIIS(ZI(JLMA),IMA,1,NBMA)
                IF(INDMA.EQ.0)GOTO 210

C               NOMBRE DE POINTS DE LA MAILLE IMA : NBPT
                NBPT=ZI(JCESD+5+4*(IMA-1))
C
C               NOMBRE DE COMPOSANTES PORTEES PAR LES POINTS
C               DE LA MAILLE IMA
                NBCMPT=ZI(JCESD+5+4*(IMA-1)+2)

C               NOMBRE DE SOUS-POINTS PORTES PAR LES POINTS
                NBSPT=ZI(JCESD+5+4*(IMA-1)+1)

C               -- ON PARCOURT LES POINTS DE LA MAILLE IMA
                DO 220 IPT=1,NBPT
C
C                 NUMERO DU POINT (DU MAILLAGE GLOBAL): INOT
                  INOT = ZI(JCONX1-1+ZI(JCONX2-1+IMA)+IPT-1)

C                 -- ON PARCOURT LES SOUS-POINTS DE LA MAILLE IMA
                  DO 225 ISPT=1,NBSPT
                     KCP=0
C
C                   -- ON PARCOURT LES COMPOSANTES PORTEES
C                   PAR LE POINT IPT
                    DO 230 ICMP=1,NBCMPT

                       IF(.NOT.TOUCMP)THEN
C                        -SI LA COMPOSANTE FAIT PARTIE DES
C                         COMPOSANTES DESIREES, ON POURSUIT,
C                         SINON ON VA A LA COMPOSANTE SUIVANTE
                          INDCMP=INDIK8(ZK8(JCMP),ZK8(JCESC+ICMP-1),
     &                                  1,NBCMP)
                          IF(INDCMP.EQ.0)GOTO 230
                       ENDIF

C                      VALEUR DE LA COMPOSANTE ICMP AU POINT IPT DE
C                      LA MAILLE IMA: ZR(JCESV+IAD-1)
                       CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISPT,
     &                                  ICMP,IAD)
                       IF(IAD.GT.0)THEN
                           KCP=KCP+1
                           ZR(JVAL+KCP-1)=ZR(JCESV+IAD-1)
                           ZK8(JKVAL+KCP-1)=ZK8(JCESC+ICMP-1)
                       ENDIF
C
 230                CONTINUE
                    IF(KCP.EQ.0)GOTO 225
C
C                   SOIT NI LE NOMBRE DE ENTIERS DE LA TABLE
C                   SOIT NR LE NOMBRE DE REELS DE LA TABLE
C                   SOIT NK LE NOMBRE DE CARACTERES DE LA TABLE
                    NI=1
                    NK=3
                    NR=NDIM+KCP
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
                    IF(TYCH.EQ.'ELNO')THEN
                       NK=NK+1
                    ELSEIF(TYCH.EQ.'ELGA')THEN
                       NI=NI+2
                    ENDIF

C                   ON REMPLIT LES TABLEAUX ZI(JI),ZR(JR),ZK16(JK)
                    KK=0
                    IF(TYPAC.EQ.'FREQ' .OR. TYPAC.EQ.'INST')THEN
                           ZR(JR+KK)=ZR(JRVAL+I-1)
                           KK=KK+1
                    ENDIF
                    IF(TYCH.EQ.'ELNO')THEN
                       DO 240 J=1,NDIM
                          ZR(JR+KK)=ZR(JCOOR+3*(INOT-1)+J-1)
                          KK=KK+1
 240                   CONTINUE
                    ELSEIF(TYCH.EQ.'ELGA')THEN
                       DO 241 J=1,NDIM
                          CALL CESEXI('C',JCPGD,JCPGL,IMA,IPT,1,
     &                                    J,IAD)
                          IF(IAD.GT.0)THEN
                             ZR(JR+KK)=ZR(JCPGV+IAD-1)
                             KK=KK+1
                          ENDIF
 241                   CONTINUE
                    ENDIF
                    DO 250 J=1,KCP
                       ZR(JR+KK)=ZR(JVAL+J-1)
                       KK=KK+1
 250                CONTINUE

                    KK=0
                    IF(RESU.NE.' ')THEN
                       ZI(JI+KK)=ZI(JNIORD+I-1)
                       KK=KK+1
                    ENDIF
                    IF(TYPAC.EQ.'MODE')THEN
                        ZI(JI+KK)=ZI(JIVAL+I-1)
                        KK=KK+1
                    ENDIF
                    IF(TYCH.EQ.'ELGA')THEN
                        ZI(JI+KK)=IPT
                        KK=KK+1
                        ZI(JI+KK)=ISPT
                        KK=KK+1
                    ENDIF

                    KK=0
                    IF(RESU.EQ.' ')THEN
                       ZK16(JK+KK)=ZK24(JKCHA+I-1)(1:16)
                       KK=KK+1
                    ELSE
                       ZK16(JK+KK)=RESU
                       KK=KK+1
                       ZK16(JK+KK)=NSYMB
                       KK=KK+1
                    ENDIF
                    CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMA),KMA)
                    ZK16(JK+KK)=KMA
                    KK=KK+1
                    IF(TYCH.EQ.'ELNO')THEN
                       CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INOT),KNO)
                       ZK16(JK+KK)=KNO
                       KK=KK+1
                    ENDIF

C                   TABLEAU DES NOMS DE PARAMETRES DE LA TABLE
                    NBPARA=NR+NI+NK
                    CALL JEDETR('&&CTELTB.TABLE_PARAK')
                    CALL WKVECT('&&CTELTB.TABLE_PARAK','V V K16',
     &                               NBPARA,JPARAK)

                    KK=0
                    IF(RESU.EQ.' ')THEN
                      ZK16(JPARAK+KK)='CHAM_GD'
                      KK=KK+1
                    ELSE
                      ZK16(JPARAK+KK)='RESULTAT'
                      KK=KK+1
                      ZK16(JPARAK+KK)='NOM_CHAM'
                      KK=KK+1
                    ENDIF

                    IF(RESU.NE.' ')THEN
                       IF(TYPAC.NE.'ORDRE')THEN
                         ZK16(JPARAK+KK)=TYPAC
                         KK=KK+1
                       ENDIF
                       ZK16(JPARAK+KK)='NUME_ORDRE'
                       KK=KK+1
                    ENDIF
                    ZK16(JPARAK+KK)='MAILLE'
                    KK=KK+1
                    IF(TYCH.EQ.'ELNO')THEN
                       ZK16(JPARAK+KK)='NOEUD'
                       KK=KK+1
                    ELSEIF(TYCH.EQ.'ELGA')THEN
                       ZK16(JPARAK+KK)='POINT'
                       KK=KK+1
                       ZK16(JPARAK+KK)='SOUS_POINT'
                       KK=KK+1
                    ENDIF
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
                    DO 260 J=1,KCP
                       ZK16(JPARAK+KK)=ZK8(JKVAL+J-1)
                       KK=KK+1
 260                CONTINUE

C                       ON AJOUTE LA LIGNE A LA TABLE
                    CALL TBAJLI(NOMTB,NBPARA,ZK16(JPARAK),ZI(JI),
     &                          ZR(JR),CBID,ZK16(JK),0)

 225              CONTINUE

 220            CONTINUE

 210          CONTINUE

          ENDIF

 100  CONTINUE

      CALL JEDETR('&&CTELTB.TABLE_VALR')
      CALL JEDETR('&&CTELTB.TABLE_VALI')
      CALL JEDETR('&&CTELTB.TABLE_VALK')

      CALL JEDEMA()

      END
