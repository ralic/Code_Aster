      SUBROUTINE IRGMCE(CHAMSY,IFI,NOMCON,ORDR,NBORDR,COORD,CONNX,POINT,
     &                  NJVPOI,NJVSEG,NJVTRI,NJVQUA,NJVTET,NJVPYR,
     &                  NJVPRI,NJVHEX,NBPOI,NBSEG,NBTRI,NBQUA,NBTET,
     &                  NBPYR,NBPRI,NBHEX,NBCMPI,NOMCMP,
     &                 LRESU,PARA,NOMAOU,NOMAIN,VERSIO)
      IMPLICIT NONE
      CHARACTER*(*) NOMCON,CHAMSY,NJVPOI,NJVSEG,NJVTRI,NJVTET,NOMCMP(*)
      CHARACTER*(*) NJVQUA,NJVPYR,NJVPRI,NJVHEX
      CHARACTER*8 NOMAOU,NOMAIN
      REAL*8 COORD(*),PARA(*)
      LOGICAL LRESU
      INTEGER NBPOI,NBSEG,NBTRI,NBTET,NBCMPI,IFI,NBORDR,VERSIO
      INTEGER NBQUA,NBPYR,NBPRI,NBHEX
      INTEGER ORDR(*),CONNX(*),POINT(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 07/01/2003   AUTEUR JMBHH01 J.M.PROIX 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21

C        IMPRESSION D'UN CHAM_ELEM AU FORMAT GMSH

C        CHAMSY : NOM DU CHAM_ELEM A ECRIRE
C        IFI    : NUMERO D'UNITE LOGIQUE DU FICHIER DE SORTIE GMSH
C        NOMCON : NOM DU CONCEPT A IMPRIMER
C        NBORDR : NOMBRE DE NUMEROS D'ORDRE DANS LE TABLEAU ORDR
C        ORDR   : LISTE DES NUMEROS D'ORDRE A IMPRIMER
C        COORD  : VECTEUR COORDONNEES DES NOEUDS DU MAILLAGE
C        CONNX  : VECTEUR CONNECTIVITES DES NOEUDS DU MAILLAGE
C        POINT  : VECTEUR DU NOMBRE DE NOEUDS DES MAILLES DU MAILLAGE
C        NJVPOI : NOM JEVEUX DEFINISSANT LES ELEMENTS POI1 DU MAILLAGE
C        NJVSEG : NOM JEVEUX DEFINISSANT LES ELEMENTS SEG2 DU MAILLAGE
C        NJVTRI : NOM JEVEUX DEFINISSANT LES ELEMENTS TRI3 DU MAILLAGE
C        NJVQUA : NOM JEVEUX DEFINISSANT LES ELEMENTS QUA4 DU MAILLAGE
C        NJVTET : NOM JEVEUX DEFINISSANT LES ELEMENTS TET4 DU MAILLAGE
C        NJVPYR : NOM JEVEUX DEFINISSANT LES ELEMENTS PYR5 DU MAILLAGE
C        NJVPRI : NOM JEVEUX DEFINISSANT LES ELEMENTS PRI6 DU MAILLAGE
C        NJVHEX : NOM JEVEUX DEFINISSANT LES ELEMENTS HEX8 DU MAILLAGE
C        NBPOI  : NOMBRE D'ELEMENTS POI1 DU MAILLAGE
C        NBSEG  : NOMBRE D'ELEMENTS SEG2 DU MAILLAGE
C        NBTRI  : NOMBRE D'ELEMENTS TRI3 DU MAILLAGE
C        NBQUA  : NOMBRE D'ELEMENTS QUA4 DU MAILLAGE
C        NBTET  : NOMBRE D'ELEMENTS TET4 DU MAILLAGE
C        NBPYR  : NOMBRE D'ELEMENTS PYR5 DU MAILLAGE
C        NBPRI  : NOMBRE D'ELEMENTS PRI6 DU MAILLAGE
C        NBHEX  : NOMBRE D'ELEMENTS HEXA8 DU MAILLAGE
C        NBCMPI : NOMBRE DE COMPOSANTES DEMANDEES A IMPRIMER
C        NOMCMP : NOMS DES COMPOSANTES DEMANDEES A IMPRIMER
C        LRESU  : LOGIQUE INDIQUANT SI NOMCON EST UNE SD RESULTAT
C        PARA   : VALEURS DES VARIABLES D'ACCES (INST, FREQ)
C        NOMAOU : NOM DU MAILLAGE REDECOUPE
C        VERSIO : NUMERO DE LA VERSION GMSH UTILISEE (1 OU 2)

C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER IOR,J,K,INOE,IMA,LISTNO(8),IX,NBNO
      INTEGER IPOI,ISEG,ITRI,ITET,JPOI,JSEG,JTRI,JTET
      INTEGER IQUA,IPYR,IPRI,IHEX,JQUA,JPYR,JPRI,JHEX
      INTEGER IBID,NBCMP,IPOIN,IRET,JCESC,JCESL
      INTEGER JTABC,JTABD,JTABV,JTABL,JCESK,JCESD
      INTEGER ICMP,JNCMP,IPT,ISP,NBPT,NBSP,JNUMOL
      INTEGER NBMA,NCMPU,IAD,NBCMPD,NBORD2,IADMAX,IADMM
      INTEGER NBPOI2,NBSEG2,NBTRI2,NBQUA2,NBTET2
      INTEGER NBPYR2,NBPRI2,NBHEX2
      LOGICAL IWRI, TENS
      REAL*8 VALE
      CHARACTER*1 TSCA
      CHARACTER*8 K8B,NOMGD,TYPE,NOCMP
      CHARACTER*19 NOCH19,CHAMPS
      CHARACTER*24 NUMOLD,CONNEX
C     ------------------------------------------------------------------

      CALL JEMARQ()

      NBORD2 = MAX(1,NBORDR)
      NUMOLD = NOMAOU//'.NUMOLD         '
      CONNEX = NOMAIN//'.CONNEX         '

      CALL WKVECT('&&IRGMCE.CESD','V V I',NBORD2,JTABD)
      CALL WKVECT('&&IRGMCE.CESC','V V I',NBORD2,JTABC)
      CALL WKVECT('&&IRGMCE.CESV','V V I',NBORD2,JTABV)
      CALL WKVECT('&&IRGMCE.CESL','V V I',NBORD2,JTABL)

      NBCMP = 0

      DO 60 IOR = 1,NBORD2
        IF (LRESU) THEN
          CALL RSEXCH(NOMCON,CHAMSY,ORDR(IOR),NOCH19,IRET)
          IF (IRET.NE.0) GO TO 60
        ELSE
          NOCH19 = NOMCON
        END IF
        CALL CODENT(IOR,'D0',K8B)
        CHAMPS = '&&IRGMCE.CH'//K8B
        CALL CELCES(NOCH19,'V',CHAMPS)
        CALL JEVEUO(CHAMPS//'.CESK','L',JCESK)
        CALL JEVEUO(CHAMPS//'.CESD','L',ZI(JTABD+IOR-1))
        CALL JEVEUO(CHAMPS//'.CESC','L',ZI(JTABC+IOR-1))
        CALL JEVEUO(CHAMPS//'.CESV','L',ZI(JTABV+IOR-1))
        CALL JEVEUO(CHAMPS//'.CESL','L',ZI(JTABL+IOR-1))

        NOMGD = ZK8(JCESK-1+2)
        CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)
        IF (TSCA.NE.'R') THEN
          CALL UTMESS('F','IRGMCE','ON IMPRIME QUE DES CHAMPS REELS')
        END IF

        TYPE = ZK8(JCESK-1+3)
        IF (TYPE(1:4).NE.'ELNO') THEN
          CALL UTMESS('F','IRGMCE','ON IMPRIME QUE DES CHAMPS ELNO')
        END IF

        IF (IOR.EQ.1) THEN
          JCESC = ZI(JTABC+IOR-1)
          JCESD = ZI(JTABD+IOR-1)
          JCESL = ZI(JTABL+IOR-1)
          NBMA = ZI(JCESD-1+1)
          NBCMP = ZI(JCESD-1+2)
          NCMPU = 0
          CALL WKVECT('&&IRGMCE.NOCMP','V V K8',NBCMP,JNCMP)
          DO 50,ICMP = 1,NBCMP
            DO 30,IMA = 1,NBMA
              NBPT = ZI(JCESD-1+5+4* (IMA-1)+1)
              NBSP = ZI(JCESD-1+5+4* (IMA-1)+2)
              DO 20,IPT = 1,NBPT
                DO 10,ISP = 1,NBSP
                  CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD)
                  IF (IAD.GT.0) GO TO 40
   10           CONTINUE
   20         CONTINUE
   30       CONTINUE
            GO TO 50
   40       CONTINUE
            NCMPU = NCMPU + 1
            ZK8(JNCMP+NCMPU-1) = ZK8(JCESC-1+ICMP)
   50     CONTINUE
        ELSE
          IF (ZI(ZI(JTABD+IOR-1)-1+2).NE.NBCMP) THEN
            CALL UTMESS('F','IRGMCE','NBCMP DIFFERENT')
          END IF
        END IF

   60 CONTINUE

C --- RECUPERATION DU TABLEAU DE CORRESPONDANCE ENTRE NUMERO DES
C     NOUVELLES MAILLES ET NUMERO DE LA MAILLE INITIALE
C     CREE PAR IRGMMA
C
      CALL JEVEUO ( NUMOLD, 'L', JNUMOL )
      NBPOI2 = 0
      NBSEG2 = 0
      NBTRI2 = 0
      NBQUA2 = 0
      NBTET2 = 0
      NBPYR2 = 0
      NBPRI2 = 0
      NBHEX2 = 0
C
      TENS = .FALSE.

C --- BOUCLE SUR LE NOMBRE DE COMPOSANTES DU CHAM_ELEM
C     *************************************************
      IF (NBCMPI.EQ.0) THEN
        NBCMPD = NBCMP
        IF (CHAMSY(1:2).EQ.'SI'.OR.CHAMSY(1:2).EQ.'EP') THEN
          TENS = .TRUE.
        ENDIF
      ELSE
        NBCMPD = NBCMPI
      END IF
      IF (VERSIO.EQ.1) THEN
        TENS = .FALSE.
      ENDIF

      DO 270 K = 1,NBCMPD

        IF (NBCMPI.NE.0) THEN
          DO 70 IX = 1,NBCMP
            IF (ZK8(JNCMP+IX-1).EQ.NOMCMP(K)) THEN
              ICMP = IX
              GO TO 80
            END IF
   70     CONTINUE
          K8B = NOMCMP(K)
          CALL UTMESS('F','IRGNCE','CMP INCONNUE'//K8B)
   80     CONTINUE
        ELSE
          ICMP = K
        END IF
        NOCMP = ZK8(JNCMP+ICMP-1)

C ----- PREMIER PASSAGE POUR DETERMINER SI LE CHAMP A ECRIRE EXISTE
C       SUR LES POI1, SEG2, TRIA3, TETR4
C       DONC ON  N'ECRIT RIEN
        IWRI = .FALSE.

C ----- BOUCLE SUR LES ELEMENTS DE TYPES POI1
C       -------------------------------------
        IF (NBPOI.NE.0) THEN
          IADMM = 0
          NBNO = 1
          CALL JEVEUO(NJVPOI,'L',JPOI)
          DO 90 IPOI = 1,NBPOI
            IMA = ZI(JPOI-1+IPOI)
            IPOIN = POINT(IMA)
            LISTNO(1) = CONNX(IPOIN)
            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)
            IADMM = MAX(IADMAX,IADMM)
   90     CONTINUE
          IF (IADMM.GT.0)  NBPOI2 = NBPOI
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES SEG2
C       -------------------------------------
        IF (NBSEG.NE.0) THEN
          IADMM = 0
          NBNO = 2
          CALL JEVEUO(NJVSEG,'L',JSEG)
          DO 110 ISEG = 1,NBSEG
            IMA = ZI(JSEG-1+ISEG)
            IPOIN = POINT(IMA)
            DO 100 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  100       CONTINUE
            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)
            IADMM = MAX(IADMAX,IADMM)
  110     CONTINUE
          IF (IADMM.GT.0)  NBSEG2 = NBSEG
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES TRIA3
C       --------------------------------------
        IF (NBTRI.NE.0) THEN
          IADMM = 0
          NBNO = 3
          CALL JEVEUO(NJVTRI,'L',JTRI)
          DO 130 ITRI = 1,NBTRI
            IMA = ZI(JTRI-1+ITRI)
            IPOIN = POINT(IMA)
            DO 120 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  120       CONTINUE
            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)
            IADMM = MAX(IADMAX,IADMM)
  130     CONTINUE
          IF (IADMM.GT.0)  NBTRI2 = NBTRI
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES QUA4
C       --------------------------------------
        IF (VERSIO.EQ.2) THEN
          IF (NBQUA.NE.0) THEN
            IADMM = 0
            NBNO  = 4
            CALL JEVEUO(NJVQUA,'L',JQUA)
            DO 300 IQUA = 1,NBQUA
              IMA = ZI(JQUA-1+IQUA)
              IPOIN = POINT(IMA)
              DO 500 INOE = 1,NBNO
                LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  500       CONTINUE
             CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)
              IADMM = MAX(IADMAX,IADMM)
  300       CONTINUE
            IF (IADMM.GT.0)  NBQUA2 = NBQUA
          END IF
        END IF
C ----- BOUCLE SUR LES ELEMENTS DE TYPES TETRA4
C       ---------------------------------------
        IF (NBTET.NE.0) THEN
          IADMM = 0
          NBNO = 4
          CALL JEVEUO(NJVTET,'L',JTET)
          DO 150 ITET = 1,NBTET
            IMA = ZI(JTET-1+ITET)
            IPOIN = POINT(IMA)
            DO 140 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  140       CONTINUE
            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)
            IADMM = MAX(IADMAX,IADMM)
  150     CONTINUE
          IF (IADMM.GT.0)  NBTET2 = NBTET
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES PYRAM5
C       ---------------------------------------
        IF (VERSIO.EQ.2) THEN
          IF (NBPYR.NE.0) THEN
            IADMM = 0
            NBNO  = 5
            CALL JEVEUO(NJVPYR,'L',JPYR)
            DO 310 IPYR = 1,NBPYR
              IMA = ZI(JPYR-1+IPYR)
              IPOIN = POINT(IMA)
              DO 510 INOE = 1,NBNO
                LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  510       CONTINUE
            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)
              IADMM = MAX(IADMAX,IADMM)
  310       CONTINUE
            IF (IADMM.GT.0)  NBPYR2 = NBPYR
          END IF
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES PENTA6
C       ---------------------------------------
        IF (VERSIO.EQ.2) THEN
          IF (NBPRI.NE.0) THEN
            IADMM = 0
            NBNO  = 6
            CALL JEVEUO(NJVPRI,'L',JPRI)
            DO 320 IPRI = 1,NBPRI
              IMA = ZI(JPRI-1+IPRI)
              IPOIN = POINT(IMA)
              DO 520 INOE = 1,NBNO
                LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  520       CONTINUE
            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)
              IADMM = MAX(IADMAX,IADMM)
  320       CONTINUE
            IF (IADMM.GT.0)  NBPRI2 = NBPRI
          END IF
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES HEXA8
C       --------------------------------------
        IF (VERSIO.EQ.2) THEN
          IF (NBHEX.NE.0) THEN
            IADMM = 0
            NBNO  = 8
            CALL JEVEUO(NJVHEX,'L',JHEX)
            DO 330 IHEX = 1,NBHEX
              IMA = ZI(JHEX-1+IHEX)
              IPOIN = POINT(IMA)
              DO 530 INOE = 1,NBNO
                LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  530       CONTINUE
            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)
              IADMM = MAX(IADMAX,IADMM)
  330       CONTINUE
            IF (IADMM.GT.0)  NBHEX2 = NBHEX
          END IF
        END IF

        IF (.NOT.TENS) THEN

C ----- ECRITURE DE L'ENTETE DE View
C       ****************************
C
        CALL IRGMPV(IFI,LRESU,NOMCON,CHAMSY,NBORD2,PARA,NOCMP,NBPOI2,
     &              NBSEG2,NBTRI2,NBQUA2,NBTET2,NBPYR2,NBPRI2,NBHEX2,
     &              .TRUE.,.FALSE.,.FALSE.,VERSIO)
C
        IWRI = .TRUE.

C ----- BOUCLE SUR LES ELEMENTS DE TYPES POI1
C       -------------------------------------
        IF (NBPOI2.NE.0) THEN
          NBNO = 1
          CALL JEVEUO(NJVPOI,'L',JPOI)
          DO 170 IPOI = 1,NBPOI
            IMA = ZI(JPOI-1+IPOI)
            IPOIN = POINT(IMA)
            LISTNO(1) = CONNX(IPOIN)
            DO 160 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(1)-1)+J))
  160       CONTINUE

            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)

  170     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES SEG2
C       -------------------------------------
        IF (NBSEG2.NE.0) THEN
          NBNO = 2
          CALL JEVEUO(NJVSEG,'L',JSEG)
          DO 200 ISEG = 1,NBSEG
            IMA = ZI(JSEG-1+ISEG)
            IPOIN = POINT(IMA)
            DO 180 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  180       CONTINUE
            DO 190 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  190       CONTINUE

            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)

  200     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES TRIA3
C       --------------------------------------
        IF (NBTRI2.NE.0) THEN
          NBNO = 3
          CALL JEVEUO(NJVTRI,'L',JTRI)
          DO 230 ITRI = 1,NBTRI
            IMA = ZI(JTRI-1+ITRI)
            IPOIN = POINT(IMA)
            DO 210 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  210       CONTINUE
            DO 220 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  220       CONTINUE

            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)

  230     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES QUA4
C       --------------------------------------
        IF (NBQUA2.NE.0) THEN
          CALL JEVEUO(NJVQUA,'L',JQUA)
          NBNO = 4
          DO 450 IQUA = 1,NBQUA
            IMA = ZI(JQUA-1+IQUA)
            IPOIN = POINT(IMA)
            DO 340 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  340       CONTINUE
            DO 350 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  350       CONTINUE

            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)


  450     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES TETRA4
C       ---------------------------------------
        IF (NBTET2.NE.0) THEN
          NBNO = 4
          CALL JEVEUO(NJVTET,'L',JTET)
          DO 260 ITET = 1,NBTET
            IMA = ZI(JTET-1+ITET)
            IPOIN = POINT(IMA)
            DO 240 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  240       CONTINUE
            DO 250 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  250       CONTINUE

            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)

  260     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES PYRAM5
C       ---------------------------------------
        IF (NBPYR2.NE.0) THEN
          CALL JEVEUO(NJVPYR,'L',JPYR)
          NBNO = 5
          DO 360 IPYR = 1,NBPYR
            IMA = ZI(JPYR-1+IPYR)
            IPOIN = POINT(IMA)
            DO 370 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  370       CONTINUE
            DO 380 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  380       CONTINUE

            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)

  360     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES PENTA6
C       ---------------------------------------
        IF (NBPRI2.NE.0) THEN
          CALL JEVEUO(NJVPRI,'L',JPRI)
          NBNO = 6
          DO 390 IPRI = 1,NBPRI
            IMA = ZI(JPRI-1+IPRI)
            IPOIN = POINT(IMA)
            DO 400 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  400       CONTINUE
            DO 410 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  410       CONTINUE

            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)

  390     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES HEXA8
C       --------------------------------------
        IF (NBHEX2.NE.0) THEN
          CALL JEVEUO(NJVHEX,'L',JHEX)
          NBNO = 8
          DO 420 IHEX = 1,NBHEX
            IMA = ZI(JHEX-1+IHEX)
            IPOIN = POINT(IMA)
            DO 430 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  430       CONTINUE
            DO 440 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  440       CONTINUE

            CALL IRGMEC(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,ICMP,IFI,IWRI,
     &                  IADMAX)

  420     CONTINUE
        END IF

C ----- FIN D'ECRITURE DE View
C       **********************

        WRITE (IFI,1010) '$EndView'

      END IF

  270 CONTINUE
C

      IF (TENS) THEN
C
C ----- ECRITURE DE L'ENTETE DE View
C       ****************************
C 
         IF (VERSIO.EQ.2) THEN
            CALL UTMESS('A','IMPR_RESU','ATTENTION, IL FAUT SPECIFIER '
     &     //'LES NOMS DES COMPOSANTES DU TENSEUR POUR POUVOIR LES '
     &     //'VISUALISER SEPAREMENT AVEC GMSH (VERSION 1.36)')
         ENDIF
        NOCMP = 'TENSEUR'
        CALL IRGMPV(IFI,LRESU,NOMCON,CHAMSY,NBORD2,PARA,NOCMP,NBPOI2,
     &              NBSEG2,NBTRI2,NBQUA2,NBTET2,NBPYR2,NBPRI2,NBHEX2,
     &              .FALSE.,.FALSE.,.TRUE.,VERSIO)
C
        IWRI = .TRUE.

C ----- BOUCLE SUR LES ELEMENTS DE TYPES POI1
C       -------------------------------------
        IF (NBPOI2.NE.0) THEN
          NBNO = 1
          CALL JEVEUO(NJVPOI,'L',JPOI)
          DO 570 IPOI = 1,NBPOI
            IMA = ZI(JPOI-1+IPOI)
            IPOIN = POINT(IMA)
            LISTNO(1) = CONNX(IPOIN)
            DO 560 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(1)-1)+J))
  560       CONTINUE

            CALL IRGME2(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,NBCMP,IFI,IWRI,
     &                  IADMAX)

  570     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES SEG2
C       -------------------------------------
        IF (NBSEG2.NE.0) THEN
          NBNO = 2
          CALL JEVEUO(NJVSEG,'L',JSEG)
          DO 600 ISEG = 1,NBSEG
            IMA = ZI(JSEG-1+ISEG)
            IPOIN = POINT(IMA)
            DO 580 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  580       CONTINUE
            DO 590 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  590       CONTINUE

            CALL IRGME2(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,NBCMP,IFI,IWRI,
     &                  IADMAX)

  600     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES TRIA3
C       --------------------------------------
        IF (NBTRI2.NE.0) THEN
          NBNO = 3
          CALL JEVEUO(NJVTRI,'L',JTRI)
          DO 630 ITRI = 1,NBTRI
            IMA = ZI(JTRI-1+ITRI)
            IPOIN = POINT(IMA)
            DO 610 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  610       CONTINUE
            DO 620 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  620       CONTINUE

            CALL IRGME2(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,NBCMP,IFI,IWRI,
     &                  IADMAX)

  630     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES QUA4
C       --------------------------------------
        IF (NBQUA2.NE.0) THEN
          CALL JEVEUO(NJVQUA,'L',JQUA)
          NBNO = 4
          DO 650 IQUA = 1,NBQUA
            IMA = ZI(JQUA-1+IQUA)
            IPOIN = POINT(IMA)
            DO 640 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  640       CONTINUE
            DO 660 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  660       CONTINUE

            CALL IRGME2(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,NBCMP,IFI,IWRI,
     &                  IADMAX)


  650     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES TETRA4
C       ---------------------------------------
        IF (NBTET2.NE.0) THEN
          NBNO = 4
          CALL JEVEUO(NJVTET,'L',JTET)
          DO 760 ITET = 1,NBTET
            IMA = ZI(JTET-1+ITET)
            IPOIN = POINT(IMA)
            DO 740 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  740       CONTINUE
            DO 750 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  750       CONTINUE

            CALL IRGME2(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,NBCMP,IFI,IWRI,
     &                  IADMAX)

  760     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES PYRAM5
C       ---------------------------------------
        IF (NBPYR2.NE.0) THEN
          CALL JEVEUO(NJVPYR,'L',JPYR)
          NBNO = 5
          DO 860 IPYR = 1,NBPYR
            IMA = ZI(JPYR-1+IPYR)
            IPOIN = POINT(IMA)
            DO 870 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  870       CONTINUE
            DO 880 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  880       CONTINUE

            CALL IRGME2(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,NBCMP,IFI,IWRI,
     &                  IADMAX)

  860     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES PENTA6
C       ---------------------------------------
        IF (NBPRI2.NE.0) THEN
          CALL JEVEUO(NJVPRI,'L',JPRI)
          NBNO = 6
          DO 890 IPRI = 1,NBPRI
            IMA = ZI(JPRI-1+IPRI)
            IPOIN = POINT(IMA)
            DO 900 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  900       CONTINUE
            DO 910 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  910       CONTINUE

            CALL IRGME2(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,NBCMP,IFI,IWRI,
     &                  IADMAX)

  890     CONTINUE
        END IF

C ----- BOUCLE SUR LES ELEMENTS DE TYPES HEXA8
C       --------------------------------------
        IF (NBHEX2.NE.0) THEN
          CALL JEVEUO(NJVHEX,'L',JHEX)
          NBNO = 8
          DO 920 IHEX = 1,NBHEX
            IMA = ZI(JHEX-1+IHEX)
            IPOIN = POINT(IMA)
            DO 930 INOE = 1,NBNO
              LISTNO(INOE) = CONNX(IPOIN-1+INOE)
  930       CONTINUE
            DO 940 J = 1,3
              WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
  940       CONTINUE

            CALL IRGME2(ZI(JNUMOL),IMA,CONNEX,NBORD2,ZI(JTABD),
     &                  ZI(JTABL),ZI(JTABV),NBNO,LISTNO,NBCMP,IFI,IWRI,
     &                  IADMAX)

  920     CONTINUE
        END IF

C ----- FIN D'ECRITURE DE View
C       **********************

        WRITE (IFI,1010) '$EndView'
C
      END IF

      CALL JEDETR('&&IRGMCE.CESC')
      CALL JEDETR('&&IRGMCE.CESD')
      CALL JEDETR('&&IRGMCE.CESV')
      CALL JEDETR('&&IRGMCE.CESL')
      CALL JEDETR('&&IRGMCE.NOCMP')

      CALL JEDEMA()

 1000 FORMAT (1P,4(E15.8,1X))
 1010 FORMAT (A8)

      END
