      SUBROUTINE GCOUR2 ( RESU, NOMA, NOMO, NOMNO, COORN, NBNOEU, TRAV1,
     &          TRAV2,TRAV3,CHFOND,FOND,CONNEX,STOK4,THLAGR,THLAG2,NBRE,
     &          MILIEU,NDIMTE,PAIR)
       IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/09/2010   AUTEUR DESOZA T.DESOZA 
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
C TOLE CRP_20
C
C FONCTION REALISEE:
C
C 1.  POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
C     LE TRIPLET ( MODULE(THETA), RINF, RSUP )
C
C 2.  PUIS ON  CALCULE LA DIRECTION DES CHAMPS THETA
C     APPEL A GDIREC
C
C 3.  ENSUITE ON CALCULE LES CHAMPS THETA SUR TOUS LES NOEUDS DU
C     MAILLAGE
C
C     ------------------------------------------------------------------
C ENTREE:
C        RESU   : NOM DU CONCEPT RESULTAT
C        NOMA   : NOM DU CONCEPT MAILLAGE
C        NOMO   : NOM DU CONCEPT MODELE
C        NOMNO  : NOM DE L'OBJET CONTENANT LES NOEUDS DU MAILLAGE
C        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DU MAILLAGE
C        NBNOEU  : NOMBRE DE NOEUDS DE GAMM0
C        CHFOND : NOMS DES NOEUDS DU FOND DE FISSURE
C        FOND   : NOM DU CONCEPT FOND_FISS
C        TRAV1  : RINF
C        TRAV2  : RSUP
C        THLAGR  : SI PRESENCE DU MOT CLE THETA_LOCAL
C        NBRE   : DEGRE DES POLYNOMES DE LEGENDRE
C                     SINON 0
C        CONNEX: .TRUE.  : FOND DE FISSURE FERME
C                .FALSE. : FOND DE FISSURE DEBOUCHANT
C SORTIE:
C        STOK4  : DIRECTION DU CHAMP THETA
C                 LISTE DE CHAMPS_NO THETA
C        TRAV3 : MODULE(THETA)
C        MILIEU: .TRUE.  : ELEMENT QUADRATIQUE
C                .FALSE. : ELEMENT LINEAIRE
C     ------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNOM
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*24      TRAV1,TRAV2,TRAV3,OBJOR,OBJEX,CHFOND,REPK
      CHARACTER*24      OBJ3,NORM,NUMGAM,CHAMNO
      CHARACTER*24      STOK4,DIRE4,COORN,NOMNO,DIRE5,INDICG
      CHARACTER*24      ABSGAM
      CHARACTER*16      K16B,NOMCMD
      CHARACTER*8       CHBID, FOND, RESU, NOMA, NOMO,K8B
      CHARACTER*6       KIORD
C
      INTEGER           NBNOEU,IADRT1,IADRT2,IADRT3,ITHETA
      INTEGER           IN2,IADRCO,JMIN,IELINF,IADNUM
      INTEGER           IADRNO,NUM,INDIC,IERD,IADRTT,NBRE,NBR8
      INTEGER           IRET,NUMA,NDIMTE,IAORIG
      INTEGER           ITANEX,ITANOR,NBNOS,IADABS,KNO,IAEXTR,JNORM
C
      REAL*8            DIRX,DIRY,DIRZ,XI1,YI1,ZI1,XJ1,YJ1,ZJ1
      REAL*8            XIJ,YIJ,ZIJ,EPS,D,TEI,TEJ,DIR(3)
      REAL*8            XM,YM,ZM,XIM,YIM,ZIM,S,DMIN,SMIN,XN,YN,ZN
      REAL*8            RII,RSI,ALPHA,VALX,VALY,VALZ,NORM2,PSCA
      REAL*8            NORME,VECX,VECY,VECZ,R8MAEM,XL,TMPV(3)
C
      LOGICAL           THLAGR,MILIEU,CONNEX,THLAG2,PAIR
C
      CALL JEMARQ()

      CALL GETRES(K8B,K16B,NOMCMD)

      EPS = 1.D-06
      CALL JEVEUO(TRAV1,'L',IADRT1)
      CALL JEVEUO(TRAV2,'L',IADRT2)
      CALL JEVEUO(TRAV3,'E',IADRT3)
      CALL JEVEUO(CHFOND,'L',IADRNO)
      CALL JEVEUO(COORN,'L',IADRCO)
C
C RECUPERATION  DES NUMEROS DE NOEUDS DE GAMM0
C
      NUMGAM = '&&COURON.NUMGAMM0'
      CALL WKVECT(NUMGAM,'V V I',NBNOEU,IADNUM)
      DO 550 J=1,NBNOEU
            CALL JENONU(JEXNOM(NOMNO,ZK8(IADRNO+J-1)),ZI(IADNUM+J-1))
550   CONTINUE
C
C RECUPERATION DES DIRECTIONS AUX EXTREMITES DE GAMM0
C
      OBJOR  = FOND//'.DTAN_ORIGINE'
      CALL JEEXIN(OBJOR,ITANOR)
      OBJEX  = FOND//'.DTAN_EXTREMITE'
      CALL JEEXIN(OBJEX,ITANEX)
C
C  SI LEVRE_SUP EST DEFINIE DANS LE CONCEPT FOND
C
      OBJ3  = FOND//'.LEVRESUP  .MAIL'
      CALL JEEXIN(OBJ3,IELSUP)
C
C  SI LEVRE_INF EST DEFINIE DANS LE CONCEPT FOND
C
      OBJ3  = FOND//'.LEVREINF  .MAIL'
      CALL JEEXIN(OBJ3,IELINF)
C
C  SI NORMALE EST DEFINIE DANS LE CONCEPT FOND
C
      NORM  = FOND//'.NORMALE        '
      CALL JEEXIN(NORM,IENORM)
C
      STOK4 = '&&COURON.DIREC'
      CALL WKVECT(STOK4,'V V R',3*NBNOEU,IN2)
C
      DIRE4 = '&&COURON.LEVRESUP'
      DIRE5 = '&&COURON.LEVREINF'
C
C  RECUPERATION DIRECTION DU CHAMP THETA
C
C     DANS LE CAS OU LA NORMALE EST DEFINIE DANS DEFI_FOND_FISS/NORMALE,
C     ON AVERTIT L'UTILISATEUR PAR UNE ALARME SI LA DIRECTION N'EST PAS
C     FOURNIE
      CALL GETVR8 ( 'THETA', 'DIRECTION', 1, 1, 3, DIR, NBR8 )
      IF(NBR8.EQ.0.AND.IENORM.NE.0)THEN
        CALL U2MESS('A','RUPTURE0_91')
      ENDIF
C     ON VERIFIE QUE LA DIRECTION FOURNIE EST ORTHOGONALE A LA NORMALE
      IF(NBR8.NE.0.AND.IENORM.NE.0)THEN
          CALL JEVEUO(NORM,'L',JNORM)
          CALL DCOPY(3,ZR(JNORM),1,TMPV,1)
          CALL NORMEV(DIR,NORME)
          CALL NORMEV(TMPV,NORME)
          CALL LCPRSN(3,DIR,TMPV,PSCA)
          IF(ABS(PSCA).GT.0.1D0)CALL U2MESS('F','RUPTURE0_94')
      ENDIF
C
C 1ER CAS: LA DIRECTION DE THETA EST DONNEE, ON LA NORME
C
      IF (NBR8.NE.0) THEN

        NORME = 0.D0
        DO 991 I=1,3
          NORME =  NORME + DIR(I)*DIR(I)
991     CONTINUE
        NORME = SQRT(NORME)
        DO 1 I=1,NBNOEU
          ZR(IN2+(I-1)*3+1-1) = DIR(1)/NORME
          ZR(IN2+(I-1)*3+2-1) = DIR(2)/NORME
          ZR(IN2+(I-1)*3+3-1) = DIR(3)/NORME
1       CONTINUE
        CALL DISMOI('F','ELEM_VOLU_QUAD',NOMO,'MODELE',IBID,REPK,IERD)
        IF(REPK.EQ.'OUI') THEN
           MILIEU = .TRUE.
        ELSE IF(REPK.EQ.'NON') THEN
           MILIEU = .FALSE.
        ENDIF

      ELSE
C
C      LA DIRECTION DE THETA EST CALCULEE, ON LA NORME
C
C  LEVRE SUPERIEURE
C
        IF (IELSUP.NE.0) THEN
          CALL GDIREC(NOMA,FOND,'LEVRESUP',NOMNO,ZK8(IADRNO),COORN,
     &                NBNOEU,DIRE4,MILIEU)
          CALL JEVEUO(DIRE4,'L',IDIRS)
          IF (IELINF.NE.0) THEN
C
C  LEVRE INFERIEURE
C
            CALL GDIREC(NOMA,FOND,'LEVREINF',NOMNO,ZK8(IADRNO),COORN,
     &              NBNOEU,DIRE5,MILIEU)
            CALL JEVEUO(DIRE5,'L',IDIRI)
C
C LES DIRECTIONS OBTENUES POUR CHAQUE LEVRE SONT MOYENNEES ET NORMEES
C
            DO 2 I=1,NBNOEU
              DIRX = ZR(IDIRI+(I-1)*3+1-1)
              DIRY = ZR(IDIRI+(I-1)*3+2-1)
              DIRZ = ZR(IDIRI+(I-1)*3+3-1)
              VECX = (ZR(IDIRS+(I-1)*3+1-1)+DIRX)/2
              VECY = (ZR(IDIRS+(I-1)*3+2-1)+DIRY)/2
              VECZ = (ZR(IDIRS+(I-1)*3+3-1)+DIRZ)/2
              NORME = SQRT(VECX*VECX + VECY*VECY + VECZ*VECZ)
              ZR(IN2+(I-1)*3+1-1) = VECX/NORME
              ZR(IN2+(I-1)*3+2-1) = VECY/NORME
              ZR(IN2+(I-1)*3+3-1) = VECZ/NORME
2           CONTINUE
          ELSE
            DO 22 I=1,NBNOEU
              DIRX = ZR(IDIRS+(I-1)*3+1-1)
              DIRY = ZR(IDIRS+(I-1)*3+2-1)
              DIRZ = ZR(IDIRS+(I-1)*3+3-1)
              NORME = SQRT(DIRX*DIRX + DIRY*DIRY + DIRZ*DIRZ)
              ZR(IN2+(I-1)*3+1-1) = DIRX/NORME
              ZR(IN2+(I-1)*3+2-1) = DIRY/NORME
              ZR(IN2+(I-1)*3+3-1) = DIRZ/NORME
22          CONTINUE
          ENDIF
        ELSE IF (IENORM.NE.0) THEN
          CALL DISMOI('F','ELEM_VOLU_QUAD',NOMO,'MODELE',IBID,REPK,IERD)
          IF(REPK.EQ.'OUI') THEN
             MILIEU = .TRUE.
          ELSE IF(REPK.EQ.'NON') THEN
             MILIEU = .FALSE.
          ENDIF
          CALL GDINOR(NORM,NBNOEU,IADNUM,COORN,IN2)
        ELSE
          CALL U2MESS('F','RUPTURE0_98')
        ENDIF
C
C  ON RECUPERE LES DIRECTIONS UTILISATEUR AUX EXTREMITES DU FOND
C
        IF (ITANOR.NE.0) THEN
          CALL JEVEUO(OBJOR,'L',IAORIG)
          VECX = ZR(IAORIG)
          VECY = ZR(IAORIG+1)
          VECZ = ZR(IAORIG+2)
          NORME = SQRT(VECX*VECX + VECY*VECY + VECZ*VECZ)
          ZR(IN2+1-1) = VECX/NORME
          ZR(IN2+2-1) = VECY/NORME
          ZR(IN2+3-1) = VECZ/NORME
        ENDIF
        IF (ITANEX.NE.0) THEN
          CALL JEVEUO(OBJEX,'L',IAEXTR)
          VECX = ZR(IAEXTR)
          VECY = ZR(IAEXTR+1)
          VECZ = ZR(IAEXTR+2)
          NORME = SQRT(VECX*VECX + VECY*VECY + VECZ*VECZ)
          ZR(IN2+3*(NBNOEU-1)+1-1) = VECX/NORME
          ZR(IN2+3*(NBNOEU-1)+2-1) = VECY/NORME
          ZR(IN2+3*(NBNOEU-1)+3-1) = VECZ/NORME
        ENDIF
C
      ENDIF
C
C ALLOCATION D UN OBJET INDICATEUR DU CHAMP THETA SUR GAMMO
C
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBEL,CHBID,IERD)
      INDICG = '&&COURON.INDIC        '
      CALL WKVECT(INDICG,'V V I',NBEL,INDIC)
C
C ALLOCATION DES OBJETS POUR STOCKER LE CHAMP_NO THETA ET LA DIRECTION
C TYPE CHAM_NO ( DEPL_R) AVEC PROFIL NOEUD CONSTANT (3 DDL)
C
      IF(THLAG2) THEN
        PAIR = .FALSE.
        NBNOS = NBNOEU
        IF (MILIEU) NBNOS = (NBNOEU+1)/2
        IF (MOD(NBNOS,2) .EQ.1) NDIMTE = (NBNOS+1)/2
        IF (MOD(NBNOS,2) .EQ.0) THEN
          NDIMTE = 1+NBNOS/2
          PAIR = .TRUE.
          IF (CONNEX) THEN
          CALL U2MESS('F','RUPTURE1_1')
          ENDIF
        END IF
      ELSEIF(THLAGR) THEN
        NDIMTE = NBNOEU
      ELSE
        NDIMTE = NBRE + 1
      ENDIF
C
      CALL WKVECT(RESU,'V V K24',NDIMTE+1,JRESU)
C
C CREATION DES NDIMTE+1 CHAMPS_NO ET VALEUR SUR GAMMA0
C
      DO 400 K = 1 , NDIMTE+1
        CALL CODENT ( K , 'D0' , KIORD )
        CHAMNO = RESU(1:8)//'_CHAM'//KIORD//'     '
        ZK24(JRESU+K-1) = CHAMNO
        CALL JEEXIN(CHAMNO(1:19)//'.DESC',IRET)
        CALL ASSERT(IRET.GE.0 .AND. IRET.LE.100)
        IF(IRET.EQ.0) THEN
           CALL JEDETR(CHAMNO(1:19)//'.DESC')
           CALL JEDETR(CHAMNO(1:19)//'.REFE')
           CALL JEDETR(CHAMNO(1:19)//'.VALE')
        ENDIF

C  .DESC
        CHAMNO(20:24) = '.DESC'
        CALL WKVECT(CHAMNO,'V V I',3,IDESC)
C
        CALL JEECRA(CHAMNO,'DOCU',0,'CHNO')
        CALL JENONU(JEXNOM('&CATA.GD.NOMGD','DEPL_R'),NUMA)
        ZI(IDESC+1-1) = NUMA
        ZI(IDESC+2-1) = -3
        ZI(IDESC+3-1) = 14
C
C  .REFE
        CHAMNO(20:24) = '.REFE'
        CALL WKVECT(CHAMNO,'V V K24',4,IREFE)
        ZK24(IREFE+1-1) = NOMA//'                '
C
C  .VALE
        CHAMNO(20:24) = '.VALE'
        CALL WKVECT(CHAMNO,'V V R',3*NBEL,ITHETA)
C
        IF(K.NE.(NDIMTE+1)) THEN

          IF(THLAG2) THEN

            DO 3 I=1,NBNOEU
              NUM    = ZI(IADNUM+I-1)
              ZR(ITHETA+(NUM-1)*3+1-1) = 0.D0
              ZR(ITHETA+(NUM-1)*3+2-1) = 0.D0
              ZR(ITHETA+(NUM-1)*3+3-1) = 0.D0
              ZI(INDIC+NUM-1) = 1
3           CONTINUE
            CALL GABSCU(NBNOEU,COORN,NOMNO,CHFOND,XL,ABSGAM)
            CALL JEVEUO(ABSGAM,'L',IADABS)
            IF (MILIEU) THEN
              KNO = 4*K-3
            ELSE
              KNO = 2*K-1
            ENDIF
            IF ((K. EQ. NDIMTE) .AND.  PAIR) THEN
              KNO = NBNOEU
            ENDIF
            IADRTT = IADRT3 + (K-1)*NBNOEU + KNO - 1
            ZR(IADRTT) = 1.D0
            IF (K. NE. 1) THEN
              IF (MILIEU) THEN
                S0 = ZR(IADABS+KNO-1)
                S1 = ZR(IADABS+KNO-1-4)
                ZR(IADRTT-1) = (ZR(IADABS+KNO-1-1)-S1)/(S0-S1)
                ZR(IADRTT-2) = (ZR(IADABS+KNO-1-2)-S1)/(S0-S1)
                ZR(IADRTT-3) = (ZR(IADABS+KNO-1-3)-S1)/(S0-S1)
                ZR(IADRTT-4) = (ZR(IADABS+KNO-1-4)-S1)/(S0-S1)
              ELSE
                S0 = ZR(IADABS+KNO-1)
                S1 = ZR(IADABS+KNO-1-2)
                ZR(IADRTT-1) = (ZR(IADABS+KNO-1-1)-S1)/(S0-S1)
              ENDIF
            ENDIF
            IF ((K.LT. NDIMTE) .OR.
     &          (K. EQ. (NDIMTE-1) .AND.  .NOT. PAIR)) THEN
              IF (MILIEU) THEN
                S0 = ZR(IADABS+KNO-1)
                S1 = ZR(IADABS+KNO-1+4)
                ZR(IADRTT+1) = (ZR(IADABS+KNO-1+1)-S1)/(S0-S1)
                ZR(IADRTT+2) = (ZR(IADABS+KNO-1+2)-S1)/(S0-S1)
                ZR(IADRTT+3) = (ZR(IADABS+KNO-1+3)-S1)/(S0-S1)
                ZR(IADRTT+4) = (ZR(IADABS+KNO-1+4)-S1)/(S0-S1)
              ELSE
                S0 = ZR(IADABS+KNO-1)
                S1 = ZR(IADABS+KNO-1+2)
                ZR(IADRTT+1) = (ZR(IADABS+KNO-1+1)-S1)/(S0-S1)
              ENDIF
            ENDIF
            IF (K. EQ. (NDIMTE-1) .AND. PAIR) THEN
              IF (MILIEU) THEN
                S0 = ZR(IADABS+KNO-1)
                S1 = ZR(IADABS+KNO-1+2)
                ZR(IADRTT+2) = 0.5D0
                ZR(IADRTT+1) =0.5D0*(1+(ZR(IADABS+KNO-1+1)-S1)/(S0-S1))
              ELSE
                ZR(IADRTT+1) = 0.5D0
              ENDIF
            ENDIF
            IF ((K. EQ. NDIMTE) .AND.  PAIR) THEN
              IF (MILIEU) THEN
                S0 = ZR(IADABS+KNO-1)
                S1 = ZR(IADABS+KNO-1-2)
                ZR(IADRTT) = 0.5D0
                ZR(IADRTT-1) =0.5D0*(ZR(IADABS+KNO-1-1)-S1)/(S0-S1)
                ZR(IADRTT-2) = 0.D0
                ZR(IADRTT-3) = 0.D0
              ELSE
                ZR(IADRTT) = 0.5D0
                ZR(IADRTT-1) = 0.D0
              ENDIF
            ENDIF
            IF ((K .EQ. 1) .AND. CONNEX)  THEN
              IADRTT = IADRT3 + (K-1)*NBNOEU + NBNOEU - 1
              IF (MILIEU) THEN
                S0 = ZR(IADABS+NBNOEU-1)
                S1 = ZR(IADABS+NBNOEU-1-4)
                ZR(IADRTT) = (ZR(IADABS+NBNOEU-1)-S1)/(S0-S1)
                ZR(IADRTT-1) = (ZR(IADABS+NBNOEU-1-1)-S1)/(S0-S1)
                ZR(IADRTT-2) = (ZR(IADABS+NBNOEU-1-2)-S1)/(S0-S1)
                ZR(IADRTT-3) = (ZR(IADABS+NBNOEU-1-3)-S1)/(S0-S1)
              ELSE
                S0 = ZR(IADABS+NBNOEU-1)
                S1 = ZR(IADABS+NBNOEU-1-2)
                ZR(IADRTT-1) = (ZR(IADABS+NBNOEU-1-1)-S1)/(S0-S1)
              ENDIF
            ENDIF
            IF ((K .EQ. NDIMTE) .AND. CONNEX)  THEN
              IADRTT = IADRT3 + (K-1)*NBNOEU + 1 - 1
              IF (MILIEU) THEN
                S0 = ZR(IADABS+1-1)
                S1 = ZR(IADABS+1-1+4)
                ZR(IADRTT) = (ZR(IADABS+1-1)-S1)/(S0-S1)
                ZR(IADRTT+1) = (ZR(IADABS+1-1+1)-S1)/(S0-S1)
                ZR(IADRTT+2) = (ZR(IADABS+1-1+2)-S1)/(S0-S1)
                ZR(IADRTT+3) = (ZR(IADABS+1-1+3)-S1)/(S0-S1)
              ELSE
                S0 = ZR(IADABS+KNO-1)
                S1 = ZR(IADABS+KNO-1+2)
                ZR(IADRTT+1) = (ZR(IADABS+KNO-1+1)-S1)/(S0-S1)
              ENDIF
            ENDIF
            I1 = 1
            IF (MILIEU) I1 = 3
            DO 4 I= (-1*I1), I1
              IF (.NOT. (((K. EQ. 1) .AND. (I. LT. 0))
     &        .OR.  ((K. EQ. NDIMTE) .AND. (I. GT. 0))
     &        .OR.  ((K. EQ. (NDIMTE-1)) .AND. (I. GT. 2) .AND. PAIR)))
     &       THEN
                NUM    = ZI(IADNUM+KNO-1+I)
                IADRTT = IADRT3 + (K-1)*NBNOEU + KNO-1 +I
                ZR(ITHETA+(NUM-1)*3+1-1) = ZR(IADRTT)
     &                          *ZR(IN2+(KNO-1+I)*3+1-1)
                ZR(ITHETA+(NUM-1)*3+2-1) = ZR(IADRTT)
     &                          *ZR(IN2+(KNO-1+I)*3+2-1)
                ZR(ITHETA+(NUM-1)*3+3-1) = ZR(IADRTT)
     &                          *ZR(IN2+(KNO-1+I)*3+3-1)
              ENDIF
4           CONTINUE
            IF (CONNEX .AND. ((K. EQ. 1) .OR. (K. EQ. NDIMTE)))  THEN
              IF (K. EQ. 1) KNO = NBNOEU
              IF (K. EQ. NDIMTE) KNO = 1
              DO 401 I= (-1*I1), I1
              IF (.NOT. (((K. EQ. 1) .AND. (I. GT. 0))
     &        .OR.  ((K. EQ. NDIMTE) .AND. (I. LT. 0))))
     &       THEN
                NUM    = ZI(IADNUM+KNO-1+I)
                IADRTT = IADRT3 + (K-1)*NBNOEU + KNO-1 +I
                ZR(ITHETA+(NUM-1)*3+1-1) = ZR(IADRTT)
     &                          *ZR(IN2+(KNO-1+I)*3+1-1)
                ZR(ITHETA+(NUM-1)*3+2-1) = ZR(IADRTT)
     &                          *ZR(IN2+(KNO-1+I)*3+2-1)
                ZR(ITHETA+(NUM-1)*3+3-1) = ZR(IADRTT)
     &                          *ZR(IN2+(KNO-1+I)*3+3-1)
              ENDIF
401           CONTINUE
            ENDIF
C
          ELSEIF(THLAGR) THEN
C
            DO 31 I=1,NBNOEU
              NUM    = ZI(IADNUM+I-1)
              ZR(ITHETA+(NUM-1)*3+1-1) = 0.D0
              ZR(ITHETA+(NUM-1)*3+2-1) = 0.D0
              ZR(ITHETA+(NUM-1)*3+3-1) = 0.D0
              ZI(INDIC+NUM-1) = 1
31           CONTINUE
            NUM    = ZI(IADNUM+K-1)
            IADRTT = IADRT3 + (K-1)*NBNOEU + K - 1
            ZR(IADRTT) = 1.D0
            ZR(ITHETA+(NUM-1)*3+1-1) = ZR(IADRTT)*ZR(IN2+(K-1)*3+1-1)
            ZR(ITHETA+(NUM-1)*3+2-1) = ZR(IADRTT)*ZR(IN2+(K-1)*3+2-1)
            ZR(ITHETA+(NUM-1)*3+3-1) = ZR(IADRTT)*ZR(IN2+(K-1)*3+3-1)
            IF (CONNEX.AND.(K.EQ.1)) THEN
              NUM    = ZI(IADNUM+NDIMTE-1)
              IADRTT = IADRT3 + (K-1)*NBNOEU + NDIMTE - 1
              ZR(IADRTT) = 1.D0
            ZR(ITHETA+(NUM-1)*3+1-1)=ZR(IADRTT)*ZR(IN2+(NDIMTE-1)*3+1-1)
            ZR(ITHETA+(NUM-1)*3+2-1)=ZR(IADRTT)*ZR(IN2+(NDIMTE-1)*3+2-1)
            ZR(ITHETA+(NUM-1)*3+3-1)=ZR(IADRTT)*ZR(IN2+(NDIMTE-1)*3+3-1)
            ENDIF
            IF (CONNEX.AND.(K.EQ.NDIMTE)) THEN
              NUM    = ZI(IADNUM+1-1)
              IADRTT = IADRT3 + (K-1)*NBNOEU + 1 - 1
              ZR(IADRTT) = 1.D0
              ZR(ITHETA+(NUM-1)*3+1-1) = ZR(IADRTT)*ZR(IN2+(1-1)*3+1-1)
              ZR(ITHETA+(NUM-1)*3+2-1) = ZR(IADRTT)*ZR(IN2+(1-1)*3+2-1)
              ZR(ITHETA+(NUM-1)*3+3-1) = ZR(IADRTT)*ZR(IN2+(1-1)*3+3-1)
            ENDIF
          ELSE
            DO 41 I=1,NBNOEU
              NUM    = ZI(IADNUM+I-1)
              IADRTT = IADRT3 + (K-1)*NBNOEU + I - 1
              ZR(ITHETA+(NUM-1)*3+1-1) = ZR(IADRTT)*ZR(IN2+(I-1)*3+1-1)
              ZR(ITHETA+(NUM-1)*3+2-1) = ZR(IADRTT)*ZR(IN2+(I-1)*3+2-1)
              ZR(ITHETA+(NUM-1)*3+3-1) = ZR(IADRTT)*ZR(IN2+(I-1)*3+3-1)
              ZI(INDIC+NUM-1) = 1
41           CONTINUE
          ENDIF
        ELSE
C     STOCKAGE DE LA DIRECTION DU CHAMPS THETA SUR LE FOND DE FISSURE
          DO 450 I=1,NBNOEU
            NUM    = ZI(IADNUM+I-1)
            ZR(ITHETA+(NUM-1)*3+1-1) = ZR(IN2+(I-1)*3+1-1)
            ZR(ITHETA+(NUM-1)*3+2-1) = ZR(IN2+(I-1)*3+2-1)
            ZR(ITHETA+(NUM-1)*3+3-1) = ZR(IN2+(I-1)*3+3-1)
450       CONTINUE
        ENDIF
400   CONTINUE
C
C         BOUCLE SUR LES NOEUDS M COURANTS DU MAILLAGE SANS GAMMO
C         POUR CALCULER PROJ(M)=N
C
      DO 500 I=1,NBEL
        IF(ZI(INDIC+I-1).NE.1) THEN
          XM = ZR(IADRCO+(I-1)*3+1-1)
          YM = ZR(IADRCO+(I-1)*3+2-1)
          ZM = ZR(IADRCO+(I-1)*3+3-1)
          DMIN = R8MAEM()
          JMIN = 0
          SMIN = 0.D0
          DO 600 J=1,NBNOEU-1
            XI1 = ZR(IADRCO+(ZI(IADNUM+J-1)-1)*3+1-1)
            YI1 = ZR(IADRCO+(ZI(IADNUM+J-1)-1)*3+2-1)
            ZI1 = ZR(IADRCO+(ZI(IADNUM+J-1)-1)*3+3-1)
            XJ1 = ZR(IADRCO+(ZI(IADNUM+J+1-1)-1)*3+1-1)
            YJ1 = ZR(IADRCO+(ZI(IADNUM+J+1-1)-1)*3+2-1)
            ZJ1 = ZR(IADRCO+(ZI(IADNUM+J+1-1)-1)*3+3-1)
            XIJ = XJ1-XI1
            YIJ = YJ1-YI1
            ZIJ = ZJ1-ZI1
            XIM = XM-XI1
            YIM = YM-YI1
            ZIM = ZM-ZI1
            S   = XIJ*XIM + YIJ*YIM + ZIJ*ZIM
            NORM2 = XIJ*XIJ + YIJ *YIJ + ZIJ*ZIJ
            S     = S/NORM2
            IF((S-1).GE.EPS) THEN
              S = 1.D0
            ENDIF
            IF(S.LE.EPS) THEN
              S = 0.D0
            ENDIF
            XN = S*XIJ+XI1
            YN = S*YIJ+YI1
            ZN = S*ZIJ+ZI1
            D = SQRT((XN-XM)*(XN-XM)+(YN-YM)*(YN-YM)+
     &                   (ZN-ZM)*(ZN-ZM))
            IF(D.LT.DMIN) THEN
              DMIN = D
              JMIN = J
              SMIN = S
            ENDIF
600       CONTINUE
          RII = (1-SMIN)*ZR(IADRT1+JMIN-1)+SMIN*ZR(IADRT1+JMIN+1-1)
          RSI = (1-SMIN)*ZR(IADRT2+JMIN-1)+SMIN*ZR(IADRT2+JMIN+1-1)
          ALPHA = (DMIN-RII)/(RSI-RII)
          DO 700 K = 1 , NDIMTE+1
          CALL CODENT ( K , 'D0' , KIORD )
          CHAMNO = RESU(1:8)//'_CHAM'//KIORD//'     '
          CHAMNO(20:24) = '.VALE'
          CALL JEVEUO(CHAMNO,'E',ITHETA)
            IF (K.NE.(NDIMTE+1)) THEN
              IADRTT = IADRT3+(K-1)*NBNOEU+JMIN-1
              TEI = ZR(IADRTT)
              TEJ = ZR(IADRTT+1)
              VALX = (1-SMIN)*ZR(IN2+(JMIN-1)*3+1-1)*TEI
              VALX = VALX+SMIN*ZR(IN2+(JMIN+1-1)*3+1-1)*TEJ
              VALY = (1-SMIN)*ZR(IN2+(JMIN-1)*3+2-1)*TEI
              VALY = VALY+SMIN*ZR(IN2+(JMIN+1-1)*3+2-1)*TEJ
              VALZ = (1-SMIN)*ZR(IN2+(JMIN-1)*3+3-1)*TEI
              VALZ = VALZ+SMIN*ZR(IN2+(JMIN+1-1)*3+3-1)*TEJ
C
              IF((ABS(ALPHA).LE.EPS).OR.(ALPHA.LT.0)) THEN
                ZR(ITHETA+(I-1)*3+1-1) = VALX
                ZR(ITHETA+(I-1)*3+2-1) = VALY
                ZR(ITHETA+(I-1)*3+3-1) = VALZ
              ELSE IF((ABS(ALPHA-1).LE.EPS).OR.((ALPHA-1).GT.0)) THEN
                ZR(ITHETA+(I-1)*3+1-1) = 0.D0
                ZR(ITHETA+(I-1)*3+2-1) = 0.D0
                ZR(ITHETA+(I-1)*3+3-1) = 0.D0
              ELSE
                ZR(ITHETA+(I-1)*3+1-1) = (1-ALPHA)*VALX
                ZR(ITHETA+(I-1)*3+2-1) = (1-ALPHA)*VALY
                ZR(ITHETA+(I-1)*3+3-1) = (1-ALPHA)*VALZ
              ENDIF
            ELSE
              ZR(ITHETA+(I-1)*3+1-1) = 0.D0
              ZR(ITHETA+(I-1)*3+2-1) = 0.D0
              ZR(ITHETA+(I-1)*3+3-1) = 0.D0
            ENDIF
700       CONTINUE
        ENDIF
500   CONTINUE
C
C
C DESTRUCTION D'OBJETS DE TRAVAIL
C
      CALL JEEXIN(DIRE4,IRET)
      IF(IRET.NE.0) CALL JEDETR(DIRE4)
      CALL JEEXIN(DIRE5,IRET)
      IF(IRET.NE.0) CALL JEDETR(DIRE5)
      CALL JEDETR(INDICG)
      CALL JEDETR(NUMGAM)
C
      CALL JEDEMA()
C
      END
