      SUBROUTINE GCOUR3 (RESU,NOMA,COORN,LNOFF,TRAV1,
     &           TRAV2,TRAV3,CHFOND,GRLT,THLAGR,THLAG2,
     &           BASFON,NBRE,MILIEU,PAIR,NDIMTE)
      IMPLICIT NONE


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/11/2012   AUTEUR TRAN V-X.TRAN 
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

C FONCTION REALISÉE:   DANS LE CADRE DE X-FEM
C
C 1.  POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
C     LE TRIPLET ( MODULE(THETA), RINF, RSUP )
C
C 2.  PUIS ON  CALCULE LA DIRECTION DES CHAMPS THETA

C
C 3.  ENSUITE ON CALCULE LES CHAMPS THETA SUR TOUS LES NOEUDS DU
C     MAILLAGE
C
C     ------------------------------------------------------------------
C ENTREE:
C        RESU   : NOM DU CONCEPT RESULTAT
C        NOMA   : NOM DU CONCEPT MAILLAGE
C        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DU MAILLAGE
C        LNOFF  : NOMBRE DE NOEUDS DE GAMM0
C        CHFOND : NOMS DES NOEUDS DU FOND DE FISSURE
C        GRLT   : GRADIENT DE LA LEVEL-SET TANGENTE
C        TRAV1  : RINF
C        TRAV2  : RSUP
C        THLAGR  : SI PRESENCE DU MOT CLE THETA_LAGRANGE
C        THLAG2  : SI PRESENCE DU MOT CLE THETA_LAGRANGE_REGU
C        BASFON  : BASE LOCALE AUX POINTS DU FOND DE FISSURE
C        NBRE   : DEGRE DES POLYNOMES DE LEGENDRE
C                     SINON 0
C SORTIE:
C                 LISTE DE CHAMPS_NO THETA
C        TRAV3 : MODULE(THETA)
C        MILIEU: .TRUE.  : ELEMENT QUADRATIQUE
C                .FALSE. : ELEMENT LINEAIRE
C     ------------------------------------------------------------------
C
C
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      CHARACTER*24      TRAV1,TRAV2,TRAV3,CHFOND,CHAMNO,COORN
      CHARACTER*24      BASFON
      CHARACTER*19      CNSGT, GRLT
      CHARACTER*8       CHBID,RESU, NOMA
      CHARACTER*6       KIORD
C
      INTEGER           LNOFF,IADRT1,IADRT2,IADRT3,ITHETA,IADRCO,JMIN
      INTEGER           IERD,IMODU,NBRE,IRET,NUMA,NDIMTE,JGT
      INTEGER           NBNO,IFON,I,IDESC,IREFE,J,JRESU,K,JGTL
C
      REAL*8            XI1,YI1,ZI1,XJ1,YJ1,ZJ1,R8PREM
      REAL*8            XIJ,YIJ,ZIJ,EPS,D, GRTX,GRTY,GRTZ
      REAL*8            XM,YM,ZM,XIM,YIM,ZIM,S,DMIN,SMIN,XN,YN,ZN
      REAL*8            RII,RSI,ALPHA,VALX,VALY,VALZ,NORM2,R8MAEM
      REAL*8            GRTX0,GRTY0,GRTZ0,GRTX1,GRTY1,GRTZ1
C
      LOGICAL           THLAGR,MILIEU, DEBUG,THLAG2,PAIR
C
C-----------------------------------------------------------------------
      INTEGER IADRTT ,JBAS ,KNO
      REAL*8 S0 ,S1
C-----------------------------------------------------------------------
      CALL JEMARQ()

      EPS = 1.D-12
      DEBUG=.FALSE.
      MILIEU=.FALSE.

      CALL JEVEUO(TRAV1,'L',IADRT1)
      CALL JEVEUO(TRAV2,'L',IADRT2)
      CALL JEVEUO(TRAV3,'E',IADRT3)

      CALL JEVEUO(COORN,'L',IADRCO)

      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,CHBID,IERD)

      CALL JEVEUO(CHFOND,'L',IFON)
      CALL JEVEUO(BASFON,'L',JBAS)

C          -----------------------

C     RÉCUPÉRATION DES GRADIENTS DE LST
      CNSGT='&&GCOUR3.CNSGT'
      CALL CNOCNS(GRLT,'V',CNSGT)
      CALL JEVEUO(CNSGT//'.CNSV','L',JGT)
      CALL JEVEUO(CNSGT//'.CNSL','L',JGTL)
C
C ALLOCATION DES OBJETS POUR STOCKER LE CHAMP_NO THETA ET LA DIRECTION
C TYPE CHAM_NO ( DEPL_R) AVEC PROFIL NOEUD CONSTANT (3 DDL)
C
      IF(THLAG2) THEN
        PAIR = .FALSE.
        IF (MOD(LNOFF,2) .EQ.1) NDIMTE = (LNOFF+1)/2
        IF (MOD(LNOFF,2) .EQ.0) THEN
          NDIMTE = 1+LNOFF/2
          PAIR = .TRUE.
        END IF
      ELSEIF(THLAGR) THEN
        NDIMTE = LNOFF
      ELSE
        NDIMTE = NBRE + 1
      ENDIF
C
      CALL WKVECT(RESU,'V V K24',NDIMTE+1,JRESU)

C BOUCLE GENERALE SUR LES NDIMTE+1 CHAMPS_NO A CREER
C
      DO 999 K = 1 , NDIMTE+1

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
        CALL WKVECT(CHAMNO,'V V R',3*NBNO,ITHETA)


C       VOIR RÉFÉRENCE BOOK I (05/01/2004)
        IF(K.NE.(NDIMTE+1)) THEN

          IF(THLAG2) THEN
            KNO = 2*K-1
            IF ((K. EQ. NDIMTE) .AND.  PAIR) THEN
              KNO = LNOFF
            ENDIF
            IADRTT = IADRT3 + (K-1)*LNOFF + KNO - 1
            ZR(IADRTT) = 1.D0
            IF (K. NE. 1) THEN
              S0 = ZR(IFON-1+4*(KNO-1)+4)
              S1 = ZR(IFON-1+4*(KNO-1-2)+4)
              ZR(IADRTT-1) = (ZR(IFON-1+4*(KNO-1-1)+4)-S1)/(S0-S1)
            ENDIF
            IF ((K.LT. (NDIMTE-1)) .OR.
     &          (K. EQ. (NDIMTE-1) .AND. .NOT. PAIR)) THEN
              S0 = ZR(IFON-1+4*(KNO-1)+4)
              S1 = ZR(IFON-1+4*(KNO-1+2)+4)
              ZR(IADRTT+1) = (ZR(IFON-1+4*(KNO-1+1)+4)-S1)/(S0-S1)
            ENDIF
            IF (K. EQ. (NDIMTE-1) .AND. PAIR) THEN
              ZR(IADRTT+1) = 0.5D0
            ENDIF
            IF ((K. EQ. NDIMTE) .AND.  PAIR) THEN
              ZR(IADRTT) = 0.5D0
              ZR(IADRTT-1) = 0.D0
            ENDIF
C
          ELSEIF(THLAGR)THEN
            ZR(IADRT3-1+(K-1)*LNOFF+K) = 1.D0
          ENDIF
C         BOUCLE SUR LES NOEUDS M COURANTS DU MAILLAGE
C         POUR CALCULER PROJ(M)=N
C
          DO 500 I=1,NBNO
              IF (DEBUG) WRITE(6,*)'NOEUD MAIL',I
C             COORD DU NOEUD M DU MAILLAGE
              XM = ZR(IADRCO+(I-1)*3+1-1)
              YM = ZR(IADRCO+(I-1)*3+2-1)
              ZM = ZR(IADRCO+(I-1)*3+3-1)
C             INITIALISATION
              DMIN = R8MAEM()
              JMIN = 0
              SMIN = 0.D0
C              BOUCLE SUR PT DE FONFIS (ALGO VOIR )
              DO 600 J=1,LNOFF-1
C               COORD PT I, ET J
                XI1 = ZR(IFON-1+4*(J-1)+1)
                YI1 = ZR(IFON-1+4*(J-1)+2)
                ZI1 = ZR(IFON-1+4*(J-1)+3)
                XJ1 = ZR(IFON-1+4*(J-1+1)+1)
                YJ1 = ZR(IFON-1+4*(J-1+1)+2)
                ZJ1 = ZR(IFON-1+4*(J-1+1)+3)
C               VECTEUR IJ ET IM
                XIJ = XJ1-XI1
                YIJ = YJ1-YI1
                ZIJ = ZJ1-ZI1
                XIM = XM-XI1
                YIM = YM-YI1
                ZIM = ZM-ZI1
C               PARAM S (PRODUIT SCALAIRE...)
                S   = XIJ*XIM + YIJ*YIM + ZIJ*ZIM
                NORM2 = XIJ*XIJ + YIJ *YIJ + ZIJ*ZIJ
                S     = S/NORM2
C               SI N=P(M) SORT DU SEGMENT
                IF((S-1).GE.EPS) THEN
                  S = 1.D0
                ENDIF
                IF(S.LE.EPS) THEN
                  S = 0.D0
                ENDIF
C               COORD DE N
                XN = S*XIJ+XI1
                YN = S*YIJ+YI1
                ZN = S*ZIJ+ZI1
C               DISTANCE MN
                D = SQRT((XN-XM)*(XN-XM)+(YN-YM)*(YN-YM)+
     &                   (ZN-ZM)*(ZN-ZM))
                IF(D.LT. (DMIN*(1-ABS(R8PREM())*100))) THEN
                  DMIN = D
                  JMIN = J
                  SMIN = S
                ENDIF
600           CONTINUE

              RII = (1-SMIN)*ZR(IADRT1+JMIN-1)+SMIN*ZR(IADRT1+JMIN+1-1)
              RSI = (1-SMIN)*ZR(IADRT2+JMIN-1)+SMIN*ZR(IADRT2+JMIN+1-1)
              ALPHA = (DMIN-RII)/(RSI-RII)

              IF((ABS(ALPHA-1).LE.EPS).OR.((ALPHA-1).GT.0)) THEN
                ZR(ITHETA+(I-1)*3+1-1) = 0.D0
                ZR(ITHETA+(I-1)*3+2-1) = 0.D0
                ZR(ITHETA+(I-1)*3+3-1) = 0.D0
              ELSE
                IF (ZL(JGTL-1+(I-1)*3+1)) THEN
                  IMODU = IADRT3+(K-1)*LNOFF+JMIN-1
                  GRTX=ZR(JGT-1+(I-1)*3+1)
                  GRTY=ZR(JGT-1+(I-1)*3+2)
                  GRTZ=ZR(JGT-1+(I-1)*3+3)
                  VALX =((1-SMIN) * ZR(IMODU) + SMIN * ZR(IMODU+1))*GRTX
                  VALY =((1-SMIN) * ZR(IMODU) + SMIN * ZR(IMODU+1))*GRTY
                  VALZ =((1-SMIN) * ZR(IMODU) + SMIN * ZR(IMODU+1))*GRTZ
                  IF((ABS(ALPHA).LE.EPS).OR.(ALPHA.LT.0)) THEN
                    ZR(ITHETA+(I-1)*3+1-1) = VALX
                    ZR(ITHETA+(I-1)*3+2-1) = VALY
                    ZR(ITHETA+(I-1)*3+3-1) = VALZ
                  ELSE
                    ZR(ITHETA+(I-1)*3+1-1) = (1-ALPHA)*VALX
                    ZR(ITHETA+(I-1)*3+2-1) = (1-ALPHA)*VALY
                    ZR(ITHETA+(I-1)*3+3-1) = (1-ALPHA)*VALZ
                  ENDIF

C                 CORRECTION DE LA DIRECTION A L ORIGINE
                  IF (JMIN .EQ. 1) THEN
                    GRTX0=ZR(JBAS+4-1)* ZR(IMODU)
                    GRTY0=ZR(JBAS+5-1)* ZR(IMODU)
                    GRTZ0=ZR(JBAS+6-1)* ZR(IMODU)
                    GRTX1=ZR(JBAS+(2-1)*6+4-1)* ZR(IMODU+1)
                    GRTY1=ZR(JBAS+(2-1)*6+5-1)* ZR(IMODU+1)
                    GRTZ1=ZR(JBAS+(2-1)*6+6-1)* ZR(IMODU+1)
                    VALX =((1-SMIN) * GRTX0 + SMIN * GRTX1)
                    VALY =((1-SMIN) * GRTY0 + SMIN * GRTY1)
                    VALZ =((1-SMIN) * GRTZ0 + SMIN * GRTZ1)
                    IF((ABS(ALPHA).LE.EPS).OR.(ALPHA.LT.0)) THEN
                      IF (K. EQ. 1) THEN
                      ENDIF
                      ZR(ITHETA+(I-1)*3+1-1) = VALX
                      ZR(ITHETA+(I-1)*3+2-1) = VALY
                      ZR(ITHETA+(I-1)*3+3-1) = VALZ
                    ELSE
                      ZR(ITHETA+(I-1)*3+1-1) = (1-ALPHA)*VALX
                      ZR(ITHETA+(I-1)*3+2-1) = (1-ALPHA)*VALY
                      ZR(ITHETA+(I-1)*3+3-1) = (1-ALPHA)*VALZ
                    ENDIF
                  ENDIF

C                 CORRECTION DE LA DIRECTION A L ETREMITE
                  IF (JMIN .EQ. (LNOFF-1)) THEN
                    GRTX0=ZR(JBAS+(LNOFF-1-1)*6+4-1)* ZR(IMODU)
                    GRTY0=ZR(JBAS+(LNOFF-1-1)*6+5-1)* ZR(IMODU)
                    GRTZ0=ZR(JBAS+(LNOFF-1-1)*6+6-1)* ZR(IMODU)
                    GRTX1=ZR(JBAS+(LNOFF-1)*6+4-1)* ZR(IMODU+1)
                    GRTY1=ZR(JBAS+(LNOFF-1)*6+5-1)* ZR(IMODU+1)
                    GRTZ1=ZR(JBAS+(LNOFF-1)*6+6-1)* ZR(IMODU+1)
                    VALX =((1-SMIN) * GRTX0 + SMIN * GRTX1)
                    VALY =((1-SMIN) * GRTY0 + SMIN * GRTY1)
                    VALZ =((1-SMIN) * GRTZ0 + SMIN * GRTZ1)
                    IF((ABS(ALPHA).LE.EPS).OR.(ALPHA.LT.0)) THEN
                      IF (K. EQ. 1) THEN
                      ENDIF
                      ZR(ITHETA+(I-1)*3+1-1) = VALX
                      ZR(ITHETA+(I-1)*3+2-1) = VALY
                      ZR(ITHETA+(I-1)*3+3-1) = VALZ
                    ELSE
                      ZR(ITHETA+(I-1)*3+1-1) = (1-ALPHA)*VALX
                      ZR(ITHETA+(I-1)*3+2-1) = (1-ALPHA)*VALY
                      ZR(ITHETA+(I-1)*3+3-1) = (1-ALPHA)*VALZ
                    ENDIF
                  ENDIF
                  
                ENDIF
              ENDIF
 500      CONTINUE
        ENDIF

999   CONTINUE
C
      CALL JEDEMA()

      END
