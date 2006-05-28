      SUBROUTINE GCOUR3 (RESU,NOMA,NOMO,NOMNO,COORN,LNOFF,TRAV1,
     &           TRAV2,TRAV3,CHFOND,GRLT,DIREC,CONNEX,THLAGR,
     &           NBRE,MILIEU)
       IMPLICIT REAL*8 (A-H,O-Z)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/05/2006   AUTEUR GALENNE E.GALENNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE CIBHHLV L.VIVAN

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
C        NOMO   : NOM DU CONCEPT MODELE
C        NOMNO  : NOM DE L'OBJET CONTENANT LES NOEUDS DU MAILLAGE
C        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DU MAILLAGE
C        LNOFF  : NOMBRE DE NOEUDS DE GAMM0
C        CHFOND : NOMS DES NOEUDS DU FOND DE FISSURE
C        GRLT   : GRADIENT DE LA LEVEL-SET TANGENTE
C        DIREC  : DIRECTION CALCULEE SI DIREC=.FALSE.
C                       APPEL A GDIREC
C        TRAV1  : RINF
C        TRAV2  : RSUP
C        THLAGR  : SI PRESENCE DU MOT CLE THETA_LOCAL
C        NBRE   : DEGRE DES POLYNOMES DE LEGENDRE
C                     SINON 0
C        CONNEX: .TRUE.  : FOND DE FISSURE FERME
C                .FALSE. : FOND DE FISSURE DEBOUCHANT
C SORTIE:
C                 LISTE DE CHAMPS_NO THETA
C        TRAV3 : MODULE(THETA)
C        MILIEU: .TRUE.  : ELEMENT QUADRATIQUE
C                .FALSE. : ELEMENT LINEAIRE
C     ------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      CHARACTER*1 K1BID
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*24      TRAV1,TRAV2,TRAV3,CHFOND,CHAMNO,COORN,NOMNO
C      CHARACTER*24      COORN,NOMNO
      CHARACTER*19      CNSGT, GRLT
      CHARACTER*8       CHBID, FISS,RESU, NOMA, NOMO
      CHARACTER*6       KIORD
C
      INTEGER           LNOFF,IADRT1,IADRT2,IADRT3,ITHETA,IADRCO,JMIN
      INTEGER           IERD,IMODU,NBRE,IRET,NUMA,NDIMTE,JGT,JGN,TMP
      INTEGER           NBNO,IFON,I,IDESC,IREFE,J,JRESU,K
C
      REAL*8            XI1,YI1,ZI1,XJ1,YJ1,ZJ1
      REAL*8            XIJ,YIJ,ZIJ,EPS,D, GRTX,GRTY,GRTZ
      REAL*8            XM,YM,ZM,XIM,YIM,ZIM,S,DMIN,SMIN,XN,YN,ZN
      REAL*8            RII,RSI,ALPHA,VALX,VALY,VALZ,NORM2,R8MAEM
C
      LOGICAL           DIREC,THLAGR,MILIEU,CONNEX, DEBUG
C
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

C     RÉCUPÉRATION DES GRADIENTS DE LST
      CNSGT='&&GCOUR3.CNSGT'
      CALL CNOCNS(GRLT,'V',CNSGT)
      CALL JEVEUO(CNSGT//'.CNSV','L',JGT)
C
C ALLOCATION DES OBJETS POUR STOCKER LE CHAMP_NO THETA ET LA DIRECTION
C TYPE CHAM_NO ( DEPL_R) AVEC PROFIL NOEUD CONSTANT (3 DDL)
C
      IF(THLAGR) THEN
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
        IF(IRET.EQ.0) THEN
           CALL JEDETR(CHAMNO(1:19)//'.DESC')
           CALL JEDETR(CHAMNO(1:19)//'.REFE')
           CALL JEDETR(CHAMNO(1:19)//'.VALE')
        ELSE IF(IRET.GT.100) THEN
           CALL UTMESS('A','GCOUR3','APPEL ERRONE')
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
        CALL WKVECT(CHAMNO,'V V K24',2,IREFE)
        ZK24(IREFE+1-1) = NOMA//'                '
C
C  .VALE
        CHAMNO(20:24) = '.VALE'
        CALL WKVECT(CHAMNO,'V V R',3*NBNO,ITHETA)


C       VOIR RÉFÉRENCE BOOK I (05/01/2004)
        IF(K.NE.(NDIMTE+1)) THEN

          IF(THLAGR)   ZR(IADRT3-1+(K-1)*LNOFF+K) = 1.D0

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
                IF(D.LT.DMIN) THEN
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
                IMODU = IADRT3+(K-1)*LNOFF+JMIN-1
                GRTX=ZR(JGT-1+(I-1)*3+1)
                GRTY=ZR(JGT-1+(I-1)*3+2)
                GRTZ=ZR(JGT-1+(I-1)*3+3)          
                VALX = ((1-SMIN) * ZR(IMODU) + SMIN * ZR(IMODU+1))*GRTX
                VALY = ((1-SMIN) * ZR(IMODU) + SMIN * ZR(IMODU+1))*GRTY
                VALZ = ((1-SMIN) * ZR(IMODU) + SMIN * ZR(IMODU+1))*GRTZ
                IF((ABS(ALPHA).LE.EPS).OR.(ALPHA.LT.0)) THEN
                  ZR(ITHETA+(I-1)*3+1-1) = VALX
                  ZR(ITHETA+(I-1)*3+2-1) = VALY
                  ZR(ITHETA+(I-1)*3+3-1) = VALZ
                ELSE 
                  ZR(ITHETA+(I-1)*3+1-1) = (1-ALPHA)*VALX
                  ZR(ITHETA+(I-1)*3+2-1) = (1-ALPHA)*VALY
                  ZR(ITHETA+(I-1)*3+3-1) = (1-ALPHA)*VALZ
                ENDIF
              ENDIF
 500      CONTINUE
        ENDIF

999   CONTINUE

      CALL JEDEMA()

      END
