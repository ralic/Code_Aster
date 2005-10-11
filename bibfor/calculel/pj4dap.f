      SUBROUTINE PJ4DAP(INO2,GEOM2,MA2,GEOM1,TRIA3,
     &                COBARY,ITR3,NBTROU,
     &  BTDI, BTVR, BTNB, BTLC,BTCO,IFM,NIV,
     &  LDMAX,DISTMA)
      IMPLICIT NONE
      REAL*8  COBARY(3),GEOM1(*),GEOM2(*),BTVR(*)
      INTEGER ITR3,NBTROU,BTDI(*),BTNB(*),BTLC(*),BTCO(*)
      INTEGER TRIA3(*),IFM,NIV
      CHARACTER*8   MA2
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/10/2005   AUTEUR VABHHTS J.PELLET 
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
C     BUT :
C       TROUVER LE TRIA3 QUI SERVIRA A INTERPOLER LE NOEUD INO2
C       AINSI QUE LES COORDONNEES BARYCENTRIQUES DE INO2 DANS CE TRIA3
C
C  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
C  IN   MA2        K8 : NOM DU MAILLAGE M2
C  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
C  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
C  IN   TRIA3(*)   I  : OBJET '&&PJXXCO.TRIA3'
C  IN   BTDI(*)    I  : OBJET .BT3DDI DE LA SD BOITE_3D
C  IN   BTVR(*)    R  : OBJET .BT3DVR DE LA SD BOITE_3D
C  IN   BTNB(*)    I  : OBJET .BT3DNB DE LA SD BOITE_3D
C  IN   BTLC(*)    I  : OBJET .BT3DLC DE LA SD BOITE_3D
C  IN   BTCO(*)    I  : OBJET .BT3DCO DE LA SD BOITE_3D
C  IN   IFM        I  : NUMERO LOGIQUE DU FICHIER MESSAGE
C  IN   NIV        I  : NIVEAU D'IMPRESSION POUR LES "INFO"
C  OUT  NBTROU     I  : NOMBRE DE TRIA3 SOLUTIONS
C  OUT  ITR3       I  : NUMERO D'UN TRIA3 SOLUTION
C  OUT  COBARY(3)  R  : COORDONNEES BARYCENTRIQUES DE INO2 DANS ITR3
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      REAL*8  COBAR2(3),DMIN,D2,R8MAEM,SURF,RTR3
      INTEGER P,Q,R,P1,Q1,P2,Q2,R1,R2,INO2,I,K,IPOSI,NX,NY,NTRBT,IBID
      CHARACTER*8   NONO2,ALARME
      CHARACTER*16 K16BID,NOMCMD

      LOGICAL LDMAX,LOIN
      REAL*8  DISTMA
C DEB ------------------------------------------------------------------
C     NTR3=TRIA3(1)
      NBTROU=0

      NX=BTDI(1)
      NY=BTDI(2)


C     --  ON CHERCHE LE TRIA3 ITR3 LE PLUS PROCHE DE INO2 :
C     ------------------------------------------------------
        IF ( LDMAX ) THEN
          DMIN = DISTMA
        ELSE
          DMIN = R8MAEM()
        ENDIF

C       -- ON RECHERCHE LA GROSSE BOITE CANDIDATE :
        CALL PJ3DGB(INO2,GEOM2,GEOM1,TRIA3,4,
     &              BTDI, BTVR, BTNB, BTLC,BTCO,
     &              P1,Q1,R1,P2,Q2,R2)
        DO 21,P=P1,P2
          DO 22,Q=Q1,Q2
            DO 23,R=R1,R2
              NTRBT=BTNB((R-1)*NX*NY+(Q-1)*NX+P)
              IPOSI=BTLC((R-1)*NX*NY+(Q-1)*NX+P)
              DO 2,K=1,NTRBT
                 I=BTCO(IPOSI+K)
                 CALL PJ4DA2(INO2,GEOM2,I,GEOM1,TRIA3,COBAR2,D2,SURF)
                 IF (D2.LT.DMIN) THEN
                   RTR3=SURF
                   ITR3=I
                   DMIN=D2
                   NBTROU=1
                   COBARY(1)=COBAR2(1)
                   COBARY(2)=COBAR2(2)
                   COBARY(3)=COBAR2(3)
                 END IF
2             CONTINUE
23          CONTINUE
22        CONTINUE
21      CONTINUE


C       S'IL N'Y A PAS DE DISTANCE MINIMALE IMPOSEE, LE NOEUD EST
C          OBLIGATOIREMENT PROJETE
C       SI LE NOEUD EST PROJETE SUR UNE MAILLE LOINTAINE, ON INFORME :
C       --------------------------------------------------------------
        IF ((NIV.GT.0).AND.(.NOT.LDMAX)) THEN
          ALARME='OUI'
          CALL GETRES(K16BID,K16BID,NOMCMD)
          IF (NOMCMD.EQ.'PROJ_CHAMP') THEN
             CALL GETVTX(' ','ALARME',1,0,1,ALARME,IBID)
          ENDIF

          IF (ALARME.EQ.'OUI') THEN
            LOIN=.FALSE.
            IF (RTR3.EQ.0) THEN
              LOIN=.TRUE.
            ELSE
              IF (DMIN/RTR3.GT.1.D-1) LOIN=.TRUE.
            END IF

            IF (LOIN) THEN
              CALL JENUNO(JEXNUM(MA2//'.NOMNOE',INO2),NONO2)
              WRITE(IFM,*)'<PROJCH> LE NOEUD :',NONO2,
     &          ' EST PROJETE SUR UNE MAILLE DISTANTE.'
              WRITE (IFM,*) '           DISTANCE              = ',DMIN
              WRITE (IFM,*) '           DIAMETRE DE LA MAILLE = ',RTR3
              WRITE (IFM,*)
            END IF
          END IF
        END IF


      END
