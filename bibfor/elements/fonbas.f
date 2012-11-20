      SUBROUTINE FONBAS( NOMA,BASFON,FONTYP,FONFIS,NBNOFF,BASLOC,LNNO,
     &                   LTNO )

      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER          NBNOFF
      CHARACTER*8      NOMA
      CHARACTER*19     BASFON,BASLOC,FONTYP,LNNO,LTNO
      CHARACTER*24     FONFIS
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/11/2012   AUTEUR TRAN V-X.TRAN 
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
C
C FONCTION REALISEE:
C
C     CALCUL :
C        - DE LA BASE LOCALE EN CHAQUE NOEUD DU MAILLAGE
C        - DES LEVEL-SETS    EN CHAQUE NOEUD DU MAILLAGE
C
C
C     ENTREES:
C        NOMA   : NOM DU MAILLAGE
C        BASFON : BASE AUX NOEUDS DU FOND DE FISSURE
C        FONTYP : TYPE DU FOND DE FISSURE
C        FONFIS : COORDONNEES DES NOEUDS DU FOND DE FISSURE
C        NBNOFF : NOMBRE DE NOEUDS AU FOND DE FISSURE
C     SORTIES:
C        BASLOC : BASE LOCALE EN CHAQUE NOEUD DU MAILLAGE
C        LTNO   : LEVEL-SETS TANGENTS EN CHAQUE NOEUD DU MAILLAGE
C        LNNO   : LEVLE-SETS NORMAUX EN CHAQUE NOEUD DU MAILLAGE
C-----------------------------------------------------------------------
C
      INTEGER       IBID, IFON, INDICA, INDICB, INA, INB, INO
      INTEGER       IRET, ISEG, JBAS, JCOOR
      INTEGER       JGSL, JGSV, JLNSV, JLNSL, JLTSV, JLTSL, JTYP
      INTEGER       K, NBNO, NDIM, NSEG
      REAL*8        D, DMIN, EPS, NORM2, R8MAEM, S, SN, XLN, XLT
      REAL*8        XA, YA, ZA, XB, YB, ZB, XM, YM, ZM
      REAL*8        XAB, YAB, ZAB, XAM, YAM, ZAM, XNM, YNM, ZNM
      REAL*8        N(3), NM(3), VDIRA(3), VNORA(3), VDIRB(3), VNORB(3)
      REAL*8        VDIRN(3), VNORN(3),R8PREM
      CHARACTER*8   K8B, LICMP(9), TYPFON
      CHARACTER*16  CASFON
      CHARACTER*19  CNSBAS, CNSLN, CNSLT

      DATA LICMP / 'X1','X2','X3',
     &             'X4','X5','X6',
     &             'X7','X8','X9'/

C     -----------------------------------------------------------------
C
      CALL JEMARQ()

C     ------------------------------------------------------------------
C     INITIALISATIONS
C     ------------------------------------------------------------------
C
C     RECUPERATION DES INFORMATIONS RELATIVES AU MAILLAGE
      CALL DISMOI('F','DIM_GEOM',NOMA,'MAILLAGE',NDIM,K8B,IRET)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
C
C     NSEG : NOMBRE DE "SEGMENTS" DU FOND A TRAITER
      IF (NDIM.EQ.2) THEN
        NSEG = 1
        CASFON = ' '
      ELSEIF (NDIM.EQ.3) THEN
        CALL JEVEUO (FONTYP,'L',JTYP)
        TYPFON = ZK8(JTYP)
        CASFON = 'LINEAIRE'
             NSEG =  NBNOFF-1
C       CAS QUADRATIQUE
        IF (TYPFON.EQ.'NOE3'.OR.TYPFON.EQ.'SEG3') THEN
          CASFON = 'QUADRATIQUE'
               NSEG = (NBNOFF-1)/2
        ENDIF 
      ENDIF

C     INITIALISATION DES CHAMPS SIMPLES DES LEVEL-SETS
      CNSLT  = '&&FONBAS.CNSLT'
      CNSLN  = '&&FONBAS.CNSLN'
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSLT)
      CALL CNSCRE(NOMA,'NEUT_R',1,'X1','V',CNSLN)

      CALL JEVEUO(CNSLT//'.CNSV','E',JLTSV)
      CALL JEVEUO(CNSLT//'.CNSL','E',JLTSL)
      CALL JEVEUO(CNSLN//'.CNSV','E',JLNSV)
      CALL JEVEUO(CNSLN//'.CNSL','E',JLNSL)

C     INITIALISATION DU CHAMP SIMPLE DE LA BASE LOCALE
      CNSBAS = '&&FONBAS.CNSBAS'
      CALL CNSCRE(NOMA,'NEUT_R',NDIM*3,LICMP,'V',CNSBAS)
      CALL JEVEUO(CNSBAS//'.CNSV','E',JGSV)
      CALL JEVEUO(CNSBAS//'.CNSL','E',JGSL)

      CALL JEVEUO(FONFIS,'L',IFON)
      CALL JEVEUO(BASFON,'L',JBAS)

C     ------------------------------------------------------------------
C                BOUCLE SUR LES NOEUDS DU MAILLAGE
C     ------------------------------------------------------------------
C
      EPS = 1.D-12
      DO 10 INO=1,NBNO

C       COORD DU NOEUD M DU MAILLAGE
        XM = ZR(JCOOR+(INO-1)*3+1-1)
        YM = ZR(JCOOR+(INO-1)*3+2-1)
        ZM = ZR(JCOOR+(INO-1)*3+3-1)

C       CAS 2D : LE PROJETE EST TRIVIAL !
        IF (NDIM.EQ.2) THEN

C         COORD PT N
          N(1) = ZR(IFON)
          N(2) = ZR(IFON+1)
          N(3) = 0.D0

C         VECTEUR NM
          NM(1) = XM-N(1)
          NM(2) = YM-N(2)

C         STOCKAGE DES VECTEURS DE LA BASE
          DO 110 K=1,NDIM
            ZR(JGSV-1+3*NDIM*(INO-1)+K)   = N(K)
            ZL(JGSL-1+3*NDIM*(INO-1)+K)   = .TRUE.
            ZR(JGSV-1+3*NDIM*(INO-1)+K+2) = ZR(JBAS-1+K)
            ZL(JGSL-1+3*NDIM*(INO-1)+K+2) = .TRUE.
            ZR(JGSV-1+3*NDIM*(INO-1)+K+4) = ZR(JBAS-1+K+NDIM)
            ZL(JGSL-1+3*NDIM*(INO-1)+K+4) = .TRUE.
 110      CONTINUE

C         STOCKAGE DES LEVEL-SETS
          ZR(JLNSV-1+(INO-1)+1)=NM(1)*ZR(JBAS-1+1)+NM(2)*ZR(JBAS-1+2)
          ZR(JLTSV-1+(INO-1)+1)=NM(1)*ZR(JBAS-1+3)+NM(2)*ZR(JBAS-1+4)
          ZL(JLNSL-1+(INO-1)+1)=.TRUE.
          ZL(JLTSL-1+(INO-1)+1)=.TRUE.

C       CAS 3D : RECHERCHE DU PROJETE PUIS STOCKAGE DES VECTEURS
        ELSEIF (NDIM.EQ.3)THEN

C         RECHERCHE DU PROJETE DE INO SUR LE FOND DE FISSURE
C         --------------------------------------------------
          DMIN = R8MAEM()

C         BOUCLE SUR LES "SEGMENTS" DU FOND DE FISSURE
          DO 100 ISEG=1,NSEG

            IF (CASFON.EQ.'LINEAIRE') THEN
              INA = ISEG
              INB = ISEG+1
            ELSEIF (CASFON.EQ.'QUADRATIQUE') THEN
              INA = 2*ISEG-1
              INB = 2*ISEG+1
            ENDIF

C           COORD DES POINTS A ET B, EXTREMITES DU SEGMENT ISEG
            XA = ZR(IFON-1+4*(INA-1)+1)
            YA = ZR(IFON-1+4*(INA-1)+2)
            ZA = ZR(IFON-1+4*(INA-1)+3)
            XB = ZR(IFON-1+4*(INB-1)+1)
            YB = ZR(IFON-1+4*(INB-1)+2)
            ZB = ZR(IFON-1+4*(INB-1)+3)

C           VECTEUR AB ET AM
            XAB = XB-XA
            YAB = YB-YA
            ZAB = ZB-ZA
            XAM = XM-XA
            YAM = YM-YA
            ZAM = ZM-ZA

C           PARAM S (PRODUIT SCALAIRE...)
            S     = XAB*XAM + YAB*YAM + ZAB*ZAM
            NORM2 = XAB*XAB + YAB*YAB + ZAB*ZAB
            S     = S/NORM2

C           SI N EN DEHORS DU SEGMENT AB
            IF ((S-1).GE.EPS) S = 1.D0
            IF (S.LE.EPS)     S = 0.D0

C           COORD DU PROJETE DE M SUR ISEG: N
            XNM = XM - (S*XAB+XA)
            YNM = YM - (S*YAB+YA)
            ZNM = ZM - (S*ZAB+ZA)

C           DISTANCE MN
            D = SQRT(XNM*XNM + YNM*YNM + ZNM*ZNM)

            IF (D.LT.(DMIN*(1-ABS(R8PREM())*100) )) THEN
              DMIN   = D
              SN     = S
              INDICA = INA
              INDICB = INB

              N(1) = S*XAB+XA
              N(2) = S*YAB+YA
              N(3) = S*ZAB+ZA
            ENDIF

 100      CONTINUE

C         CALCUL DES VECTEURS DE LA BASE LOCALE AU POINT PROJETE
C         ------------------------------------------------------

          NM(1) = XM-N(1)
          NM(2) = YM-N(2)
          NM(3) = ZM-N(3)

          DO 200 K=1,NDIM

            VNORA(K) = ZR(JBAS-1+6*(INDICA-1)+K)
            VDIRA(K) = ZR(JBAS-1+6*(INDICA-1)+K+NDIM)
            VNORB(K) = ZR(JBAS-1+6*(INDICB-1)+K)
            VDIRB(K) = ZR(JBAS-1+6*(INDICB-1)+K+NDIM)
            VNORN(K) = SN*VNORB(K)+(1-SN)*VNORA(K)
            VDIRN(K) = SN*VDIRB(K)+(1-SN)*VDIRA(K)

C           STOCKAGE DU PROJETE ET DES GRADIENTS
            ZR(JGSV-1+3*NDIM*(INO-1)+K)   = N(K)
            ZL(JGSL-1+3*NDIM*(INO-1)+K)   = .TRUE.
            ZR(JGSV-1+3*NDIM*(INO-1)+K+3) = VDIRN(K)
            ZL(JGSL-1+3*NDIM*(INO-1)+K+3) = .TRUE.
            ZR(JGSV-1+3*NDIM*(INO-1)+K+6) = VNORN(K)
            ZL(JGSL-1+3*NDIM*(INO-1)+K+6) = .TRUE.

 200      CONTINUE

C         STOCKAGE DES LEVEL-SETS
          XLN = NM(1)*VNORN(1)+NM(2)*VNORN(2)+NM(3)*VNORN(3)
          XLT = NM(1)*VDIRN(1)+NM(2)*VDIRN(2)+NM(3)*VDIRN(3)
          ZR(JLNSV-1+(INO-1)+1) = XLN
          ZR(JLTSV-1+(INO-1)+1) = XLT

          ZL(JLNSL-1+(INO-1)+1) = .TRUE.
          ZL(JLTSL-1+(INO-1)+1) = .TRUE.

C       CAS NI 2D NI 3D
        ELSE
        
          CALL ASSERT(.FALSE.)
        
        ENDIF

 10   CONTINUE

C
C --- CREATION DES CHAM_NO
C
C     ENREGISTREMENT DE .LTNO, .LNNO ET .BASLOC DANS LA SD FOND_FISS
      CALL CNSCNO(CNSLT,' ','NON','G',LTNO,'F',IBID)
      CALL CNSCNO(CNSLN,' ','NON','G',LNNO,'F',IBID)
      CALL CNSCNO(CNSBAS,' ','NON','G',BASLOC,'F',IBID)


C     MENAGE
      CALL DETRSD('CHAM_NO_S',CNSLN)
      CALL DETRSD('CHAM_NO_S',CNSLT)
      CALL DETRSD('CHAM_NO_S',CNSBAS)

      CALL JEDEMA()
      END
