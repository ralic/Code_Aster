      SUBROUTINE ARLAPH(MAIL,NOM1Z,NOMCZ,APH0,APH)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C ----------------------------------------------------------------------
C        ATTRIBUTION DES PONDERATIONS ELEMENTAIRES POUR ARLEQUIN
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C CHARACTER*8       MAIL       : SD MAILLAGE
C CHARACTER*(10)    NOM1Z      : SD DOMAINE MECANIQUE
C CHARACTER*(10)    NOMCZ      : SD DOMAINE DE COLLAGE
C REAL*8            APH0       : POIDS A EFFECTER AUX MAILLES
C                                EN VIS-A-VIS DE NOM1
C
C VARIABLE DE ENTREE/SORTIE
C REAL*8            APH(*)     : VECTEUR PONDERATION DES MAILLES
C                               (POIDS MA.1, POIDS MA.2, ...)
C
C SD D'ENTREE
C NOM1.GROUPEMA      : LISTE DE MAILLES DOMAINE MECANIQUE
C NOM1.BOITE         : SD BOITES ENGLOBANTES (CF BOITE)
C NOM1.GRMAMA        : SD GRAPHE MAILLE -> MAILLES VOISINES (CF GRMAMA)
C NOMC.GROUPEMA.BORD : LISTE DE MAILLES DU BORD DU DOMAINE 
C                      DE COLLAGE (CF LBORD)
C NOMC.BOITE         : SD BOITES ENGLOBANTES (CF BOITE)
C NOMC.GRMAMA        : SD GRAPHE MAILLE -> MAILLES VOISINES (CF GRMAMA)
C NOMC.NOM1          : SD GRAPHE D'APPARIEMENT (CF ARLAPP)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C --- PARAMETRES
      REAL*8        R8MAX
      PARAMETER     (R8MAX = 1.D92)

C --- VARIABLES
      CHARACTER*8   MAIL
      CHARACTER*(*) NOM1Z,NOMCZ
      CHARACTER*10  NOM1,NOMC
      INTEGER       DIM,NMB,NMC,NM1,NCMP,NPILE,CPI,MA1,MA2,I,J,K,P0
      INTEGER       B0,B1,B2,B6,B7,C1,C2,C6,C7,C8,C9,C10,D0,D1,D2,P1,P2
      REAL*8        APH(*),APH0,BOITE(2,3,2)

      NOM1 = NOM1Z
      NOMC = NOMCZ

C --- LECTURE DONNEES

      CALL JEMARQ()

      CALL JEVEUO(NOM1//'.GROUPEMA','L',B0)
      CALL JEVEUO(NOM1//'.BOITE.DIME','L',B1)
      CALL JEVEUO(NOM1//'.BOITE.MINMAX','L',B2)      

      NM1 = ZI(B1+1)
      IF (NM1.GT.1) THEN
        CALL JEVEUO(NOM1//'.GRMAMA','L',B6)
        CALL JEVEUO(JEXATR(NOM1//'.GRMAMA','LONCUM'),'L',B7)
      ENDIF

      CALL JEVEUO(NOMC//'.BOITE.DIME','L',C1)
      CALL JEVEUO(NOMC//'.BOITE.MINMAX','L',C2)
      CALL JELIRA(NOMC//'.GROUPEMA.BORD','LONMAX',NMB,ZK8)
      CALL JEVEUO(NOMC//'.GROUPEMA.BORD','L',C8)
      CALL JEVEUO(JEXATR(NOMC//'.'//NOM1,'LONCUM'),'L',C9)     
      CALL JELIRA(NOMC//'.'//NOM1,'LONT',I,ZK8)
      CALL JEVEUO(NOMC//'.'//NOM1,'L',C10)

      NMC = ZI(C1+1)
      IF (NMC.GT.1) THEN
        CALL JEVEUO(NOMC//'.GRMAMA','L',C6)
        CALL JEVEUO(JEXATR(NOMC//'.GRMAMA','LONCUM'),'L',C7)
      ENDIF

      CALL WKVECT('&&ARLAPH.PILE','V V I',MAX(NMC,NM1),D0)
      CALL WKVECT('&&ARLAPH.FILTRE.C','V V I',NMC,D1)
      CALL WKVECT('&&ARLAPH.FILTRE.1','V V I',NM1,D2)

      DIM = ZI(B1)
      
C --- COMPOSANTES CONNEXES DE NOMC.BORD

      DO 10 I = 1, NMC
        ZI(D1-1+I) = 0
 10   CONTINUE

      DO 20 I = 1, NMB
        ZI(D1-1+ZI(C8-1+I)) = -1
 20   CONTINUE

      NCMP = 0
      DO 30 I = 1, NMB

        MA1 = ZI(C8-1+I)
        IF (ZI(D1-1+MA1).GE.0) GOTO 30

        NCMP = NCMP + 1
        ZI(D1-1+MA1) = NCMP
        IF (NMC.EQ.1) GOTO 30
        ZI(D0) = MA1
        NPILE = 1
 
 40     CONTINUE

        IF (NPILE.NE.0) THEN

          NPILE = NPILE - 1
          MA1 = ZI(D0+NPILE)
          P0 = ZI(C7-1+MA1)
          P1 = ZI(C7+MA1)-1

          DO 50 J = P0, P1

            MA2 = ZI(C6-1+J)
            IF (ZI(D1-1+MA2).GE.0) GOTO 50

            ZI(D1-1+MA2) = NCMP
            ZI(D0+NPILE) = MA2
            NPILE = NPILE + 1

 50       CONTINUE

          GOTO 40

        ENDIF

 30   CONTINUE

C --- BOITES ENGLOBANTES DES COMPOSANTES CONNEXES DE NOMC.BORD

      DO 60 I = 1, NCMP
        DO 60 J = 1, DIM
          BOITE(1,J,I) = R8MAX
          BOITE(2,J,I) = -R8MAX
 60   CONTINUE

      P0 = C2
      DO 70 I = 1, NMC
        K = ZI(D1-1+I)
        IF (K.NE.0) THEN
          DO 80 J = 1, DIM
            IF (ZR(P0  ).LT.BOITE(1,J,K)) BOITE(1,J,K)=ZR(P0  )
            IF (ZR(P0+1).GT.BOITE(2,J,K)) BOITE(2,J,K)=ZR(P0+1)
            P0 = P0 + 2
 80       CONTINUE
        ELSE
          P0 = P0 + 2*DIM
        ENDIF
 70   CONTINUE

      IF (NCMP.EQ.2) THEN

        IF ((BOITE(2,1,1)-BOITE(1,1,1)).LT.
     &      (BOITE(2,1,2)-BOITE(1,1,2))) THEN
          CPI = 1
        ELSE
          CPI = 2
        ENDIF

        DO 90 I = 1, NMC
          NCMP = ZI(D1-1+I)
          IF (NCMP.EQ.CPI) THEN
            ZI(D1-1+I) = 0
          ELSEIF (NCMP.NE.0) THEN
            ZI(D1-1+I) = 1
          ENDIF
 90     CONTINUE

      ELSE

        CPI = 1

      ENDIF

C --- COMPOSANTES CONNEXES DE NOM1 - NOMC

      DO 100 I = 1, NM1
        ZI(D2-1+I) = 0
 100  CONTINUE

      P1 = ZI(C9)
      DO 110 I = 1, NMC
        NCMP = -ZI(D1-1+I) - 1
        P0 = P1
        P1 = ZI(C9+I)
        DO 110 J = P0, P1-1
          P2 = D2-1+ZI(C10-1+J)
          IF (NCMP.LT.ZI(P2)) ZI(P2) = NCMP
 110  CONTINUE

      NCMP = 0
      DO 120 I = 1, NM1

        IF (ZI(D2-1+I).NE.0) GOTO 120

        NCMP = NCMP + 1
        ZI(D2-1+I) = NCMP
        IF (NM1.EQ.1) GOTO 120
        ZI(D0) = I
        NPILE = 1
 
 130    CONTINUE

        IF (NPILE.NE.0) THEN

          NPILE = NPILE - 1
          MA1 = ZI(D0+NPILE)
          P0 = ZI(B7-1+MA1)
          P1 = ZI(B7+MA1)-1

          DO 140 J = P0, P1

            MA2 = ZI(B6-1+J)
            IF (ZI(D2-1+MA2).NE.0) GOTO 140

            ZI(D2-1+MA2) = NCMP
            ZI(D0+NPILE) = MA2
            NPILE = NPILE + 1

 140      CONTINUE

          GOTO 130

        ENDIF

 120  CONTINUE

C --- COMPOSANTES INTERIEURES / EXTERIEURES

      DO 150 I = 1, NCMP

        P0 = B2

        DO 160 J = 1, NM1

          IF (ZI(D2-1+J).EQ.I) THEN

            DO 170 K = 1, DIM
              IF ((ZR(P0  ).LT.BOITE(1,K,CPI)).OR.
     &            (ZR(P0+1).GT.BOITE(2,K,CPI))) GOTO 150
              P0 = P0 + 2
 170        CONTINUE
         
            DO 180 K = 1, NM1            
              IF (ZI(D2-1+J).NE.I) GOTO 180
              ZI(D2-1+J) = -1
 180        CONTINUE

          ELSE

            P0 = P0 + 2*DIM

          ENDIF

 160    CONTINUE

 150  CONTINUE

C --- PONDERATIONS DES MAILLES INTERIEURES

      NPILE = 0

      DO 190 I = 1, NM1
        NCMP = ZI(D2-1+I)
        IF (NCMP.LE.-1) THEN
          P0 = ZI(B0-1+I)
          APH(P0) = APH(P0) * APH0 
        ENDIF
 190  CONTINUE

C --- DESALLOCATION

      CALL JEDETR('&&ARLAPH.VOL2')
      CALL JEDETR('&&ARLAPH.MAMA')
      CALL JEDETR('&&ARLAPH.FILTRE.1')
      CALL JEDETR('&&ARLAPH.FILTRE.C')
      CALL JEDETR('&&ARLAPH.PILE')

      CALL JEDEMA()

      END
