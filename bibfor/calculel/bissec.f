      SUBROUTINE BISSEC(NOMZ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C                                                                       
C                                                                       
C ======================================================================
C A_UTIL
C ----------------------------------------------------------------------
C     CONSTRUCTION D'UN ARBRE DE PARTITION BINAIRE DE L'ESPACE (BSP)
C ----------------------------------------------------------------------
C VARIABLE D'ENTREE
C CHARACTER*(10)   NOMZ   :  SD DOMAINE
C 
C SD D'ENTREE
C NOM.BOITE      : SD BOITES ENGLOBANTES (CF BOITE)
C
C SD DE SORTIE
C NOM.ARBRE.LIMA : VECTEUR LISTE DE MAILLE (MA1, MA2, ...)
C NOM.ARBRE.CELL : TABLEAU D'ARBORESCENCE A 3 COLONNES
C                 ( CELL1.1, CELL1.2, CELL1.3, CELL2.1, CELL2.2, ... )
C                  SI CELL*.1 > 0  
C                     CELL*.1 : POINTEUR PAN DANS NOM.BOITE.PAN
C                     CELL*.2 : LIGNE DANS NOM.ARBRE.CELL LORSQUE
C                               POSITION POINT < 0 PAR RAPPORT AU PAN
C                     CELL*.3 : LIGNE DANS NOM.ARBRE.CELL LORSQUE
C                               POSITION POINT > 0 PAR RAPPORT AU PAN
C                  SI CELL*.1 < 0
C                     CELL*.1 : - NOMBRE DE MAILLES CANDIDATES
C                     CELL*.2 : POINTEUR DANS NOM.ARBRE.LIMA DES MAILLES
C                               CANDIDATES
C ---------------------------------------------------------------------
C             POUR SON UTILISATION : CF CERNE ET LOCALI
C ---------------------------------------------------------------------

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
      INTEGER IMAX,NMIN
      PARAMETER (IMAX = 15)
      PARAMETER (NMIN = 8)
      REAL*8  GAMMA0,GAMMA1,PREC00
      PARAMETER (GAMMA0 = 0.17D0)
      PARAMETER (GAMMA1 = 0.90D0)
      PARAMETER (PREC00 = 0.00001D0)

C --- GENERATEUR PSEUDO-ALEATOIRE
      INTEGER GRAIN0,GRAIN1,GRAINS(32),GRAMAX
      PARAMETER (GRAMAX = 2147483647)

C --- VARIABLES
      INTEGER        DIME,NMA,NLIMA,NGAMA,NGAMA0,NGAM00,NGAM01,L0,LN
      INTEGER        NCELL,DCELL,FCELL,ICELL,IPAN,IPAN0,IMA,ILIMA
      INTEGER        NN,NP,NZ,NN0,NP0,NSOM,NPAN,ISOM,MODNMA,MODPAN
      INTEGER        H,I,J,K,L,P0,P1,P2,P3,P4,P5,P6,Q0,Q1,Q2,Q3,Q4
      CHARACTER*(*)  NOMZ
      CHARACTER*10   NOM
      REAL*8         PREC,PREC0,R,R0

      NOM = NOMZ

C --- LECTURE DONNEES
  
      CALL JEMARQ()
      CALL JEVEUO(NOM//'.BOITE.DIME','L',P0)
      CALL JEVEUO(NOM//'.BOITE.PAN','L',P1)
      CALL JEVEUO(NOM//'.BOITE.SOMMET','L',P2)
      CALL JEVEUO(NOM//'.BOITE.H','L',P3)

      DIME = ZI(P0)
      NMA = ZI(P0+1)

C --- ALLOCATIONS

      H = (LOG(1.D0*NMIN/NMA))/LOG(0.5D0+0.5D0*GAMMA0)
      H = MAX(H,0) + 1
      NLIMA = ((1.D0+GAMMA0)**H)*NMA
      NCELL = (2**H)-1
      CALL WKVECT(NOM//'.ARBRE.LIMA','V V I',NLIMA,Q0)
      CALL WKVECT(NOM//'.ARBRE.CELL','V V I',NCELL*3,Q1)
      CALL WKVECT('&&BISSEC.LIMA','V V I',NMA,Q2)

C --- INITIALISATION
 
      GRAIN1 = 0
      DO 10 I = 1, NMA
        ZI(Q0-1+I) = I
 10   CONTINUE
      DCELL = 0
      FCELL = 1
      ZI(Q1) = -NMA
      ZI(Q1+1) = 1
      ZI(Q1+2) = NLIMA

 20   CONTINUE

C --- BISSECTION

      IF (DCELL.LT.FCELL) THEN
  
C ----- LECTURE CELLULE

        ICELL = Q1 + 3*DCELL
        DCELL = DCELL + 1
        NMA = -ZI(ICELL)
        IF (NMA.LE.NMIN) GOTO 20
        ILIMA = ZI(ICELL+1)
        L0 = ZI(ICELL+2)
        Q3 = Q0-1 + ILIMA

        DO 30 I = 1, NMA
          ZI(Q2-1+I) = ZI(Q3-1+I)
 30     CONTINUE

        NGAM00 = GAMMA0*NMA + 1
        NGAM01 = GAMMA1*NMA
        NGAMA0 = 3*NMA
        MODNMA = 1 + (GRAMAX-1)/NMA

C ----- CHOIX DU PAN SEPARATEUR

        DO 40 I = 1, IMAX

          CALL HASARD(GRAIN0,GRAIN1,GRAINS,32)
          IMA = ZI(Q2+GRAIN1/MODNMA)
          PREC = PREC00*ZR(P3-1+IMA) 
          P6 = P0+2*IMA
          IPAN = ZI(P6)
          NPAN = ZI(P6+2)-IPAN
          CALL HASARD(GRAIN0,GRAIN1,GRAINS,32)
          MODPAN = 1 + (GRAMAX-1)/NPAN
          IPAN = 1+(DIME+2)*(IPAN-1+GRAIN1/MODPAN)
          P4 = P1-1+IPAN
          R0 = ZR(P4+DIME) 

          NN = 0
          NP = 0
          NZ = 0

C ------- POSITION DES MAILLES PAR RAPPORT AU PAN

          DO 50 J = 1, NMA
          
            P6 = P0+1+2*ZI(Q2-1+J)
            ISOM = ZI(P6)
            NSOM = ZI(P6+2)-ISOM
            P5 = P2+DIME*(ISOM-1)            

C --------- SOMMETS DU CONVEXE ASSOCIE DU MEME COTE ?
   
            DO 60 K = 1, NSOM
              R = R0
              DO 70 L = 1, DIME
                R = R + ZR(P4-1+L)*ZR(P5)
                P5 = P5 + 1
 70           CONTINUE
              IF (ABS(R).GT.PREC) GOTO 80
 60         CONTINUE

            NZ = NZ + 1
            GOTO 50

 80         CONTINUE

            IF (R.LT.0.D0) THEN
              DO 90 K = K+1, NSOM
                R = R0
                DO 100 L = 1, DIME
                  R = R + ZR(P4-1+L)*ZR(P5)
                  P5 = P5 + 1
 100            CONTINUE
                IF (R.GT.PREC) THEN
                  NZ = NZ + 1
                  GOTO 50
                ENDIF
 90           CONTINUE
              NN = NN + 1
            ELSE
              DO 110 K = K+1, NSOM
                R = R0
                DO 120 L = 1, DIME
                  R = R + ZR(P4-1+L)*ZR(P5)
                  P5 = P5 + 1
 120            CONTINUE
                IF (R.LT.-PREC) THEN
                  NZ = NZ + 1
                  GOTO 50
                ENDIF
 110          CONTINUE
              NP = NP + 1
            ENDIF

 50       CONTINUE

C ------- QUALITE DU PAN

          NGAMA = NZ + ABS(NN-NP)
 
          IF (NGAMA.LT.NGAMA0) THEN
            NGAMA0 = NGAMA
            IPAN0 = IPAN
            NN0 = NN + NZ
            NP0 = NP + NZ
            PREC0 = PREC
            IF (NGAMA0.LT.NGAM00) GOTO 130
          ENDIF
 
 40     CONTINUE

 130    CONTINUE

        IF (NGAMA0.LT.NGAM01) THEN

          IF (((NN0+NP0).LT.L0).AND.((FCELL+2).LE.NCELL)) THEN

C --------- ECRITURE CELLULES

            ZI(ICELL) = IPAN0
            ZI(ICELL+1) = FCELL + 1
            ZI(ICELL+2) = FCELL + 2

            ICELL = Q1 + 3*FCELL
            LN = (L0*NN0)/(NN0+NP0)

            ZI(ICELL) = -NN0
            ZI(ICELL+1) = ILIMA
            ZI(ICELL+2) = LN

            ZI(ICELL+3) = -NP0
            ZI(ICELL+4) = ILIMA+LN
            ZI(ICELL+5) = L0-LN

            FCELL = FCELL + 2
            Q4 = Q3 + LN
            P4 = P1-1+IPAN0
            R0 = ZR(P4+DIME)          
            PREC = PREC0

C --------- ECRITURE LIMA

            DO 140 J = 1, NMA
          
              IMA = ZI(Q2-1+J)
              P6 = P0+1+2*IMA
              ISOM = ZI(P6)
              NSOM = ZI(P6+2)-ISOM
              P5 = P2+DIME*(ISOM-1)

C ----------- SOMMETS DU CONVEXE ASSOCIE DU MEME COTE ?

              DO 150 K = 1, NSOM
                R = R0
                DO 160 L = 1, DIME
                  R = R + ZR(P4-1+L)*ZR(P5)
                  P5 = P5 + 1
 160            CONTINUE
                IF (ABS(R).GT.PREC) GOTO 170
 150          CONTINUE

              ZI(Q3) = IMA
              Q3 = Q3 + 1
              ZI(Q4) = IMA
              Q4 = Q4 + 1
              GOTO 140

 170          CONTINUE

              IF (R.LT.0.D0) THEN
                ZI(Q3) = IMA
                Q3 = Q3 + 1
                DO 180 K = K+1, NSOM
                  R = R0
                  DO 190 L = 1, DIME
                    R = R + ZR(P4-1+L)*ZR(P5)
                    P5 = P5 + 1
 190              CONTINUE
                  IF (R.GT.PREC) THEN
                    ZI(Q4) = IMA
                    Q4 = Q4 + 1
                    GOTO 140
                  ENDIF
 180            CONTINUE
              ELSE
                ZI(Q4) = IMA
                Q4 = Q4 + 1
                DO 200 K = K+1, NSOM
                  R = R0
                  DO 210 L = 1, DIME
                    R = R + ZR(P4-1+L)*ZR(P5)
                    P5 = P5 + 1
 210              CONTINUE
                  IF (R.LT.-PREC) THEN
                    ZI(Q3) = IMA
                    Q3 = Q3 + 1
                    GOTO 140
                  ENDIF
 200            CONTINUE
              ENDIF

 140       CONTINUE

          ENDIF

        ENDIF

        GOTO 20

      ENDIF

C --- DESALLOCATION

      CALL JEDETR('&&BISSEC.LIMA')
      CALL JEDEMA()

      END
