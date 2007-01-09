      SUBROUTINE BISSEC(NOMARB,NOMBOI)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C 
      IMPLICIT NONE
      CHARACTER*16   NOMBOI,NOMARB
C     
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C CONSTRUCTION D'UN ARBRE DE PARTITION BINAIRE DE L'ESPACE (BSP)
C
C ----------------------------------------------------------------------
C
C
C I/O NOMMED : NOM DE LA SD DE STOCKAGE MAILLES DE LA ZONE MEDIATRICE 
C
C ON ENRICHIT NOMMED(1:10)   
C     IN : NOMMED(1:10)//'.GROUPEMA': LISTE DES MAILLES
C     IN : NOMMED(1:10)//'.BOITE'   : LISTE DES BOITES ENGLOBANTES
C     IN : NOMMED(1:10)//'.GRMAMA'  : GRAPHE MAILLE/MAILLE
C     IN : NOMMED(1:10)//'.CNCINV'  : CONNECTIVITE INVERSE
C     OUT: NOMMED(1:10)//'.ARBRE'   : ARBRE DE PARTITION
C       ARBRE.LIMA : VECTEUR LISTE DE MAILLE (MA1, MA2, ...)
C       ARBRE.CELL : TABLEAU D'ARBORESCENCE A 3 COLONNES
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
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C 
      INTEGER        IMAX,NMIN
      REAL*8         GAMMA0,GAMMA1,PREC   
      INTEGER        DIME,NMA,NGAMA,NGAMA0,NGAM00,NGAM01,L0,LN
      INTEGER        DCELL,FCELL,ICELL,IPAN,IPAN0,IMA,ILIMA
      INTEGER        NN,NP,NZ,NN0,NP0,NSOM,NPAN,ISOM,MODNMA,MODPAN
      INTEGER        I,J,K,L,P4,P5,Q3,Q4
      INTEGER        JINFO
      REAL*8         R,R0
      INTEGER        IFM,NIV
      INTEGER        NCELL
      INTEGER        P0,P1,P2,Q0,Q1,Q2,P6
C --- GENERATEUR PSEUDO-ALEATOIRE
      INTEGER        GRAIN0,GRAIN1,GRAINS(32),GRAMAX
      PARAMETER      (GRAMAX = 2147483647)      
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV) 
C
C --- LECTURE SD ARBRE
C
      CALL JEVEUO(NOMARB(1:16)//'.LIMA','E',Q0)
      CALL JEVEUO(NOMARB(1:16)//'.CELL','E',Q1)
      CALL JEVEUO(NOMARB(1:16)//'.INFO','L',JINFO)            
C
C --- LECTURE SD BOITES
C
      CALL JEVEUO(NOMBOI(1:16)//'.DIME'  ,'L',P0)
      CALL JEVEUO(NOMBOI(1:16)//'.PAN'   ,'L',P1)
      CALL JEVEUO(NOMBOI(1:16)//'.SOMMET','L',P2)
      DIME   = ZI(P0)      
C
C --- INITIALISATIONS
C  
      GRAIN0 = 0
      GRAIN1 = 0
      DCELL  = 0
      FCELL  = 1    
      NMA    = NINT(ZR(JINFO + 1))
      NMIN   = NINT(ZR(JINFO + 2))
      GAMMA0 =      ZR(JINFO + 3)
      GAMMA1 =      ZR(JINFO + 4)
      PREC   =      ZR(JINFO + 5)
      IMAX   = NINT(ZR(JINFO + 6))     
      NCELL  = NINT(ZR(JINFO + 8))   
C      
C --- CREATION SD TEMPORAIRE
C 
      CALL WKVECT('&&BISSEC.LIMA','V V I',NMA,Q2)
C
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
C
C --- IMPRESSIONS
C
      IF (NIV.GE.2) THEN
        CALL ARBRIM(IFM,NOMARB)
      ENDIF      
C
C --- DESALLOCATION
C
      CALL JEDETR('&&BISSEC.LIMA')
      CALL JEDEMA()

      END
