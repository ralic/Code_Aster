      SUBROUTINE ARLCAL(MAIL,DIM,QUADZ,NOMCZ,CINEC,NOM1Z,
     &                  CINE1,NTM,NORMZ,TANGZ,EPAIZ,MORZ)
 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C          CALCUL DES MATRICES DE COUPLAGE ELEMENTAIRE ARLEQUIN 
C               ASSEMBLAGE DANS LA MATRICE ARLEQUIN MORSE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C CHARACTER*8       MAIL       : SD MAILLAGE
C INTEGER           DIM        : DIMENSION DE L'ESPACE
C CHARACTER*(10)    QUADZ      : SD QUADRATURES A CALCULER (CF ARLFAM)
C CHARACTER*(10)    NOMCZ      : SD DOMAINE DE COLLAGE
C CHARACTER*8       CINEC      : CINEMATIQUE DU DOMAINE DE COLLAGE
C                                ('SOLIDE  ' OU 'COQUE   ') (CF ARLVER)
C CHARACTER*(10)    NOM1Z      : SD DOMAINE MECANIQUE
C CHARACTER*8       CINE1      : CINEMATIQUE DU DOMAINE MECANIQUE
C                                ('SOLIDE  ' OU 'COQUE   ') (CF ARLVER)
C CHARACTER*8       NTM(*)     : VECTEUR NOMS TYPES DE MAILLE
C CHARACTER*(10)    NNORMZ     : NORMALES LISSEES COQUE (CF LISNOR)
C CHARACTER*(10)    TANGZ      : TANGENTES LISSEES COQUE (CF LISNOR)
C CHARACTER*(10)    EPAIZ      : EPAISSEUR COQUE (CF LISNOR)
C CHARACTER*(16)    MORZ       : SD MATRICE ARLEQUIN MORSE (CF ARLFAC)
C
C SD D'ENTREE
C NOMC.GROUPEMA : LISTE DE MAILLES DOMAINE DE COLLAGE
C NOMC.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C NOMC.METRQ    : METRIQUE D'ADIMENSIONEMENT DU PRODUIT H1 (CF ARLMTR)
C NOM1.GROUPEMA : LISTE DE MAILLES DOMAINE 1
C NOM1.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C
C SD DE SORTIE
C MORSE.VALE    : VECTEUR DE VALEUR DE LA MATRICE ARLEQUIN MORSE
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
      REAL*8        PREC
      PARAMETER     (PREC = 0.1D0)

      INTEGER       NH,NI,NN0,NG0
      PARAMETER     (NH = 2)
      PARAMETER     (NI = 10)
      PARAMETER     (NN0 = 27)
      PARAMETER     (NG0 = 64)
      
C --- VARIABLES
      CHARACTER*(*) QUADZ,NOMCZ,NOM1Z,NORMZ,MORZ,TANGZ,EPAIZ
      CHARACTER*16  MORSE
      CHARACTER*10  NOMC,NOM1,QUAD,NORM,TANG,EPAI
      CHARACTER*8   MAIL,CINEC,CINE1,NTM(*),TMA1,TMA2
      INTEGER       DIM,NINO,NQUA,NN1,NN2,NN3,IMA1,IMA2,MA1,MA2
      INTEGER       A0,A1,A2,A3,A4,A5,A6,B0,B1,B2,B3,B4,B5,M,IJ,B
      INTEGER       C0,C1,C2,C3,C4,C5,D0,D1,D2,D3,D4,D5,D6,E0,E1,E2,E3
      INTEGER       F0,F1,F2,F3,F4,F5,F6,F7,G0,G1,G2,G3,P0,P1,P2
      INTEGER       NDEMA,NL,NG,NMO,DPOINT,IAS,I,J,K,L
      REAL*8        NO1(3*NN0),NO2(3*NN0),NO3(3*NN0),H1,H2
      LOGICAL       NONNUL

      MORSE = MORZ
      QUAD = QUADZ
      NOMC = NOMCZ
      NOM1 = NOM1Z
      NORM = NORMZ
      TANG = TANGZ
      EPAI = EPAIZ

C --- NUMERO D'ASSEMBLAGE

      IAS = 0
      IF (CINEC.EQ.'COQUE   ') IAS = IOR(IAS,1)
      IF (CINE1.EQ.'COQUE   ') IAS = IOR(IAS,2)

C --- LECTURE DONNEES

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.TYPMAIL','L',A0)
      CALL JEVEUO(MAIL//'.COORDO    .VALE','L',A1)
      CALL JEVEUO(MAIL//'.CONNEX','L',A2)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',A3)

      IF (IAS.NE.0) THEN 
        CALL JEVEUO(NORM,'L',A4)
        CALL JEVEUO(TANG,'L',A5)
        CALL JEVEUO(EPAI,'L',A6)
      ELSE
        A4 = A1
      ENDIF

      CALL JEVEUO(NOMC//'.GROUPEMA','L',B0)
      CALL JEVEUO(NOMC//'.BOITE.H','L',B1)
      CALL JEVEUO(NOMC//'.BOITE.DIME','L',B2)
      CALL JEVEUO(NOMC//'.BOITE.MINMAX','L',B3)
      CALL JEVEUO(NOMC//'.BOITE.PAN','L',B4)
      CALL JEVEUO(NOMC//'.BOITE.SOMMET','L',B5)
      CALL JEVEUO(NOMC//'.METRQ','L',M)

      CALL JEVEUO(NOM1//'.GROUPEMA','L',C0)
      CALL JEVEUO(NOM1//'.BOITE.H','L',C1)
      CALL JEVEUO(NOM1//'.BOITE.DIME','L',C2)
      CALL JEVEUO(NOM1//'.BOITE.MINMAX','L',C3)
      CALL JEVEUO(NOM1//'.BOITE.PAN','L',C4)
      CALL JEVEUO(NOM1//'.BOITE.SOMMET','L',C5)

      CALL JELIRA(QUAD//'.NUMERO','LONMAX',NQUA,ZK8)
      CALL JEVEUO(QUAD//'.NUMERO','L',D0)
      CALL JEVEUO(QUAD//'.TYPEMA','L',D1)
      CALL JEVEUO(QUAD//'.LIMAMA','L',D2)
      CALL JEVEUO(JEXATR(QUAD//'.LIMAMA','LONCUM'),'L',D3)
      CALL JEVEUO(QUAD//'.MAMA','L',D4)

      CALL JEVEUO(NOMC//'.INO','L',E0)
      CALL JELIRA(NOMC//'.INO','LONMAX',NINO,ZK8)
      CALL JEVEUO(MORSE//'.INO','L',E1)
      CALL JELIRA(MORSE//'.INO','LONT',NMO,ZK8)
      CALL JEVEUO(JEXATR(MORSE//'.INO','LONCUM'),'L',E2)
      CALL JEVEUO(MORSE//'.DIME','L',E3)
      DPOINT = ZI(E3)
      CALL WKVECT(MORSE//'.VALE','V V R',DPOINT*NMO,E3)

      CALL PREDIV(NH,NI,'&&ARLCAL',F0,F1,F2,F3,F4,F5,F6,F7,NL)

      CALL WKVECT('&&ARLCAL.PG','V V R',NG0,G0)
      CALL WKVECT('&&ARLCAL.G','V V R',3*NG0,G1)
      CALL WKVECT('&&ARLCAL.FG','V V R',NG0*NN0,G2)
      CALL WKVECT('&&ARLCAL.DFG','V V R',3*NG0*NN0,G3)
     
      CALL WKVECT('&&ARLCAL.B','V V R',NN0*NN0,B)
      CALL WKVECT('&&ARLCAL.IJ','V V I',NN0*NN0,IJ)

C --- INITIALISATION

      DO 10 I = 1, NN0*NN0
        ZR(B-1+I) = 0.D0
 10   CONTINUE

      DO 20 I = 1, DPOINT*NMO
        ZR(E3-1+I) = 0.D0
 20   CONTINUE

C --- INTEGRATION

      D6 = ZI(D3)

      DO 30 I = 1, NQUA

        TMA1 = ZK8(D1)
        CALL PGAUSS(TMA1,ZI(D0),ZR(G0),ZR(G1),NG,DIM)

        P0 = G1
        P1 = G2
        P2 = G3

        DO 40 J = 1, NG
          CALL FORME0(ZR(P0),TMA1,ZR(P1),NN1)
          CALL FORME1(ZR(P0),TMA1,ZR(P2),NN1,DIM)
          P0 = P0 + DIM
          P1 = P1 + NN1
          P2 = P2 + NN1*DIM
 40     CONTINUE

        D5 = D6
        D6 = ZI(D3+I)

        DO 50 J = D5, D6-1
        
          P0 = D4 + 2*(ZI(D2-1+J)-1)
          MA1 = ZI(P0)
          MA2 = ZI(P0+1)
          NONNUL = .TRUE.

          IMA2 = ZI(C0-1+ABS(MA2))
          TMA2 = NTM(ZI(A0-1+IMA2))
          CALL TMACOQ(TMA2,DIM,L)
          CALL CONOEU(IMA2,ZI(A2),ZI(A3),ZR(A1),ZR(A4),DIM,L,NO2,NN2)

C ------- MEME MAILLE

          IF (MA1.EQ.0) THEN

            CALL ARLTE2(DIM,ZR(G0),ZR(G2),ZR(G3),NG,NO2,NN2,ZR(M),ZR(B))
            NN1 = NN2
            MA1 = MA2
            IMA1 = IMA2
            GOTO 80

          ENDIF

          IMA1 = ZI(B0-1+ABS(MA1))
          TMA1 = NTM(ZI(A0-1+IMA1))
          CALL TMACOQ(TMA1,DIM,L)
          CALL CONOEU(IMA1,ZI(A2),ZI(A3),ZR(A1),ZR(A4),DIM,L,NO1,NN1)

C ------- INTEGRATION STANDARD

          IF (MA2.GT.0) THEN

C --------- INTEGRATION SUR MA1

            IF (MA1.GT.0) THEN

              H2 = ZR(C1-1+MA2)
              CALL ARLTE3(DIM,ZR(G0),ZR(G2),ZR(G3),NG,NO1,
     &                    NN1,TMA2,NO2,NN2,H2,ZR(M),ZR(B))
                    
C --------- INTEGRATION SUR MA2

            ELSE

               MA1 = -MA1
              H1 = ZR(B1-1+MA1)
              CALL ARLTE4(DIM,ZR(G0),ZR(G2),ZR(G3),NG,NO2,
     &                    NN2,TMA1,NO1,NN1,H1,ZR(M),ZR(B))

            ENDIF

C ------- INTEGRATION SPECIALE

          ELSE

            MA2 = -MA2

C --------- INTEGRATION SUR MA1

            IF (MA1.GT.0) THEN

              H2 = ZR(C1-1+MA2)

              CALL DIVISE(MA2,TMA2,NO2,ZI(C2),ZR(C1),ZR(C3),ZR(C4),
     &                    MA1,TMA1,NO1,ZI(B2),ZR(B1),ZR(B3),ZR(B4),
     &                    DIM,NH,NI,NL,ZR(F0),ZI(F1),ZI(F2),NDEMA,
     &                    ZR(F3),ZI(F4),ZL(F5),ZI(F6),ZI(F7))

              IF (NDEMA.EQ.0) THEN
                
                CALL MINCLU(DIM,MA1,ZI(B2),ZR(B3),ZR(B5),
     &                          MA2,ZI(C2),ZR(C4),ZR(C1),1,PREC,NONNUL)
                IF (NONNUL) CALL ARLTE3(DIM,ZR(G0),ZR(G2),ZR(G3),NG,NO1,
     &                                  NN1,TMA2,NO2,NN2,H2,ZR(M),ZR(B))

              ELSE

                P0 = F1
                ZR(B) = 0.D0
                DO 60 K = 1, NDEMA

                  NN3 = ZI(F2-1+K)
                  IF (NN3.NE.0) THEN
                    CALL CONOEU(0,ZI(P0),NN3,ZR,ZR(F0),DIM,NN2,NO3,NN3)
                    CALL ARLTE5(DIM,ZR(G0),ZR(G2),ZR(G3),NG,TMA1,NO1,
     &                          NN1,TMA2,NO2,NN2,H2,NO3,ZR(M),ZR(B))
                  ENDIF

                  P0 = P0 + 8

 60             CONTINUE

              ENDIF

C --------- INTEGRATION SUR MA2

            ELSE

              MA1 = -MA1
              H1 = ZR(B1-1+MA1)

              CALL DIVISE(MA1,TMA1,NO1,ZI(B2),ZR(B1),ZR(B3),ZR(B4),
     &                    MA2,TMA2,NO2,ZI(C2),ZR(C1),ZR(C3),ZR(C4),
     &                    DIM,NH,NI,NL,ZR(F0),ZI(F1),ZI(F2),NDEMA,
     &                    ZR(F3),ZI(F4),ZL(F5),ZI(F6),ZI(F7))
              
              IF (NDEMA.EQ.0) THEN
                
                CALL MINCLU(DIM,MA2,ZI(C2),ZR(C3),ZR(C5),
     &                          MA1,ZI(B2),ZR(B4),ZR(B1),1,PREC,NONNUL)
                IF (NONNUL) CALL ARLTE4(DIM,ZR(G0),ZR(G2),ZR(G3),NG,NO2,
     &                                  NN2,TMA1,NO1,NN1,H1,ZR(M),ZR(B))
              ELSE

                P0 = F1
                ZR(B) = 0.D0
                DO 70 K = 1, NDEMA

                  NN3 = ZI(F2-1+K)
                  IF (NN3.NE.0) THEN
                    CALL CONOEU(0,ZI(P0),NN3,ZR,ZR(F0),DIM,NN1,NO3,NN3)
                    CALL ARLTE6(DIM,ZR(G0),ZR(G2),ZR(G3),NG,TMA2,NO2,
     &                          NN2,TMA1,NO1,NN1,H1,NO3,ZR(M),ZR(B))
                  ENDIF

                  P0 = P0 + 8

 70             CONTINUE

              ENDIF

            ENDIF

          ENDIF

C ------- ASSEMBLAGE

          IF (.NOT.NONNUL) GOTO 50

 80       CONTINUE

C          WRITE(*,*) 'INTEGRATION',MA1,MA2
C          WRITE(*,*) 'NO1',(NO1(K),K=1,DIM*NN1)
C          WRITE(*,*) 'NO2',(NO2(K),K=1,DIM*NN2)
C          DO 2 K = 1, NN1
C            WRITE(*,*) (ZR(B-1+L+NN2*(K-1)),L=1,NN2)
C 2        CONTINUE

          CALL ARLAS0(IMA1,IMA2,ZI(A2),ZI(A3),ZI(E0),
     &                     NINO,ZI(E1),ZI(E2),ZI(IJ))

          GOTO (90,100,110) IAS

          CALL ARLAS1(DPOINT,NN1,NN2,ZI(IJ),ZR(B),ZR(E3))
          GOTO 50

 90       CONTINUE
          CALL ARLAS2(DIM,DPOINT,ZI(A2),ZI(A3),ZR(A5),
     &                NN1,IMA1,NN2,ZI(IJ),ZR(B),ZR(E3))
          GOTO 50
          
 100      CONTINUE
          CALL ARLAS3(DIM,DPOINT,ZR(A6-1+IMA2),
     &                NN1,NO2,NN2,ZI(IJ),ZR(B),ZR(E3))
          GOTO 50

 110      CONTINUE
          CALL ARLAS4(DIM,DPOINT,ZI(A2),ZI(A3),ZR(A5),ZR(A6-1+IMA2),
     &                         NN1,IMA1,NO2,NN2,ZI(IJ),ZR(B),ZR(E3))
 50     CONTINUE

        D0 = D0 + 1
        D1 = D1 + 1

 30   CONTINUE

C --- DESALLOCATION

      CALL JEDETR('&&ARLCAL.B')
      CALL JEDETR('&&ARLCAL.IJ')

      CALL JEDETR('&&ARLCAL.PG')
      CALL JEDETR('&&ARLCAL.G')
      CALL JEDETR('&&ARLCAL.FG')
      CALL JEDETR('&&ARLCAL.DFG')

      CALL FINDIV('&&ARLCAL')

      CALL JEDEMA()

      END
