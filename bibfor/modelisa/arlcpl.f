      SUBROUTINE ARLCPL(MAIL,QUADZ,NOMCZ,NOM1Z,CINE1,
     &                 NOM2Z,CINE2,NTM,NORMZ,TANGZ,L,APP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C          CALCUL DES MATRICES DE COUPLAGE ELEMENTAIRE ARLEQUIN
C             ASSEMBLAGE DANS LES MATRICES ARLEQUIN MORSES
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8       MAIL       : SD MAILLAGE
C CHARACTER*(10)    QUADZ      : SD QUADRATURES A CALCULER (CF ARLFAM)
C CHARACTER*(10)    NOMCZ      : SD DOMAINE DE COLLAGE
C CHARACTER*(10)    NOM1Z      : SD DOMAINE MECANIQUE 1
C CHARACTER*8       CINE1      : CINEMATIQUE DU DOMAINE MECANIQUE 1
C                                ('SOLIDE  ' OU 'COQUE   ') (CF ARLVER)
C CHARACTER*(10)    NOM2Z      : SD DOMAINE MECANIQUE 2
C CHARACTER*8       CINE2      : CINEMATIQUE DU DOMAINE MECANIQUE 2
C                                ('SOLIDE  ' OU 'COQUE   ') (CF ARLVER)
C CHARACTER*8       NTM(*)     : VECTEUR NOMS TYPES DE MAILLE
C CHARACTER*(10)    NNORMZ     : NORMALES LISSEES COQUE (CF LISNOR)
C CHARACTER*(10)    TANGZ      : TANGENTES LISSEES COQUE (CF LISNOR)
C CHARACTER*(10)    EPAIZ      : EPAISSEUR COQUE (CF LISNOR)
C REAL*8            L          : PONDERATION DEVANT LE TERME H1
C
C VARIABLES D'ENTREE/SORTIE
C LOGICAL           APP(*)     : .TRUE. SI MAILLE * APPARIEE
C
C SD D'ENTREE
C NOMC.INO        : NUMEROS DES NOEUDS DES MULTIPLICATEURS
C NOM1.GROUPEMA   : LISTE DE MAILLES DOMAINE MECANIQUE 1
C NOM1.BOITE      : SD BOITES ENGLOBANTES (CF BOITE)
C NOM1.MORSE      : SD MATRICE DE COUPLAGE MORSE (CF ARLFAC)
C NOM2.GROUPEMA   : LISTE DE MAILLES DOMAINE MECANIQUE 2
C NOM2.BOITE      : SD BOITES ENGLOBANTES (CF BOITE)
C NOM2.MORSE      : SD MATRICE DE COUPLAGE MORSE (CF ARLFAC)
C
C SD DE SORTIE
C NOM*.MORSE.VALE : VECTEUR DE VALEUR DE LA MATRICE ARLEQUIN MORSE
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

C --- FUNCTION
      REAL*8        DDOT,PROVE2

C --- PARAMETRES
      REAL*8  PREC0
      INTEGER NNM,NGM,IMAX
      PARAMETER (NNM = 27)
      PARAMETER (NGM = 64)
      PARAMETER (IMAX = 200)
      PARAMETER (PREC0 = 1.D-8)

C --- VARIABLES
      CHARACTER*(*) QUADZ,NOMCZ,NOM1Z,NOM2Z,NORMZ,TANGZ
      CHARACTER*16  MORSE1,MORSE2
      CHARACTER*10  NOMC,NOM1,NOM2,QUAD,NORM,TANG
      CHARACTER*8   MAIL,CINE1,CINE2,NTM(*),TM0,TM1,TM2
      INTEGER       DIM,NH,NQ,NG,NT,NC,NN0,NN1,NN2,MA1,MA2
      INTEGER       IAS,I1(NNM*NNM),I2(NNM*NNM),IM1,IM2,I,J,K,K1,K2,N
      INTEGER       A0,A1,A2,A3,A4,A5,P,P0,P1,P2,P3,P4,B0,B1,C0,C1,IRET
      INTEGER       Z0,Z1,Z2,E0,E1,E2,E3,E4,E5,E6,D0,D1,D2,D3,D4,D5,D6
      REAL*8        PP(NGM),PG(NGM),FG(NGM*NNM),F1(NGM*NNM),F2(NGM*NNM)
      REAL*8        DF1(3*NGM*NNM),DF2(3*NGM*NNM),DFG(3*NGM*NNM)
      REAL*8        NO1(3*NNM),NO2(3*NNM),G(3*NGM),G1(3),G2(3),PREC
      REAL*8        L1(10*NNM*NNM),L2(10*NNM*NNM),H1,H2,R,L,R8BID
      LOGICAL       APP(*),IR,NN

      QUAD = QUADZ
      NOMC = NOMCZ
      NOM1 = NOM1Z
      NOM2 = NOM2Z
      NORM = NORMZ
      TANG = TANGZ


      MORSE1 = NOM1//'.MORSE'
      MORSE2 = NOM2//'.MORSE'

C --- NUMERO D'ASSEMBLAGE

      IAS = 0
      IF (CINE1.EQ.'COQUE   ') IAS = IOR(IAS,1)
      IF (CINE2.EQ.'COQUE   ') IAS = IOR(IAS,2)

C --- LECTURE DONNEES

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.TYPMAIL','L',A0)
      CALL JEVEUO(MAIL//'.COORDO    .VALE','L',A1)
      CALL JEVEUO(MAIL//'.CONNEX','L',A2)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',A3)

      IF (IAS.NE.0) THEN
        CALL JEVEUO(NORM,'L',A4)
        CALL JEVEUO(TANG,'L',A5)
      ELSE
        A4 = A1
      ENDIF

      CALL JEVEUO(NOM1//'.GROUPEMA','L',B0)
      CALL JEVEUO(NOM1//'.BOITE.H','L',B1)

      CALL JEVEUO(NOM2//'.GROUPEMA','L',C0)
      CALL JEVEUO(NOM2//'.BOITE.H','L',C1)

      CALL JELIRA(QUAD//'.NUMERO','LONMAX',NQ,ZK8)
      CALL JEVEUO(QUAD//'.NUMERO','L',D0)
      CALL JEVEUO(QUAD//'.TYPEMA','L',D1)
      CALL JEVEUO(QUAD//'.LIMAMA','L',D2)
      CALL JEVEUO(JEXATR(QUAD//'.LIMAMA','LONCUM'),'L',D3)
      CALL JEVEUO(QUAD//'.MAMA','L',D4)

      CALL JEVEUO(NOMC//'.INO','L',E0)
      CALL JELIRA(NOMC//'.INO','LONMAX',NC,ZK8)

      CALL JEVEUO(MORSE1//'.INO','L',E1)
      CALL JELIRA(MORSE1//'.INO','LONT',NN1,ZK8)
      CALL JEVEUO(JEXATR(MORSE1//'.INO','LONCUM'),'L',E2)
      CALL JEVEUO(MORSE1//'.DIME','L',E3)
      NN1 = NN1*ZI(E3)
      CALL WKVECT(MORSE1//'.VALE','V V R',NN1,E3)

      CALL JEVEUO(MORSE2//'.INO','L',E4)
      CALL JELIRA(MORSE2//'.INO','LONT',NN2,ZK8)
      CALL JEVEUO(JEXATR(MORSE2//'.INO','LONCUM'),'L',E5)
      CALL JEVEUO(MORSE2//'.DIME','L',E6)
      NN2 = NN2*ZI(E6)
      CALL WKVECT(MORSE2//'.VALE','V V R',NN2,E6)

      CALL JEVEUO('&&ARL.ZR','E',Z0)
      CALL JEVEUO('&&ARL.ZI','E',Z1)
      CALL JEVEUO('&&ARL.ZL','E',Z2)
      CALL JEVEUO('&&ARL.NH','L',P0)
      NH = ZI(P0+1)

C --- INITIALISATION

      DO 10 I = 1, NN1
        ZR(E3) = 0.D0
 10   CONTINUE

      DO 20 I = 1, NN2
        ZR(E6) = 0.D0
 20   CONTINUE

C --- INTEGRATION

      D6 = ZI(D3)

      DO 30 I = 1, NQ

C ----- FORMULE D'INTEGRATION DE LA FAMILLE

        TM0 = ZK8(D1)
        CALL PGAUSS(TM0,ZI(D0),PP,G,NG,DIM)

        P0 = 1
        P1 = 1
        P2 = 1

        DO 40 J = 1, NG

          CALL FORME0(G(P0),TM0,FG(P1),NN0)
          CALL FORME1(G(P0),TM0,DFG(P2),NN0,DIM)

          P0 = P0 + DIM
          P1 = P1 + NN0
          P2 = P2 + NN0*DIM

 40     CONTINUE

        D0 = D0 + 1
        D1 = D1 + 1

C ----- COUPLE DE MAILLES DE LA FAMILLE

        D5 = D6
        D6 = ZI(D3+I)

        DO 50 J = D5, D6-1

          P0 = D4 + 2*(ZI(D2-1+J)-1)
          MA1 = ZI(P0)
          MA2 = ZI(P0+1)

          IM1 = ZI(B0-1+ABS(MA1))
          H1 = ZR(B1-1+ABS(MA1))
          TM1 = NTM(ZI(A0-1+IM1))
          CALL TMACOQ(TM1,DIM,K1)
          CALL CONOEU(IM1,ZI(A2),ZI(A3),ZR(A1),ZR(A4),DIM,K1,NO1,NN1)

          IM2 = ZI(C0-1+ABS(MA2))
          H2 = ZR(C1-1+ABS(MA2))
          TM2 = NTM(ZI(A0-1+IM2))
          CALL TMACOQ(TM2,DIM,K2)
          CALL CONOEU(IM2,ZI(A2),ZI(A3),ZR(A1),ZR(A4),DIM,K2,NO2,NN2)

C ------- MISE A ZERO DES MATRICES DE COUPLAGE ELEMENTAIRES

          DO 60 K = 1, 10*NNM*NNM
            L1(K) = 0.D0
            L2(K) = 0.D0
 60       CONTINUE

          IR = .TRUE.

C ------- INTEGRATION STANDARD

          IF (MA2.GT.0) THEN

            P0 = 1
            P1 = 1
            P2 = 1
            P3 = 1
            P4 = 1

            NN = .TRUE.

C --------- INTEGRATION SUR MA1

            IF (MA1.GT.0) THEN

             DO 70 K = 1, NG

              CALL DCOPY(DIM*NN1,DFG(P1),1,DF1(P3),1)
              CALL MTPROD(NO1,DIM,0,DIM,0,NN1,DF1(P3),DIM,0,DIM,0,G)
              CALL MGAUSS('TCVD',G,DF1(P3),DIM,DIM,NN1,R,IRET)
              IF (IRET.NE.0) GOTO 140

              PG(K) = PP(K) * ABS(R)

              PREC = H2*PREC0
              CALL MTPROD(NO1,DIM,0,DIM,0,NN1,FG(P0),1,0,1,0,G)
              CALL REFERE(G,NO2,DIM,TM2,PREC,IMAX,.TRUE.,G2,IR,F2(P2))
              IF (.NOT.IR) THEN
                   IRET=1
                   GOTO 140
              ENDIF

              CALL FORME1(G2,TM2,DF2(P4),NN2,DIM)
              CALL MTPROD(NO2,DIM,0,DIM,0,NN2,DF2(P4),DIM,0,DIM,0,G)
              CALL MGAUSS('TFVP',G,DF2(P4),DIM,DIM,NN2,R8BID,IRET)

              P0 = P0 + NN0
              P1 = P1 + NN0*DIM
              P2 = P2 + NN2
              P3 = P3 + NN1*DIM
              P4 = P4 + NN2*DIM

 70          CONTINUE

             CALL ARLTE(DIM,PG,NG,FG,DF1,NN1,F2,DF2,NN2,L1,L2)

C --------- INTEGRATION SUR MA2

            ELSE

             DO 80 K = 1, NG

              CALL DCOPY(DIM*NN2,DFG(P1),1,DF2(P4),1)
              CALL MTPROD(NO2,DIM,0,DIM,0,NN2,DF2(P4),DIM,0,DIM,0,G)
              CALL MGAUSS('TCVD',G,DF2(P4),DIM,DIM,NN2,R,IRET)
              IF (IRET.NE.0) GOTO 140

              PG(K) = PP(K) * ABS(R)

              PREC = H1*PREC0
              CALL MTPROD(NO2,DIM,0,DIM,0,NN2,FG(P0),1,0,1,0,G)
              CALL REFERE(G,NO1,DIM,TM1,PREC,IMAX,.TRUE.,G1,IR,F1(P2))
              IF (.NOT.IR) THEN
                  IRET=1
                  GOTO 140
              ENDIF

              CALL FORME1(G1,TM1,DF1(P3),NN1,DIM)
              CALL MTPROD(NO1,DIM,0,DIM,0,NN1,DF1(P3),DIM,0,DIM,0,G)
              CALL MGAUSS('TFVP',G,DF1(P3),DIM,DIM,NN1,R8BID,IRET)

              P0 = P0 + NN0
              P1 = P1 + NN0*DIM
              P2 = P2 + NN1
              P3 = P3 + NN1*DIM
              P4 = P4 + NN2*DIM

 80          CONTINUE

             CALL ARLTE(DIM,PG,NG,F1,DF1,NN1,FG,DF2,NN2,L1,L2)

            ENDIF

C ------- INTEGRATION PAR SOUS-MAILLES

          ELSE

            CALL INTMAM(DIM,NH,TM1,NO1,NN1,H1,TM2,NO2,NN2,H2,
     &                  ZR(Z0),ZI(Z1),ZL(Z2),NT)

            NN = NT.GT.0
            P  = Z1

            DO 90 K = 1, NT

C ----------- CALCUL DU VOLUME DU TRIANGLE OU TETRAEDRE

             IF (DIM.EQ.2) THEN

               P0 = Z0+2*ZI(P)   - 2
               P1 = Z0+2*ZI(P+1) - 2
               P2 = Z0+2*ZI(P+2) - 2

               R = ABS(PROVE2(ZR(P0),ZR(P1),ZR(P2)))

             ELSE

               P0 = Z0+3*ZI(P)   - 3
               P1 = Z0+3*ZI(P+1) - 3
               P2 = Z0+3*ZI(P+2) - 3
               P3 = Z0+3*ZI(P+3) - 3

               G1(1) = ZR(P3)   - ZR(P0)
               G1(2) = ZR(P3+1) - ZR(P0+1)
               G1(3) = ZR(P3+2) - ZR(P0+2)

               CALL PROVE3(ZR(P0),ZR(P1),ZR(P2),G2)
               R = ABS(DDOT(3,G1,1,G2,1))

             ENDIF

C ----------- CALCUL DES FNCT FORME ET DERIVEES POUR LES DEUX MAILLES

             P0 = 1
             P1 = 1
             P2 = 1
             P3 = 1
             P4 = 1

             DO 100 N = 1, NG

              CALL MTPROD(ZR(Z0),DIM,0,DIM,ZI(P),NN0,FG(P0),1,0,1,0,G)

              PG(N) = PP(N) * R

              PREC = H1*PREC0
              CALL REFERE(G,NO1,DIM,TM1,PREC,IMAX,.TRUE.,G1,IR,F1(P1))
              IF (.NOT.IR) THEN
                  IRET=1
                  GOTO 140
              ENDIF

              PREC = H2*PREC0
              CALL REFERE(G,NO2,DIM,TM2,PREC,IMAX,.TRUE.,G2,IR,F2(P2))
              IF (.NOT.IR) THEN
                  IRET=1
                  GOTO 140
              ENDIF

              CALL FORME1(G1,TM1,DF1(P3),NN1,DIM)
              CALL MTPROD(NO1,DIM,0,DIM,0,NN1,DF1(P3),DIM,0,DIM,0,G)
              CALL MGAUSS('TFVP',G,DF1(P3),DIM,DIM,NN1,R8BID,IRET)

              CALL FORME1(G2,TM2,DF2(P4),NN2,DIM)
              CALL MTPROD(NO2,DIM,0,DIM,0,NN2,DF2(P4),DIM,0,DIM,0,G)
              CALL MGAUSS('TFVP',G,DF2(P4),DIM,DIM,NN2,R8BID,IRET)

              P0 = P0 + NN0
              P1 = P1 + NN1
              P2 = P2 + NN2
              P3 = P3 + DIM*NN1
              P4 = P4 + DIM*NN2

 100         CONTINUE

             CALL ARLTE(DIM,PG,NG,F1,DF1,NN1,F2,DF2,NN2,L1,L2)

             P = P + DIM + 1

 90         CONTINUE

          ENDIF

          IF (.NOT.NN) GOTO 50

C ------- ASSEMBLAGE

          APP(IM1) = .TRUE.
          APP(IM2) = .TRUE.

          CALL ARLAS0(IM1,IM1,ZI(A2),ZI(A3),ZI(E0),NC,ZI(E1),ZI(E2),I1)
          CALL ARLAS0(IM1,IM2,ZI(A2),ZI(A3),ZI(E0),NC,ZI(E4),ZI(E5),I2)

          GOTO (110,120,130) IAS

          CALL ARLAS1(DIM,L,NN1,NN1,I1,L1,ZR(E3))
          CALL ARLAS1(DIM,L,NN1,NN2,I2,L2,ZR(E6))
          GOTO 50

 110      CONTINUE
          P1 = A2-1+ZI(A3-1+IM1)
          CALL ARLAS4(DIM,L,ZR(A4),ZR(A5),ZI(P1),
     &                NN1,ZI(P1),NN1,I1,L1,ZR(E3))
          CALL ARLAS2(DIM,L,ZR(A5),ZI(P1),NN1,NN2,I2,L2,ZR(E6))
          GOTO 50

 120      CONTINUE
          P2 = A2-1+ZI(A3-1+IM2)
          CALL ARLAS1(DIM,L,NN1,NN1,I1,L1,ZR(E3))
          CALL ARLAS3(DIM,L,ZR(A4),NN1,ZI(P2),NN2,I2,L2,ZR(E6))
          GOTO 50

 130      CONTINUE
          P1 = A2-1+ZI(A3-1+IM1)
          P2 = A2-1+ZI(A3-1+IM2)
          CALL ARLAS4(DIM,L,ZR(A4),ZR(A5),ZI(P1),
     &                NN1,ZI(P1),NN1,I1,L1,ZR(E3))
          CALL ARLAS4(DIM,L,ZR(A4),ZR(A5),ZI(P1),
     &                NN1,ZI(P2),NN2,I2,L2,ZR(E6))

 50     CONTINUE

 30   CONTINUE

      IRET = 0

 140  CONTINUE

      IF (IRET.NE.0) THEN
        CALL INFMAJ()
        CALL INFNIV(I,J)
        WRITE(I,*) '   <F> POUR LE COUPLE ',IM1,IM2
        CALL UTMESS('F','ARLCPL','MAILLE NON CONFORME')
      ENDIF

      CALL JEDEMA()

      END
