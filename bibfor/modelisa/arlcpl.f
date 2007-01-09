      SUBROUTINE ARLCPL(MAIL  ,NOMARL,TYPMAI,QUADRA,NOMC  ,
     &                  NOM1  ,NOM2  ,CINE1 ,CINE2 ,NORM  ,
     &                  TANG  ,LCARA)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT NONE
      CHARACTER*16 TYPMAI
      CHARACTER*8  MAIL
      CHARACTER*8  NOMARL      
      CHARACTER*10 NOM1,NOM2,NOMC
      CHARACTER*10 NORM,TANG     
      CHARACTER*8  CINE1,CINE2      
      REAL*8       LCARA   
      CHARACTER*10 QUADRA
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C CALCUL DES MATRICES DE COUPLAGE ELEMENTAIRE ARLEQUIN
C ASSEMBLAGE DANS LES MATRICES ARLEQUIN MORSES
C
C ----------------------------------------------------------------------
C
C
C IN  MAIL   : NOM DU MAILLAGE
C IN  NOM1   : NOM DE LA SD DE STOCKAGE PREMIER GROUPE 
C IN  NOM2   : NOM DE LA SD DE STOCKAGE SECOND GROUPE
C IN  NORM   : NOM DE LA SD POUR STOCKAGE DES NORMALES
C IN  TANG   : NOM DE L'OBJET TANGENTES LISSEES
C IN  TYPMAI : SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)
C IN  LCARA  : LONGUEUR CARACTERISTIQUE POUR TERME DE COUPLAGE (PONDERA
C              TION DES TERMES DE COUPLAGE)
C IN  NOMC   : NOM DE LA SD POUR LES MAILLES DE COLLAGE
C IN  QUADRA : SD DES QUADRATURES A CALCULER
C IN  CINE1  : CINEMATIQUE DU PREMIER GROUPE
C IN  CINE2  : CINEMATIQUE DU SECOND GROUPE
C
C
C SD DE SORTIE
C NOM*.MORSE.VALE : VECTEUR DE VALEUR DE LA MATRICE ARLEQUIN MORSE
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXATR
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
      INTEGER       NNM,NGM
      PARAMETER     (NNM = 27)
      PARAMETER     (NGM = 64)
C
      REAL*8        PRECCP
      INTEGER       ITEMCP
C      
      REAL*8        DDOT,PROVE2,ARLGER
      INTEGER       ARLGEI
C      
      CHARACTER*24  NOMCOL,NGRMA1,NGRMA2
      CHARACTER*16  NOMMO1,NOMMO2,NOMBO1,NOMBO2
      CHARACTER*8   TM0,TYPEM1,TYPEM2,K8BID
      INTEGER       NFAM,NMA,LCOQUE
      INTEGER       JCOLM,JTYPMM,JNORM,JTANG
      INTEGER       NUM1,NUM2
      INTEGER       DIME,NG,NT,NC,NN0,NN1,NN2,MA1,MA2
      INTEGER       IAS,J,K,N
      INTEGER       IFAM,IMA
      INTEGER       IJ1(NNM*NNM),IJ2(NNM*NNM)
      REAL*8        PP(NGM),PG(NGM),FG(NGM*NNM),F1(NGM*NNM),F2(NGM*NNM)
      REAL*8        DF1(3*NGM*NNM),DF2(3*NGM*NNM),DFG(3*NGM*NNM)
      REAL*8        NO1(3*NNM),NO2(3*NNM) 
      REAL*8        G(3*NGM),L1(10*NNM*NNM),L2(10*NNM*NNM)           
      INTEGER       A0,A1,A2,A3
      INTEGER       P,P0,P1,P2,P3,P4,B0,B1,C0,C1,IRET
      INTEGER       Z0,Z1,Z2
      INTEGER       E0,E1,E2,E3,E4,E5,E6
      INTEGER       D0,D1,D2,D3,D4,D5,D6
      REAL*8        G1(3),G2(3)
      REAL*8        PREC,H1,H2,R,R8BID
      LOGICAL       NN,PROJOK
      INTEGER       IFM,NIV
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV) 
C
C --- INITIALISATIONS
C 
      CALL JEVEUO(TYPMAI,'L',JTYPMM)
C
C --- PARAMETRES DE NEWTON POUR PROJECTION POINT SUR MAILLE DE REFERENCE
C
      PRECCP = ARLGER(NOMARL,'PRECCP')
      ITEMCP = ARLGEI(NOMARL,'ITEMCP')
C
C --- INITIALISATION COLLAGE
C      
      NOMCOL = NOMC(1:10)//'.MAILLE'
      CALL JEVEUO(NOMCOL(1:24),'E',JCOLM)
      CALL JELIRA(NOMCOL(1:24),'LONMAX',NMA,K8BID)
      DO 10 IMA = 1, NMA
        ZL(JCOLM-1+IMA) = .FALSE.
 10   CONTINUE
C
C --- NUMERO D'ASSEMBLAGE
C
      IAS = 0
      IF (CINE1.EQ.'COQUE   ') IAS = IOR(IAS,1)
      IF (CINE2.EQ.'COQUE   ') IAS = IOR(IAS,2)
C
C --- LECTURE DONNEES MAILLAGE
C
      CALL JEVEUO(MAIL(1:8)//'.TYPMAIL','L',A0)
      CALL JEVEUO(MAIL(1:8)//'.COORDO    .VALE','L',A1)
      CALL JEVEUO(MAIL(1:8)//'.CONNEX','L',A2)
      CALL JEVEUO(JEXATR(MAIL(1:8)//'.CONNEX','LONCUM'),'L',A3)
C
C --- LECTURE DONNEES NORMALES ET TANGENTES
C  
      IF (IAS.NE.0) THEN
        CALL JEVEUO(NORM,'L',JNORM)
        CALL JEVEUO(TANG,'L',JTANG)
      ELSE
        JNORM = A1
      ENDIF
C
C --- LECTURE DONNEES GROUPE DE MAILLES
C
      NGRMA1 = NOM1(1:10)//'.GROUPEMA'
      NGRMA2 = NOM2(1:10)//'.GROUPEMA'      
      CALL JEVEUO(NGRMA1,'L',B0)
      CALL JEVEUO(NGRMA2,'L',C0)      
C
C --- LECTURE DONNEES BOITES APPARIEMENT
C      
      NOMBO1 = NOM1(1:10)//'.BOITE'
      NOMBO2 = NOM2(1:10)//'.BOITE'      
      CALL JEVEUO(NOMBO1(1:16)//'.H','L',B1)
      CALL JEVEUO(NOMBO2(1:16)//'.H','L',C1)
C
C --- LECTURE DONNEES QUADRATURES
C 
      CALL JELIRA(QUADRA(1:10)//'.NUMERO','LONMAX',NFAM,K8BID)
      CALL JEVEUO(QUADRA(1:10)//'.NUMERO','L',D0)
      CALL JEVEUO(QUADRA(1:10)//'.TYPEMA','L',D1)
      CALL JEVEUO(QUADRA(1:10)//'.LIMAMA','L',D2)
      CALL JEVEUO(JEXATR(QUADRA(1:10)//'.LIMAMA','LONCUM'),'L',D3)
      CALL JEVEUO(QUADRA(1:10)//'.MAMA','L',D4)
C
C --- LECTURE DONNEES ZONE COLLAGE
C 
      CALL JEVEUO(NOMC(1:10)//'.INO','L',E0)
      CALL JELIRA(NOMC(1:10)//'.INO','LONMAX',NC,K8BID)
C
C --- LECTURE DONNEES MATRICES MORSES
C
      NOMMO1 = NOM1(1:10)//'.MORSE'
      CALL JEVEUO(NOMMO1(1:16)//'.INO','L',E1)
      CALL JELIRA(NOMMO1(1:16)//'.INO','LONT',NN1,K8BID)
      CALL JEVEUO(JEXATR(NOMMO1(1:16)//'.INO','LONCUM'),'L',E2)
      CALL JEVEUO(NOMMO1(1:16)//'.DIME','L',E3)
      NN1 = NN1*ZI(E3)
      CALL JEVEUO(NOMMO1(1:16)//'.VALE','E',E3)
C
      NOMMO2 = NOM2(1:10)//'.MORSE'
      CALL JEVEUO(NOMMO2(1:16)//'.INO','L',E4)
      CALL JELIRA(NOMMO2(1:16)//'.INO','LONT',NN2,K8BID)
      CALL JEVEUO(JEXATR(NOMMO2(1:16)//'.INO','LONCUM'),'L',E5)
      CALL JEVEUO(NOMMO2(1:16)//'.DIME','L',E6)
      NN2 = NN2*ZI(E6)
      CALL JEVEUO(NOMMO2(1:16)//'.VALE','E',E6)
C
C --- LECTURE DONNEES TEMPORAIRES ARLEQUIN
C
      CALL JEVEUO(NOMARL(1:8)//'.TRAVR','E',Z0)
      CALL JEVEUO(NOMARL(1:8)//'.TRAVI','E',Z1)
      CALL JEVEUO(NOMARL(1:8)//'.TRAVL','E',Z2)
C
C --- INTEGRATION
C
      D6 = ZI(D3)
C
      DO 30 IFAM = 1, NFAM
C
C --- FORMULE D'INTEGRATION DE LA FAMILLE
C
        TM0 = ZK8(D1)
        CALL PGAUSS(TM0,ZI(D0),PP,G,NG,DIME)
C
C --- FONCTIONS DE FORME ET DERIVEES PREMIERES DE LA FAMILLE
C
        P0 = 1
        P1 = 1
        P2 = 1
        DO 40 J = 1, NG
          CALL FORME0(G(P0),TM0,FG(P1) ,NN0)
          CALL FORME1(G(P0),TM0,DFG(P2),NN0,DIME)
          P0 = P0 + DIME
          P1 = P1 + NN0
          P2 = P2 + NN0*DIME
 40     CONTINUE
        D0 = D0 + 1
        D1 = D1 + 1
C
C --- COUPLE DE MAILLES DE LA FAMILLE
C
        D5 = D6
        D6 = ZI(D3+IFAM)
        DO 50 J = D5, D6-1
C
          P0 = D4 + 2*(ZI(D2-1+J)-1)
C
C --- INFORMATIONS SUR LE COUPLE DE MAILLE
C     
          MA1    = ZI(P0)
          NUM1   = ZI(B0-1+ABS(MA1))
          H1     = ZR(B1-1+ABS(MA1))
          TYPEM1 = ZK8(JTYPMM+ZI(A0-1+NUM1)-1)  
          MA2    = ZI(P0+1)
          NUM2   = ZI(C0-1+ABS(MA2))
          H2     = ZR(C1-1+ABS(MA2))
          TYPEM2 = ZK8(JTYPMM+ZI(A0-1+NUM2)-1)
C               
C --- COORDONNEES DES SOMMETS DU COUPLE DE MAILLE        
C       
          CALL TMACOQ(TYPEM1,DIME,LCOQUE)
          CALL CONOEU(NUM1,ZI(A2),ZI(A3),ZR(A1),ZR(JNORM),
     &                DIME,LCOQUE,NO1,NN1)
          CALL TMACOQ(TYPEM2,DIME,LCOQUE)
          CALL CONOEU(NUM2,ZI(A2),ZI(A3),ZR(A1),ZR(JNORM),
     &                DIME,LCOQUE,NO2,NN2)        
C
C --- MISE A ZERO DES MATRICES DE COUPLAGE ELEMENTAIRES
C
          DO 60 K = 1, 10*NNM*NNM
            L1(K) = 0.D0
            L2(K) = 0.D0
 60       CONTINUE

          PROJOK = .TRUE.
C
C --- INTEGRATION STANDARD
C
          IF (MA2.GT.0) THEN

            P0 = 1
            P1 = 1
            P2 = 1
            P3 = 1
            P4 = 1

            NN = .TRUE.
C
C --- INTEGRATION SUR MA1
C
            IF (MA1.GT.0) THEN

             DO 70 K = 1, NG

              CALL DCOPY (DIME*NN1,DFG(P1),1,DF1(P3),1)
              CALL MTPROD(NO1    ,DIME,0,DIME,0,NN1,
     &                    DF1(P3),DIME,0,DIME,0,
     &                    G)
              CALL MGAUSS('TCVD',G,DF1(P3),DIME,DIME,
     &                    NN1,R,IRET)
     
              IF (IRET.NE.0) THEN
                GOTO 140
              ENDIF
              
              PG(K) = PP(K) * ABS(R)

              PREC = H2*PRECCP
              CALL MTPROD(NO1,DIME,0,DIME,0,NN1,
     &                    FG(P0),1,0,1,0,
     &                    G)
              CALL REFERE(G,NO2,DIME,TYPEM2,PREC,ITEMCP,.TRUE.,
     &                    G2,PROJOK,F2(P2))
     
              IF (.NOT.PROJOK) THEN
                IRET = 2
                GOTO 140
              ENDIF

              CALL FORME1(G2,TYPEM2,DF2(P4),NN2,DIME)
              CALL MTPROD(NO2,DIME,0,DIME,0,
     &                    NN2,DF2(P4),DIME,0,DIME,0,G)
              CALL MGAUSS('TFVP',G,DF2(P4),DIME,DIME,
     &                    NN2,R8BID,IRET)

              P0 = P0 + NN0
              P1 = P1 + NN0*DIME
              P2 = P2 + NN2
              P3 = P3 + NN1*DIME
              P4 = P4 + NN2*DIME

 70          CONTINUE
C
C --- MATRICE ELEMENTAIRE
C
             CALL ARLTE(DIME  ,NG    ,PG    ,
     &                  FG    ,DF1   ,NN1   ,L1    ,
     &                  F2    ,DF2   ,NN2   ,L2)
C
C --- INTEGRATION SUR MA2
C
            ELSE

             DO 80 K = 1, NG

              CALL DCOPY (DIME*NN2,DFG(P1),1,DF2(P4),1)
              CALL MTPROD(NO2,DIME,0,DIME,0,
     &                    NN2,DF2(P4),DIME,0,DIME,0,G)
              CALL MGAUSS('TCVD',G,DF2(P4),DIME,DIME,NN2,R,IRET)
             
              IF (IRET.NE.0) THEN
                GOTO 140
              ENDIF
              
              PG(K) = PP(K) * ABS(R)

              PREC = H1*PRECCP
              CALL MTPROD(NO2,DIME,0,DIME,0,NN2,FG(P0),1,0,1,0,G)
              CALL REFERE(G,NO1,DIME,TYPEM1,PREC,ITEMCP,.TRUE.,
     &                    G1,PROJOK,F1(P2))
     
              IF (.NOT.PROJOK) THEN
                IRET = 2
                GOTO 140
              ENDIF

              CALL FORME1(G1,TYPEM1,DF1(P3),NN1,DIME)
              CALL MTPROD(NO1,DIME,0,DIME,0,NN1,DF1(P3),DIME,0,DIME,0,G)
              CALL MGAUSS('TFVP',G,DF1(P3),DIME,DIME,NN1,R8BID,IRET)

              P0 = P0 + NN0
              P1 = P1 + NN0*DIME
              P2 = P2 + NN1
              P3 = P3 + NN1*DIME
              P4 = P4 + NN2*DIME

 80          CONTINUE
C
C --- MATRICE ELEMENTAIRE
C
             CALL ARLTE(DIME  ,NG    ,PG    ,
     &                  F1    ,DF1   ,NN1   ,L1    ,
     &                  FG    ,DF2   ,NN2   ,L2)   
     
            ENDIF          
C            
C --- INTEGRATION PAR SOUS-MAILLES
C
          ELSE     
C
C --- CALCUL DE L'INTERSECTION
C
            CALL INTMAM(DIME  ,NOMARL,
     &                  TYPEM1,NO1,NN1,H1,
     &                  TYPEM2,NO2,NN2,H2,
     &                  ZR(Z0),ZI(Z1),ZL(Z2),NT)           
            NN = NT.GT.0
            P  = Z1
            DO 90 K = 1, NT
C
C --- CALCUL DU VOLUME DU TRIANGLE OU TETRAEDRE
C
             IF (DIME.EQ.2) THEN
               P0 = Z0+2*ZI(P)   - 2
               P1 = Z0+2*ZI(P+1) - 2
               P2 = Z0+2*ZI(P+2) - 2
               R  = ABS(PROVE2(ZR(P0),ZR(P1),ZR(P2)))
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
C             
C --- CALCUL DES FNCT FORME ET DERIVEES POUR LES DEUX MAILLES
C
             P0 = 1
             P1 = 1
             P2 = 1
             P3 = 1
             P4 = 1

             DO 100 N = 1, NG

              CALL MTPROD(ZR(Z0),DIME  ,0,DIME,ZI(P),
     &                    NN0   ,FG(P0),1,0   ,1    ,
     &                    0     ,G)

              PG(N) = PP(N) * R

              PREC = H1*PRECCP
              CALL REFERE(G,NO1,DIME,TYPEM1,PREC,ITEMCP,.TRUE.,
     &                    G1,PROJOK,F1(P1))
              IF (.NOT.PROJOK) THEN
                IRET=1
                GOTO 140
              ENDIF

              PREC = H2*PRECCP
              CALL REFERE(G,NO2,DIME,TYPEM2,PREC,ITEMCP,.TRUE.,
     &                    G2,PROJOK,F2(P2))
              IF (.NOT.PROJOK) THEN
                IRET=1
                GOTO 140
              ENDIF

              CALL FORME1(G1,TYPEM1,DF1(P3),NN1,DIME)
              CALL MTPROD(NO1,DIME,0,DIME,0,NN1,DF1(P3),DIME,0,DIME,0,G)
              CALL MGAUSS('TFVP',G,DF1(P3),DIME,DIME,NN1,R8BID,IRET)

              CALL FORME1(G2,TYPEM2,DF2(P4),NN2,DIME)
              CALL MTPROD(NO2,DIME,0,DIME,0,NN2,DF2(P4),DIME,0,DIME,0,G)
              CALL MGAUSS('TFVP',G,DF2(P4),DIME,DIME,NN2,R8BID,IRET)

              P0 = P0 + NN0
              P1 = P1 + NN1
              P2 = P2 + NN2
              P3 = P3 + DIME*NN1
              P4 = P4 + DIME*NN2

 100         CONTINUE
C
C --- MATRICE ELEMENTAIRE
C
             CALL ARLTE(DIME  ,NG    ,PG    ,
     &                  F1    ,DF1   ,NN1   ,L1    ,
     &                  F2    ,DF2   ,NN2   ,L2)

             P = P + DIME + 1

 90         CONTINUE
          ENDIF
C
C --- DEBOGUAGE
C
          IF (NIV.GE.2) THEN
            CALL ARLTIM(6     ,DIME  ,NNM   ,MA2.GT.0,MA1.GT.0,
     &                  NUM1  ,NUM2  ,L1    ,L2    ,TYPEM1,
     &                  TYPEM2,H1    ,H2    ,NG    ,NT)
          ENDIF

          IF (.NOT.NN) THEN
            GOTO 50
          ENDIF
C          
C --- ASSEMBLAGE DES MATRICES ELEMENTAIRES
C
          ZL(JCOLM+NUM1-1) = .TRUE.
          ZL(JCOLM+NUM2-1) = .TRUE.

          CALL ARLAS0(NUM1  ,NUM1  ,ZI(A2) ,ZI(A3),
     &                ZI(E0),NC    ,ZI(E1) ,ZI(E2),
     &                IJ1)
          CALL ARLAS0(NUM1  ,NUM2  ,ZI(A2) ,ZI(A3),
     &                ZI(E0),NC    ,ZI(E4) ,ZI(E5),
     &                IJ2)

          GOTO (110,120,130) IAS

          CALL ARLAS1(DIME  ,LCARA ,NN1   ,NN1   ,IJ1   ,
     &                L1    ,ZR(E3))
          CALL ARLAS1(DIME  ,LCARA ,NN1   ,NN2   ,IJ2   ,
     &                L2    ,ZR(E6))
          GOTO 50

 110      CONTINUE
 
          P1 = A2-1+ZI(A3-1+NUM1)   
            
          CALL ARLAS4(DIME  ,LCARA ,NN1   ,NN1      ,IJ1 ,
     &                ZI(P1),ZI(P1),ZR(JNORM),ZR(JTANG),L1  ,
     &                ZR(E3))
          CALL ARLAS2(DIME  ,LCARA ,NN1   ,NN2   ,IJ2   ,
     &                ZI(P1),ZR(JTANG),L2,ZR(E6))
          GOTO 50

 120      CONTINUE
 
          P2 = A2-1+ZI(A3-1+NUM2)
           
          CALL ARLAS1(DIME  ,LCARA ,NN1   ,NN1   ,IJ1   ,
     &                L1    ,ZR(E3))
          CALL ARLAS3(DIME  ,LCARA ,NN1   ,NN2   ,IJ2   ,
     &                ZI(P2),ZR(JNORM),L2,ZR(E6))
          GOTO 50

 130      CONTINUE
 
          P1 = A2-1+ZI(A3-1+NUM1)
          P2 = A2-1+ZI(A3-1+NUM2)
    
          CALL ARLAS4(DIME  ,LCARA ,NN1      ,NN1      ,IJ1 ,
     &                ZI(P1),ZI(P1),ZR(JNORM),ZR(JTANG),L1  ,
     &                ZR(E3))     

          CALL ARLAS4(DIME  ,LCARA ,NN1      ,NN2      ,IJ2 ,
     &                ZI(P1),ZI(P2),ZR(JNORM),ZR(JTANG),L2  ,
     &                ZR(E6))

 50     CONTINUE

 30   CONTINUE

      IRET = 0

 140  CONTINUE
C
      IF (IRET.NE.0) THEN
        WRITE(6,*) '   <F> POUR LE COUPLE ',NUM1,NUM2
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()

      END
