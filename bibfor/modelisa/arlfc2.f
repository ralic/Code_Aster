      SUBROUTINE ARLFC2(MAIL  ,DIME  ,NOM1  ,CINE1 ,NOM2  ,
     &                  CINE2 ,NOMC  ,NOMARL)
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
C
      IMPLICIT NONE
      CHARACTER*10  NOM1,NOM2,NOMC
      CHARACTER*8   MAIL,CINE1,CINE2,NOMARL 
      INTEGER       DIME
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C ALLOCATION ET ECRITURE DES POINTEURS DES MATRICES DE COUPLAGE ARLEQUIN
C LORSQUE LE GROUPE DE MAILLE DE COLLAGE N'EST PAS CELUI DES MULTIPL.
C
C ----------------------------------------------------------------------
C
C
C IN  MAIL   : NOM UTILISATEUR DU MAILLAGE
C IN  DIME   : DIMENSION DE L'ESPACE GLOBAL
C IN  NOM1   : NOM DE LA SD POUR LES MAILLES GROUPE 1 
C IN  NOM2   : NOM DE LA SD POUR LES MAILLES GROUPE 2 
C IN  CINE1  : CINEMATIQUE DU PREMIER GROUPE DE MAILLE
C IN  CINE2  : CINEMATIQUE DU DEUXIEME GROUPE DE MAILLE
C IN  NOMARL : NOM DE LA SD PRINCIPALE ARLEQUIN
C I/O NOMC   : NOM DE LA SD POUR LE COLLAGE 
C
C
C SD DE SORTIE
C NOMC.INO        : LISTE TRIEE DES NOEUDS LIGNE DE LA MATRICE MORSE
C                   (NO1, NO2, NO3, ... ) AVEC NO1 < NO2 < NO3 < ...
C NOM*.MORSE.DIME : DIMENSION DE LA MATRICE NODALE (* = 1, 2)
C NOM*.MORSE.INO  : LISTES TRIEES DES NOEUDS COLONNES MATRICE MORSE
C                   (XC V I NUMERO VARIABLE)
C                 [LIGNE 1] (NO1.1, NO1.2, ...) AVEC NO1.1 < NO1.2 < ...
C                 [LIGNE 2] (NO2.1, NO2.2, ...) AVEC NO2.1 < NO2.2 < ...
C                    ...
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM , JEXATR
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
      CHARACTER*16  NOMMO1,NOMMO2
      CHARACTER*24  NOMAPP      
      CHARACTER*8   K8BID
      INTEGER       DIM1,DIM2,NNO,NN1,NN2,NNC,NM2,N1,N2,M1,M2
      INTEGER       A0,A1,B0,B1,B2,B3,B4,C0,C1,C2,D0,D1,D2,D3,D4,D5
      INTEGER       E0,E1,E2,I,J,K,P0,P1,P2,P3,P4,P5
      INTEGER       JCOLM
      LOGICAL       IR
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C  
C --- INITIALISATIONS  
C 
      CALL JEVEUO(NOMC(1:10)//'.MAILLE','E',JCOLM)
C
C --- NOMS OBJETS MORSE
C
      NOMMO1 = NOM1(1:10)//'.MORSE'
      NOMMO2 = NOM2(1:10)//'.MORSE'
C
C --- DIMENSION DES MATRICES NODALES ARLEQUIN
C
      IF (CINE2(1:6).EQ.'SOLIDE') THEN
          DIM2 = 5*DIME-6
        IF (CINE1(1:6).EQ.'SOLIDE') THEN
          DIM1 = DIM2
        ELSEIF (CINE2(1:5).EQ.'COQUE') THEN
          DIM1 = 12*DIME-18
        ELSE
          CALL ASSERT(.FALSE.)          
        ENDIF
      ELSEIF (CINE2(1:5).EQ.'COQUE') THEN
          DIM2 = 21*DIME-33
        IF (CINE1(1:6).EQ.'SOLIDE') THEN
          DIM1 = 9*DIME-12
        ELSEIF (CINE1(1:5).EQ.'COQUE') THEN
          DIM1 = DIM2
        ELSE
          CALL ASSERT(.FALSE.)  
        ENDIF          
      ELSE
        CALL ASSERT(.FALSE.) 
      ENDIF
C
C --- LECTURE DONNEES MAILLAGE
C
      CALL JEVEUO(MAIL(1:8)//'.CONNEX','L',A0)
      CALL JEVEUO(JEXATR(MAIL(1:8)//'.CONNEX','LONCUM'),'L',A1)
C
C --- LECTURE DONNEES GROUPE DE MAILLES
C  
      CALL JEVEUO(NOM1(1:10)//'.GROUPEMA','L',B0)
      CALL JEVEUO(NOM2(1:10)//'.GROUPEMA','L',C0)
      CALL JELIRA(NOM2(1:10)//'.GROUPEMA','LONMAX',NM2,K8BID)      
C
C --- LECTURE CONNECTIVITES INVERSES
C          
      CALL JEVEUO(NOM1(1:10)//'.CNCINV','L',B1)
      CALL JELIRA(NOM1(1:10)//'.CNCINV','NMAXOC',NNO,K8BID)
      CALL JEVEUO(JEXATR(NOM1(1:10)//'.CNCINV','LONCUM'),'L',B2)
      CALL JEVEUO(NOM2(1:10)//'.CNCINV','L',C1)
      CALL JEVEUO(JEXATR(NOM2(1:10)//'.CNCINV','LONCUM'),'L',C2)
C
C --- LECTURE GRAPHE APPARIEMENT
C     
      NOMAPP = NOMARL(1:8)//'.GRAPH'     
      CALL JEVEUO(NOMAPP,'L',B3)
      CALL JEVEUO(JEXATR(NOMAPP,'LONCUM'),'L',B4)      
C
C --- ALLOCATION OBJETS TEMPORAIRES
C
      CALL WKVECT('&&ARLFC2.FILTRE.N1','V V L',NNO,D0)
      CALL WKVECT('&&ARLFC2.COMPTEUR1','V V I',NNO,D1)
      CALL WKVECT('&&ARLFC2.COMPTEUR2','V V I',NNO,D2)
      CALL WKVECT('&&ARLFC2.FILTRE.MA','V V L',NM2,D3)
      CALL WKVECT('&&ARLFC2.FILTRE.MD','V V L',NM2,D4)
      CALL WKVECT('&&ARLFC2.FILTRE.N2','V V L',NNO,D5)

      DO 10 I = 1, NNO
        ZL(D5-1+I) = .FALSE.
 10   CONTINUE

      DO 20 I = 1, NM2
        ZL(D4-1+I) = .TRUE.
 20   CONTINUE

      NN1 = 0
      NN2 = 0
      NNC = 0

C --- DIMENSIONS DES MATRICES

      P1 = ZI(B2)
      IR = .TRUE.

      DO 30 N1 = 1, NNO

        P0 = P1
        P1 = ZI(B2+N1)
        IF (ZI(B1-1+P0).EQ.0) GOTO 30
        ZL(D5-1+N1) = .TRUE.

        IF (IR) THEN

          IR = .FALSE.

          DO 40 M2 = 1, NM2
            ZL(D3-1+M2) = .FALSE.
 40       CONTINUE

          DO 50 N2 = 1, NNO
            ZL(D0-1+N2) = .FALSE.
 50       CONTINUE

        ENDIF

        DO 60 I = P0, P1-1

          M1 = ZI(B1-1+I)
          IF (.NOT.ZL(JCOLM+M1-1)) GOTO 60
          P2 = ZI(B4-1+M1)
          P3 = ZI(B4+M1)

          DO 70 J = P2, P3-1

            M2 = ZI(B3-1+J)
            IF (ZL(D3-1+M2)) GOTO 70
            ZL(D3-1+M2) = .TRUE.
            ZL(D4-1+M2) = .FALSE.
            K = ZI(C0-1+M2)
            P4 = ZI(A1-1+K)
            P5 = ZI(A1+K)

            DO 80 K = P4, P5-1

              N2 = ZI(A0-1+K)
              IF (ZL(D0-1+N2)) GOTO 80
              ZL(D0-1+N2) = .TRUE.
              ZI(D1-1+N2) = ZI(D1-1+N2) + 1
              NN1 = NN1 + 1
              IR = .TRUE.

 80         CONTINUE

 70       CONTINUE

 60     CONTINUE

 30   CONTINUE

      P1 = ZI(C2)

      DO 90 N2 = 1, NNO

        P0 = P1
        P1 = ZI(C2+N2)
        IF (ZI(D1-1+N2).EQ.0) GOTO 90
        NNC = NNC + 1

        DO 100 N1 = 1, NNO
          ZL(D0-1+N1) = .FALSE.
 100    CONTINUE

        DO 110 I = P0, P1-1

          M2 = ZI(C1-1+I)
          IF (ZL(D4-1+M2)) GOTO 110
          J = ZI(C0-1+M2)
          P2 = ZI(A1-1+J)
          P3 = ZI(A1+J)

          DO 120 J = P2, P3-1

            N1 = ZI(A0-1+J)
            IF (ZL(D0-1+N1)) GOTO 120
            ZL(D0-1+N1) = .TRUE.

            IF (ZL(D5-1+N1)) THEN
              WRITE(6,*) 'NOEUD REDONDANT : ',N1
              CALL ASSERT(.FALSE.)
            ENDIF

            ZI(D2-1+N2) = ZI(D2-1+N2) + 1
            NN2 = NN2 + 1

 120      CONTINUE

 110    CONTINUE

  90  CONTINUE

C
C --- ALLOCATION OBJET MORSES
C
      CALL ARLCMO(NOMMO1,'V',NN1,NNC,DIM1)
      CALL ARLCMO(NOMMO2,'V',NN2,NNC,DIM2) 
      CALL JEVEUO(NOMMO1(1:16)//'.INO','E',E1)
      CALL JEVEUO(NOMMO2(1:16)//'.INO','E',E2)
C
C --- ALLOCATION OBJET INO
C
      CALL WKVECT(NOMC(1:10)//'.INO','V V I',NNC,E0)      
C
      NNC = 0
      NN1 = 1
      NN2 = 1

      DO 130 I = 1, NNO
        IF (ZI(D2-1+I).NE.0) THEN
          ZI(E0+NNC) = I
          NNC = NNC + 1
          N1 = ZI(D1-1+I)
          CALL JECROC(JEXNUM(NOMMO1(1:16)//'.INO',NNC))
          CALL JEECRA(JEXNUM(NOMMO1(1:16)//'.INO',NNC),'LONMAX',N1,' ')
          ZI(D1-1+I) = NN1
          NN1 = NN1 + N1
          N2 = ZI(D2-1+I)
          CALL JECROC(JEXNUM(NOMMO2(1:16)//'.INO',NNC))
          CALL JEECRA(JEXNUM(NOMMO2(1:16)//'.INO',NNC),'LONMAX',N2,' ')
          ZI(D2-1+I) = NN2
          NN2 = NN2 + N2
        ENDIF
 130  CONTINUE
C
C --- ECRITURE DU PROFIL DES MATRICES
C
      P1 = ZI(B2)

      DO 140 N1 = 1, NNO

        P0 = P1
        P1 = ZI(B2+N1)
        IF (ZI(B1-1+P0).EQ.0) GOTO 140

        DO 150 M2 = 1, NM2
          ZL(D3-1+M2) = .FALSE.
 150    CONTINUE

        DO 160 N2 = 1, NNO
          ZL(D0-1+N2) = .FALSE.
 160    CONTINUE

        DO 170 I = P0, P1-1

          M1 = ZI(B1-1+I)
          IF (.NOT.ZL(JCOLM+M1-1)) GOTO 170
          P2 = ZI(B4-1+M1)
          P3 = ZI(B4+M1)

          DO 180 J = P2, P3-1

            M2 = ZI(B3-1+J)
            IF (ZL(D3-1+M2)) GOTO 180
            ZL(D3-1+M2) = .TRUE.
            K = ZI(C0-1+M2)
            P4 = ZI(A1-1+K)
            P5 = ZI(A1+K)

            DO 190 K = P4, P5-1

              N2 = ZI(A0-1+K)
              IF (ZL(D0-1+N2)) GOTO 190
              ZL(D0-1+N2) = .TRUE.
              NN1 = ZI(D1-1+N2)
              ZI(E1-1+NN1) = N1
              ZI(D1-1+N2) = NN1 + 1

 190        CONTINUE

 180      CONTINUE

 170    CONTINUE

 140  CONTINUE

      P1 = ZI(C2)

      DO 200 N2 = 1, NNO

        P0 = P1
        P1 = ZI(C2+N2)
        IF (ZI(D1-1+N2).EQ.0) GOTO 200

        DO 210 N1 = 1, NNO
          ZL(D0-1+N1) = .FALSE.
 210    CONTINUE

        DO 220 I = P0, P1-1

          M2 = ZI(C1-1+I)
          IF (ZL(D4-1+M2)) GOTO 220
          J = ZI(C0-1+M2)
          P2 = ZI(A1-1+J)
          P3 = ZI(A1+J)

          DO 230 J = P2, P3-1

            N1 = ZI(A0-1+J)
            IF (ZL(D0-1+N1)) GOTO 230
            ZL(D0-1+N1) = .TRUE.
            NN2 = ZI(D2-1+N1)
            ZI(E2-1+NN2) = N2
            ZI(D2-1+N1) = NN2 + 1

 230      CONTINUE

 220    CONTINUE

 200   CONTINUE
C
C --- DESALLOCATIONS
C
      CALL JEDETR('&&ARLFC2.FILTRE.N2')
      CALL JEDETR('&&ARLFC2.FILTRE.MD')
      CALL JEDETR('&&ARLFC2.FILTRE.MA')
      CALL JEDETR('&&ARLFC2.COMPTEUR2')
      CALL JEDETR('&&ARLFC2.COMPTEUR1')
      CALL JEDETR('&&ARLFC2.FILTRE.N1')

      CALL JEDEMA()

      END
