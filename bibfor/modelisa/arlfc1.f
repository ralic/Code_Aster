      SUBROUTINE ARLFC1(MAIL,DIME,NOM1Z,CINE1,NOM2Z,CINE2,NOMCZ,COL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C ALLOCATION ET ECRITURE DES POINTEURS DES MATRICES DE COUPLAGE ARLEQUIN
C  LORSQUE LE GROUPE DE MAILLE DE COLLAGE EST CELUI DES MULTIPLICATEURS
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8    MAIL      : SD MAILLAGE
C INTEGER        DIME      : DIMENSION DE L'ESPACE
C CHARACTER*(10) NOM1Z     : SD DOMAINE 1
C CHARACTER*8    CINE1     : CINEMATIQUE DU DOMAINE 1 (CF ARLVER)
C CHARACTER*(10) NOM2Z     : SD DOMAINE 2
C CHARACTER*8    CINE2     : CINEMATIQUE DU DOMAINE 2 (CF ARLVER)
C LOGICAL        COL(*)    : FILTRE DECRIVANT ZONE DE COLLAGE
C                            SOUS-ENSEMBLE DU DOMAINE 1
C CHARACTER*(10) NOMC      : NOM DE LA ZONE DE COLLAGE
C
C SD D'ENTREE
C NOM1.GROUPEMA : LISTE DE MAILLES DOMAINE 1
C NOM1.CNCINV   : SD GRAPHE CONNECTIVITE INVERSE (CF CNCINV)
C NOM1.NOM2     : SD GRAPHE D'APPARIEMENT (CF ARLAPP)
C NOM2.GROUPEMA : LISTE DE MAILLES DOMAINE 2
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

C --- VARIABLES
      CHARACTER*(*) NOM1Z,NOM2Z,NOMCZ
      CHARACTER*16  MORSE1,MORSE2
      CHARACTER*10  NOM1,NOM2,NOMC
      CHARACTER*8   MAIL,CINE1,CINE2
      INTEGER       DIME,DIM1,DIM2,NNO,NN1,NN2,NNC,NM2,N1,N2,M1,M2
      INTEGER       A0,A1,B0,B1,B2,B3,B4,C0,D0,D1,D2,D3,D4,E0,E1,E2
      INTEGER       I,J,K,P0,P1,P2,P3,P4,P5,IFM,NIV
      LOGICAL       COL(*),IR

      NOM1 = NOM1Z
      NOM2 = NOM2Z
      NOMC = NOMCZ

      MORSE1 = NOM1//'.MORSE'
      MORSE2 = NOM2//'.MORSE'

      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C --- DIMENSION DES MATRICES NODALES ARLEQUIN

      IF (CINE1(1:6).EQ.'SOLIDE') THEN
          DIM1 = 5*DIME-6
        IF (CINE2(1:6).EQ.'SOLIDE') THEN
          DIM2 = DIM1
        ELSEIF (CINE2(1:5).EQ.'COQUE') THEN
          DIM2 = 12*DIME-18
        ENDIF
      ELSEIF (CINE1(1:5).EQ.'COQUE') THEN
          DIM1 = 21*DIME-33
        IF (CINE2(1:6).EQ.'SOLIDE') THEN
          DIM2 = 9*DIME-12
        ELSEIF (CINE2(1:5).EQ.'COQUE') THEN
          DIM2 = DIM1
        ENDIF
      ENDIF

C --- LECTURE

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.CONNEX','L',A0)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',A1)

      CALL JEVEUO(NOM1//'.GROUPEMA','L',B0)
      CALL JEVEUO(NOM1//'.CNCINV','L',B1)
      CALL JELIRA(NOM1//'.CNCINV','NMAXOC',NNO,ZK8)
      CALL JEVEUO(JEXATR(NOM1//'.CNCINV','LONCUM'),'L',B2)
      CALL JEVEUO(NOM1//'.'//NOM2,'L',B3)
      CALL JEVEUO(JEXATR(NOM1//'.'//NOM2,'LONCUM'),'L',B4)

      CALL JEVEUO(NOM2//'.GROUPEMA','L',C0)
      CALL JELIRA(NOM2//'.GROUPEMA','LONMAX',NM2,ZK8)

      CALL WKVECT('&&ARLFC1.FILTRE.N1','V V L',NNO,D0)
      CALL WKVECT('&&ARLFC1.COMPTEUR1','V V I',NNO,D1)
      CALL WKVECT('&&ARLFC1.COMPTEUR2','V V I',NNO,D2)
      CALL WKVECT('&&ARLFC1.FILTRE.MA','V V L',NM2,D3)
      CALL WKVECT('&&ARLFC1.FILTRE.N2','V V L',NNO,D4)

      DO 10 I = 1, NNO
        ZI(D1-1+I) = 0
        ZI(D2-1+I) = 0
        ZL(D4-1+I) = .FALSE.
 10   CONTINUE

      NN1 = 0
      NN2 = 0
      NNC = 0

C --- DIMENSIONS DES MATRICES

      P1 = ZI(B2)
      IR = .TRUE.

      DO 20 N1 = 1, NNO

        P0 = P1
        P1 = ZI(B2+N1)
        IF (ZI(B1-1+P0).EQ.0) GOTO 20

        IF (IR) THEN

          IR = .FALSE.

          DO 30 M2 = 1, NM2
            ZL(D3-1+M2) = .FALSE.
 30       CONTINUE

          DO 40 N2 = 1, NNO
            ZL(D0-1+N2) = .FALSE.
 40       CONTINUE

        ENDIF

        DO 50 I = P0, P1-1

          M1 = ZI(B1-1+I)
          IF (.NOT.COL(M1)) GOTO 50

          J = ZI(B0-1+M1)
          P2 = ZI(A1-1+J)
          P3 = ZI(A1+J)

          DO 60 J = P2, P3-1

            N2 = ZI(A0-1+J)
            IF (ZL(D0-1+N2)) GOTO 60
            ZL(D0-1+N2) = .TRUE.
            ZL(D4-1+N2) = .TRUE.
            ZI(D1-1+N1) = ZI(D1-1+N1) + 1
            NN1 = NN1 + 1

 60       CONTINUE

          P2 = ZI(B4-1+M1)
          P3 = ZI(B4+M1)

          DO 70 J = P2, P3-1

            M2 = ZI(B3-1+J)
            IF (ZL(D3-1+M2)) GOTO 70
            ZL(D3-1+M2) = .TRUE.
            K = ZI(C0-1+M2)
            P4 = ZI(A1-1+K)
            P5 = ZI(A1+K)

            DO 80 K = P4, P5-1

              N2 = ZI(A0-1+K)
              IF (ZL(D0-1+N2)) GOTO 80
              ZL(D0-1+N2) = .TRUE.

              IF (ZL(D4-1+N2)) THEN
                WRITE(IFM,*) 'NOEUD REDONDANT : ',N2
                CALL U2MESS('F','MODELISA2_8')
              ENDIF

              ZI(D2-1+N1) = ZI(D2-1+N1) + 1
              NN2 = NN2 + 1
              IR = .TRUE.

 80         CONTINUE

 70       CONTINUE

 50     CONTINUE

        IF (IR) NNC = NNC + 1

 20   CONTINUE

C --- ALLOCATIONS

      CALL JECREO(MORSE1//'.DIME','V E I')
      CALL JEVEUO(MORSE1//'.DIME','E',E0)
      ZI(E0) = DIM1

      CALL JECREO(MORSE2//'.DIME','V E I')
      CALL JEVEUO(MORSE2//'.DIME','E',E0)
      ZI(E0) = DIM2

      CALL WKVECT(NOMC//'.INO','V V I',NNC,E0)

      CALL JECREC(MORSE1//'.INO','V V I','NU','CONTIG','VARIABLE',NNC)
      CALL JEECRA(MORSE1//'.INO','LONT',NN1,' ')
      CALL JEVEUO(MORSE1//'.INO','E',E1)

      CALL JECREC(MORSE2//'.INO','V V I','NU','CONTIG','VARIABLE',NNC)
      CALL JEECRA(MORSE2//'.INO','LONT',NN2,' ')
      CALL JEVEUO(MORSE2//'.INO','E',E2)

      NNC = 0

      DO 90 N1 = 1, NNO

        IF (ZI(D2-1+N1).EQ.0) GOTO 90
        ZI(E0+NNC) = N1
        NNC = NNC + 1

        CALL JECROC(JEXNUM(MORSE1//'.INO',NNC))
        CALL JEECRA(JEXNUM(MORSE1//'.INO',NNC),'LONMAX',ZI(D1-1+N1),' ')

        CALL JECROC(JEXNUM(MORSE2//'.INO',NNC))
        CALL JEECRA(JEXNUM(MORSE2//'.INO',NNC),'LONMAX',ZI(D2-1+N1),' ')

 90   CONTINUE

C --- ECRITURE DU PROFIL DES MATRICES

      P1 = ZI(B2)

      DO 100 N1 = 1, NNO

        P0 = P1
        P1 = ZI(B2+N1)
        IF (ZI(D2-1+N1).EQ.0) GOTO 100

        DO 110 M2 = 1, NM2
          ZL(D3-1+M2) = .FALSE.
 110    CONTINUE

        DO 120 N2 = 1, NNO
          ZL(D0-1+N2) = .FALSE.
 120    CONTINUE

        NN1 = 0
        NN2 = 0

        DO 130 I = P0, P1-1

          M1 = ZI(B1-1+I)
          IF (.NOT.COL(M1)) GOTO 130
          J = ZI(B0-1+M1)
          P2 = ZI(A1-1+J)
          P3 = ZI(A1+J)

          DO 140 J = P2, P3-1

            N2 = ZI(A0-1+J)
            IF (ZL(D0-1+N2)) GOTO 140
            ZL(D0-1+N2) = .TRUE.
            ZI(E1+NN1) = N2
            NN1 = NN1 + 1

 140      CONTINUE

          P2 = ZI(B4-1+M1)
          P3 = ZI(B4+M1)

          DO 150 J = P2, P3-1

            M2 = ZI(B3-1+J)
            IF (ZL(D3-1+M2)) GOTO 150
            ZL(D3-1+M2) = .TRUE.
            K = ZI(C0-1+M2)
            P4 = ZI(A1-1+K)
            P5 = ZI(A1+K)

            DO 160 K = P4, P5-1

              N2 = ZI(A0-1+K)
              IF (ZL(D0-1+N2)) GOTO 160
              ZL(D0-1+N2) = .TRUE.
              ZI(E2+NN2) = N2
              NN2 = NN2 + 1

 160        CONTINUE

 150      CONTINUE

 130    CONTINUE

        CALL TRI(ZI(E1),ZI,0,NN1)
        CALL TRI(ZI(E2),ZI,0,NN2)
        E1 = E1 + NN1
        E2 = E2 + NN2

 100  CONTINUE

C --- DESALLOCATIONS

      CALL JEDETR('&&ARLFC1.FILTRE.MA')
      CALL JEDETR('&&ARLFC1.COMPTEUR2')
      CALL JEDETR('&&ARLFC1.COMPTEUR1')
      CALL JEDETR('&&ARLFC1.FILTRE.N1')
      CALL JEDETR('&&ARLFC1.FILTRE.N2')

      CALL JEDEMA()

      END
