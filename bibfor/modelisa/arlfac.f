      SUBROUTINE ARLFAC(MAIL,DIME,NOMCZ,CINEC,NOM1Z,CINE1,NMORSZ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C ----------------------------------------------------------------------
C   ALLOCATION ET ECRITURE DES POINTEURS DE LA MATRICE ARLEQUIN MORSE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C CHARACTER*8    MAIL      : SD MAILLAGE
C INTEGER        DIME      : DIMENSION DE L'ESPACE
C CHARACTER*(10) NOMCZ     : SD DOMAINE DE COLLAGE
C CHARACTER*8    CINEC     : CINEMATIQUE DU DOMAINE DE COLLAGE
C                            ('SOLIDE  ' OU 'COQUE   ') (CF ARLVER)
C CHARACTER*(10) NOM1Z     : SD DOMAINE MECANIQUE
C CHARACTER*8    CINE1     : CINEMATIQUE DU DOMAINE MECANIQUE
C                            ('SOLIDE  ' OU 'COQUE   ') (CF ARLVER)
C CHARACTER*(16) NMORSZ    : SD MATRICE ARLEQUIN MORSE
C
C SD D'ENTREE
C NOMC.GROUPEMA : LISTE DE MAILLES DOMAINE DE COLLAGE
C NOMC.CNCINV   : SD GRAPHE CONNECTIVITE INVERSE (CF CNCINV)
C NOMC.NOM1     : SD GRAPHE D'APPARIEMENT (CF ARLAPP)
C NOM1.GROUPEMA : LISTE DE MAILLES DOMAINE MECANIQUE
C
C SD DE SORTIE
C NOMC.INO      : LISTE TRIEE DES NOEUDS LIGNE DE LA MATRICE MORSE
C                 (NO1, NO2, NO3, ... ) AVEC NO1 < NO2 < NO3 < ... 
C MORSE.DIME    : DIMENSION DE LA MATRICE NODALE
C MORSE.INO     : LISTES TRIEES DES NOEUDS COLONNES DE LA MATRICE MORSE
C                 (XC V I NUMERO VARIABLE)
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
      CHARACTER*(*) NOMCZ,NOM1Z,NMORSZ
      CHARACTER*16  NMORSE
      CHARACTER*10  NOMC,NOM1
      CHARACTER*8   MAIL,CINEC,CINE1
      INTEGER       DIME,NMA1,NMA2,NNO,D,I,J,K,N,N1,N2,NN1,NN2,M1,M2,IM2
      INTEGER       A0,A1,B0,B1,C0,C1,C2,D0,D1,D2,P0,P1,P2,P3,P4,P5

      NOMC = NOMCZ
      NOM1 = NOM1Z
      NMORSE = NMORSZ

C --- DIMENSION DE LA MATRICE NOEUD ARLEQUIN

      IF (CINE1(1:6).EQ.'SOLIDE') THEN
        IF (CINEC(1:6).EQ.'SOLIDE') THEN
          D = 1
        ELSEIF (CINEC(1:5).EQ.'COQUE') THEN
          D = DIME*(DIME-1) + 1
        ENDIF
      ELSEIF (CINE1(1:5).EQ.'COQUE') THEN
        IF (CINEC(1:6).EQ.'SOLIDE') THEN
          D = DIME + 1
        ELSEIF (CINEC(1:5).EQ.'COQUE') THEN
          D = (3*DIME-2)*(DIME-1) + 2
        ENDIF      
      ENDIF

C --- LECTURE

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.CONNEX','L',A0)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',A1)

      CALL JELIRA(NOMC//'.GROUPEMA','LONMAX',NMA1,ZK8)
      CALL JELIRA(NOMC//'.CNCINV','NMAXOC',NNO,ZK8)
      CALL JEVEUO(NOMC//'.CNCINV','L',B0)
      CALL JEVEUO(JEXATR(NOMC//'.CNCINV','LONCUM'),'L',B1)

      CALL JEVEUO(NOMC//'.'//NOM1,'L',C0)
      CALL JEVEUO(JEXATR(NOMC//'.'//NOM1,'LONCUM'),'L',C1)
      CALL JEVEUO(NOM1//'.GROUPEMA','L',C2)
      CALL JELIRA(NOM1//'.GROUPEMA','LONMAX',NMA2,ZK8)

C --- OBJETS TEMPORAIRES

      CALL WKVECT('&&ARLFAC.FILTRE.MA','V V L',NMA2,D0)
      CALL WKVECT('&&ARLFAC.FILTRE.NO','V V L',NNO,D1)

      P1 = ZI(B1)
      NN1 = 0
      NN2 = 0

C --- COMPTE NOEUDS 2 EN VIS-A-VIS AVEC LES NOEUDS 1

      DO 10 N1 = 1, NNO

        P0 = P1
        P1 = ZI(B1+N1)

        IF (ZI(B0-1+P0).EQ.0) GOTO 10
  
        DO 20 M2 = 1, NMA2
          ZL(D0-1+M2) = .FALSE.
 20     CONTINUE

        DO 30 N2 = 1, NNO
          ZL(D1-1+N2) = .FALSE.
 30     CONTINUE

        N = 0

        DO 40 I = P0, P1-1

          M1 = ZI(B0-1+I)
          P2 = ZI(C1-1+M1)
          P3 = ZI(C1+M1)

          DO 40 J = P2, P3-1

            M2 = ZI(C0-1+J)
            IF (ZL(D0-1+M2)) GOTO 40
            ZL(D0-1+M2) = .TRUE.
            IM2 = ZI(C2-1+M2)
            P4 = ZI(A1-1+IM2)
            P5 = ZI(A1+IM2)

            DO 50 K = P4, P5-1

              N2 = ZI(A0-1+K)
              IF (ZL(D1-1+N2)) GOTO 50
              ZL(D1-1+N2) = .TRUE.
              N = N + 1
 
 50       CONTINUE

 40     CONTINUE

        NN1 = NN1 + 1
        NN2 = NN2 + N

 10   CONTINUE

C --- ALLOCATION MATRICE DE COUPLAGE MORSE

      CALL JECREO(NMORSE//'.DIME','V E I')
      CALL JEVEUO(NMORSE//'.DIME','E',D2)
      ZI(D2) = D

      CALL JECREC(NMORSE//'.INO','V V I','NU','CONTIG','VARIABLE',NN1)
      CALL JEECRA(NMORSE//'.INO','LONT',NN2,' ')
 
C --- ALLOCATION ET ECRITURE DE NOMC.INO

      CALL JEEXIN(NOMC//'.INO',I)

      IF (I.EQ.0) THEN

        CALL WKVECT(NOMC//'.INO','V V I',NN1,D2)

        DO 60 N1 = 1, NNO

          IF (ZI(B0-1+ZI(B1-1+N1)).EQ.0) GOTO 60
          ZI(D2) = N1
          D2 = D2 + 1

 60     CONTINUE

      ENDIF

C --- ECRITURE DE MORSE.INO

      CALL JEVEUO(NMORSE//'.INO','E',D2)
      P1 = ZI(B1)
      NN1 = 0

      DO 70 N1 = 1, NNO

        P0 = P1
        P1 = ZI(B1+N1)

        IF (ZI(B0-1+P0).EQ.0) GOTO 70
  
        DO 80 I = 1,NMA2
          ZL(D0-1+I) = .FALSE.
 80     CONTINUE

        DO 90 I = 1, NNO
          ZL(D1-1+I) = .FALSE.
 90     CONTINUE

        N = 0

        DO 100 I = P0, P1-1

          M1 = ZI(B0-1+I)
          P2 = ZI(C1-1+M1)
          P3 = ZI(C1+M1)

          DO 100 J = P2, P3-1

            M2 = ZI(C0-1+J)
            IF (ZL(D0-1+M2)) GOTO 100
            ZL(D0-1+M2) = .TRUE.
            IM2 = ZI(C2-1+M2)            
            P4 = ZI(A1-1+IM2)
            P5 = ZI(A1+IM2)

            DO 110 K = P4, P5-1

              N2 = ZI(A0-1+K)
              IF (ZL(D1-1+N2)) GOTO 110
              ZL(D1-1+N2) = .TRUE.
              ZI(D2+N) = N2
              N = N + 1

 110      CONTINUE

 100    CONTINUE

        NN1 = NN1 + 1
        CALL JECROC(JEXNUM(NMORSE//'.INO',NN1))
        CALL JEECRA(JEXNUM(NMORSE//'.INO',NN1),'LONMAX',N,' ')
        CALL TRI(ZI(D2),ZI,0,N)
        D2 = D2 + N

 70   CONTINUE

C --- DESALLOCATIONS

      CALL JEDETR('&&ARLFAC.FILTRE.MA')
      CALL JEDETR('&&ARLFAC.FILTRE.NO')
      CALL JEDEMA()

      END
