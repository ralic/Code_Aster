      SUBROUTINE ARLAPP(MAIL,NOM1Z,NOM2Z,NNORMZ,NMAX,NTM,COL,NMC,NAPP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C======================================================================
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
C         APPARIEMENT DES MAILLES EN VIS-A-VIS POUR ARLEQUIN
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8       MAIL       : NOM DU MAILLAGE
C CHARACTER*(10)    NOM1Z      : NOM SD DOMAINE 1
C CHARACTER*(10)    NOM2Z      : NOM SD DOMAINE 2
C CHARACTER*(10)    NNORMZ     : NORMALES LISSEES COQUE (CF LISNOR)
C INTEGER           NMAX       : DEGRE MAXIMAL GRAPHES NOEUD->MAILLE
C CHARACTER*8       NTM(*)     : VECTEUR NOMS TYPES DE MAILLE
C
C VARIABLES D'ENTREE / SORTIE
C LOGICAL           COL(*)     : FILTRE DECRIVANT ZONE DE COLLAGE
C                                SOUS-ENSEMBLE DU DOMAINE 1
C INTEGER           NMC        : NOMBRE DE MAILLES DE ZONE COLLAGE
C INTEGER           NAPP       : NOMBRE DE COUPLES D'APPARIEMENT
C
C SD D'ENTREE
C NOM1.GROUPEMA : LISTE DES MAILLES DOMAINE 1
C NOM1.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C NOM2.GROUPEMA : LISTE DES MAILLES DOMAINE 2
C NOM2.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C NOM2.ARBRE    : SD ARBRE DE LOCALISATION (CF BISSEC)
C NOM2.GRMAMA   : SD GRAPHE MAILLE -> MAILLES VOISINES (CF GRMAMA)
C
C SD DE SORTIE
C NOM1.NOM2  : GRAPHE D'APPARIEMENT (XC V I NUMERO VARIABLE)
C              MAILLE DOMAINE 1 -> MAILLES DOMAINE 2 EN VIS-A-VIS
C              [MA1] : (MA2.1, MA2.2, MA2.3, ...)
C                      AVEC MA* INDEX NOM*.GROUPEMA
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
      CHARACTER*(*) NOM1Z,NOM2Z,NNORMZ
      CHARACTER*24  NOMAPP
      CHARACTER*10  NOM1,NOM2,NNORMA
      CHARACTER*8   MAIL,NTM(*),TYPEMA
      INTEGER       A0,A1,A2,A3,A4,B0,B1,B2,B3
      INTEGER       C0,C1,C2,C3,C4,C5,C6,C7,C8
      INTEGER       Z0,Z1,E0(2),E1,E2,E3,E4
      INTEGER       DIME,NMAX,NAPP,NAPT,NVIS,NVIS0,NM1,NM2,NMC,NMA,N,NH
      INTEGER       NVOIS(2),IMA,M1,M2,NNO,NNOH,DD1,DD2,I,J,K,L,P0,P1
      INTEGER       ARE(48),NARE,PAN(60),NPAN
      REAL*8        NO(81),R
      LOGICAL       COL(*),IR

C --- FONCTION
      REAL*8        DDOT
      INTEGER       NSOMMT

      NOM1 = NOM1Z
      NOM2 = NOM2Z
      NNORMA = NNORMZ

C --- LECTURE DONNEES

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.COORDO    .VALE','L', A0)
      CALL JEVEUO(MAIL//'.CONNEX','L',A1)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',A2)
      CALL JEVEUO(MAIL//'.TYPMAIL','L',A3)
      CALL JEEXIN(NNORMA,I)
      IF (I.NE.0) CALL JEVEUO(NNORMA,'L',A4)

      CALL JEVEUO(NOM1//'.GROUPEMA','L',B0)
      CALL JEVEUO(NOM1//'.BOITE.DIME','L',B1)
      CALL JEVEUO(NOM1//'.BOITE.MINMAX','L',B2)
      CALL JEVEUO(NOM1//'.BOITE.PAN','L',B3)
      DIME = ZI(B1)
      NM1 = ZI(B1+1)

      CALL JEVEUO(NOM2//'.GROUPEMA','L',C0)
      CALL JEVEUO(NOM2//'.BOITE.DIME','L',C1)
      CALL JEVEUO(NOM2//'.BOITE.MINMAX','L',C2)
      CALL JEVEUO(NOM2//'.BOITE.PAN','L',C3)
      CALL JEVEUO(NOM2//'.ARBRE.CELL','L',C4)
      CALL JEVEUO(NOM2//'.ARBRE.LIMA','L',C5)
      NM2 = ZI(C1+1)

      IF (NM2.GT.1) THEN
        CALL JEVEUO(NOM2//'.GRMAMA','L',C6)
        CALL JEVEUO(JEXATR(NOM2//'.GRMAMA','LONCUM'),'L',C7)
      ENDIF

      CALL JEVEUO('&&ARL.ZR','E',Z0)
      CALL JEVEUO('&&ARL.NH','L',Z1)
      NH = ZI(Z1)
      DD1 = 2*DIME
      DD2 = DIME+2

C --- ALLOCATION OBJETS TEMPORAIRES

      NVIS0 = 2*NMAX*MAX(NMC,NM2)
      CALL WKVECT('&&ARLAPP.VOISIN1','V V I',NM2,E0(1))
      CALL WKVECT('&&ARLAPP.VOISIN2','V V I',NM2,E0(2))
      CALL WKVECT('&&ARLAPP.FILTRE','V V L',NM2,E1)
      CALL WKVECT('&&ARLAPP.LISTE.ENTETE','V V I',NM1,E2)
      CALL WKVECT('&&ARLAPP.LISTE.RESERVE','V V I',2*NVIS0,E3)
      CALL WKVECT('&&ARLAPP.NVISAVIS','V V I',NM1,E4)

C --- APPARIEMENT

      NAPP = 0
      NAPT = 0

      DO 10 M1 = 1, NM1

        IF (.NOT.COL(M1)) GOTO 150

C ----- ECHANTILLONNAGE DE LA FRONTIERE

        IMA = ZI(B0-1+M1)

        TYPEMA = NTM(ZI(A3-1+IMA))
        CALL TMACOQ(TYPEMA,DIME,I)
        CALL CONOEU(IMA,ZI(A1),ZI(A2),ZR(A0),ZR(A4),DIME,I,NO,NNO)

        NNO = NSOMMT(TYPEMA)
        CALL NOARET(TYPEMA,ARE,NARE)
        IF (DIME.EQ.3) CALL NOPAN(TYPEMA,PAN,NPAN)

        CALL ECHMAP(DIME,NO,NNO,ARE,NARE,PAN,NPAN,NH,ZR(Z0),NNOH)

C ----- APPARIEMENT PONCTUEL

        NVIS = 0
        DO 20 I = 1, NM2
          ZL(E1-1+I) = .TRUE.
 20     CONTINUE

        Z1 = Z0 - DIME

        DO 30 I = 1, NNOH

          Z1 = Z1 + DIME
          CALL CERNE(ZR(Z1),DIME,ZI(C4),ZR(C3),NMA,J)
          C8 = C5 - 1 + J

C ------- EXPLORATION DES MAILLES CANDIDATES

          DO 40 J = 1, NMA

            M2 = ZI(C8)
            C8 = C8 + 1
            IF (.NOT.ZL(E1-1+M2)) GOTO 40

C --------- MINMAX

            P0 = C2 + DD1*(M2-1)

            DO 50 K = 1, DIME
              R = ZR(Z1-1+K)
              IF ((R.LT.ZR(P0)).OR.(R.GT.ZR(P0+1))) GOTO 40
              P0 = P0 + 2
 50         CONTINUE

C --------- PAN

            P0 = ZI(C1+2*M2)
            N = ZI(C1+2*(M2+1)) - P0
            P0 = C3 + DD2*(P0-1)

            DO 60 K = 1, N
              R = DDOT(DIME,ZR(P0),1,ZR(Z1),1) + ZR(P0+DIME)
              IF (R.GT.0.D0) GOTO 40
              P0 = P0 + DD2
 60         CONTINUE

C --------- STOCKAGE

            ZL(E1-1+M2) = .FALSE.
            ZI(E3) = M2
            IF (NVIS.NE.0) THEN
              ZI(E3+1) = ZI(E2-1+M1)
            ELSE
              ZI(E3+1) = 0
            ENDIF
            ZI(E2-1+M1) = E3
            NVIS = NVIS + 1
            E3 = E3 + 2

            NVIS0 = NVIS0 - 1
            IF (NVIS0.LT.0) CALL U2MESS('F','MODELISA2_5')

            GOTO 30

 40       CONTINUE

 30     CONTINUE

C ----- CAS DE L'INCLUSION

        IF (NVIS.EQ.0) THEN

C ------- POINT MILIEU DE LA PREMIERE MAILLE DOMAINE 2

          IMA = ZI(C0)
          TYPEMA = NTM(ZI(A3-1+IMA))
          CALL TMACOQ(TYPEMA,DIME,I)
          CALL CONOEU(IMA,ZI(A1),ZI(A2),ZR(A0),ZR(A4),DIME,I,NO,NNO)

          P0 = DIME
          DO 70 K = 1, NNO
            DO 70 L = 1, DIME
              P0 = P0 + 1
              NO(L) = NO(L) + NO(P0)
 70       CONTINUE

          DO 80 K = 1, DIME
            NO(K) = NO(K) / NNO
 80       CONTINUE

C ------- MINMAX

          P0 = B2 + DD1*(M1-1)

          DO 90 K = 1, DIME
            R = NO(K)
            IF ((R.LT.ZR(P0)).OR.(R.GT.ZR(P0+1))) GOTO 140
            P0 = P0 + 2
 90       CONTINUE

C ------- PAN

          P0 = ZI(B1+2*M1)
          N = ZI(B1+2*(M1+1)) - P0
          P0 = B3 + DD2*(P0-1)

          DO 100 K = 1, N
            R = DDOT(DIME,ZR(P0),1,NO,1) + ZR(P0+DIME)
            IF (R.GT.0.D0) GOTO 140
            P0 = P0 + DD2
 100      CONTINUE

C ------- STOCKAGE

          ZL(E1) = .FALSE.
          ZI(E3) = 1
          ZI(E3+1) = 0
          ZI(E2-1+M1) = E3
          E3 = E3 + 2
          NVIS = 1

        ENDIF

C ----- INITIALISATION PARCOURS DU VOISINAGE

        IF ((NM2.EQ.1).OR.(NVIS.EQ.0)) GOTO 140

        K = 1
        L = 2
        P0 = E0(1)
        P1 = ZI(E2-1+M1)
        NVOIS(1) = NVIS

        DO 110 I = 1, NVIS
          ZI(P0-1+I) = ZI(P1)
          P1 = ZI(P1+1)
 110     CONTINUE

C ----- PARCOURS DU VOISINAGE

 120    CONTINUE

        NVOIS(L) = 0
        NMA = NVOIS(K)
        P0 = E0(K)

        DO 130 I = 1, NMA

          J = ZI(P0)
          P0 = P0 + 1

          P1 = ZI(C7-1+J)
          N = ZI(C7+J) - P1
          P1 = C6 - 1 + P1

          DO 130 J = 1, N

            M2 = ZI(P1)
            P1 = P1 + 1

            IF (ZL(E1-1+M2)) THEN

              ZL(E1-1+M2) = .FALSE.

              CALL MINTER(DIME,M1,M2,ZI(B1),ZI(C1),
     &                    ZR(B2),ZR(C2),ZR(B3),ZR(C3),IR)

              IF (IR) THEN

                ZI(E0(L)+NVOIS(L)) = M2
                NVOIS(L) = NVOIS(L) + 1
                ZI(E3) = M2
                ZI(E3+1) = ZI(E2-1+M1)
                ZI(E2-1+M1) = E3
                E3 = E3 + 2
                NVIS = NVIS + 1

                NVIS0 = NVIS0 - 1
                IF (NVIS0.LT.0) CALL U2MESS('F','MODELISA2_5')

              ENDIF

            ENDIF

 130    CONTINUE

        K = 3 - K
        L = 3 - L

        IF (NVOIS(K).NE.0) GOTO 120

 140    CONTINUE

        IF (NVIS.NE.0) THEN
          ZI(E4-1+M1) = NVIS
          NAPP = NAPP + NVIS
          NAPT = NAPT + NVIS
          GOTO 10
        ENDIF

        COL(M1) = .FALSE.
        NMC = NMC - 1

 150    CONTINUE

        ZI(E4-1+M1) = 1
        NAPT = NAPT + 1

 10   CONTINUE

      IF (NMC.EQ.0) CALL U2MESS('F','MODELISA2_6')

C --- ALLOCATION

      NOMAPP = NOM1//'.'//NOM2
      CALL JECREC(NOMAPP,'V V I','NU','CONTIG','VARIABLE',NM1)
      CALL JEECRA(NOMAPP,'LONT',NAPT,' ')

C --- COPIE LISTE -> SD. APPARIEMENT

      DO 160 M1 = 1, NM1

        CALL JECROC(JEXNUM(NOMAPP,M1))
        CALL JEECRA(JEXNUM(NOMAPP,M1),'LONMAX',ZI(E4-1+M1),' ')
        IF (.NOT.COL(M1)) GOTO 160
        CALL JEVEUO(JEXNUM(NOMAPP,M1),'E',P0)

        P1 = ZI(E2-1+M1)
 170    CONTINUE
        ZI(P0) = ZI(P1)
        P0 = P0 + 1
        P1 = ZI(P1+1)
        IF (P1.NE.0) GOTO 170

 160  CONTINUE

C --- DESALLOCATION

      CALL JEDETR('&&ARLAPP.VOISIN1')
      CALL JEDETR('&&ARLAPP.VOISIN2')
      CALL JEDETR('&&ARLAPP.FILTRE')
      CALL JEDETR('&&ARLAPP.LISTE.ENTETE')
      CALL JEDETR('&&ARLAPP.LISTE.RESERVE')
      CALL JEDETR('&&ARLAPP.NVISAVIS')

      CALL JEDEMA()

      END
