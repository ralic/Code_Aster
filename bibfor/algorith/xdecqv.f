      SUBROUTINE XDECQV(NNOSE,IT,CNSET,LSN,IGEOM,NINTER,NPTS,
     &                          AINTER,NSE,CNSE,HEAV,NSEMAX)
      IMPLICIT NONE

      INTEGER       NNOSE,IT,CNSET(*),IGEOM,NINTER,NPTS,NSE,CNSE(6,6)
      INTEGER       NSEMAX
      REAL*8        LSN(*),AINTER(*),HEAV(*)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/01/2012   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                      DÉCOUPER LE TETRA EN NSE SOUS-TETRAS
C
C     ENTREE
C       NNOSE    : NOMBRE DE NOEUDS DU SOUS TETRA
C       IT       : INDICE DU TETRA EN COURS
C       CNSET    : CONNECTIVITÉ DES NOEUDS DU TETRA
C       LSN      : VALEURS DE LA LEVEL SET NORMALE
C       IGEOM    : ADRESSE DES COORDONNÉES DES NOEUDS DE L'ELT PARENT
C       NINTER   : NB DE POINTS D'INTERSECTION
C       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN NOEUD
C       AINTER   : INFOS ARETE CORRESPONDATE AU PT INTERSECTION
C
C     SORTIE
C       NSE      : NOMBRE DE SOUS-ÉLÉMENTS (TÉTRAS)
C       CNSE     : CONNECTIVITÉ DES SOUS-ÉLÉMENTS (TÉTRAS)
C       HEAV     : FONCTION HEAVYSIDE CONSTANTE SUR CHAQUE SOUS-ÉLÉMENT
C     ----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------
C
      REAL*8          X(3),XLSN
      INTEGER         IN,INH,I,J,AR(12,3),NBAR,ISE,NDIM,IBID
      INTEGER         A1,A2,A,B,C,IADZI,IAZK24,NDIME,JDIM
      INTEGER         K,E,E2,NNOP
      INTEGER         ZXAIN,XXMMVD
      CHARACTER*8     TYPMA,NOMA,ELRESE(3)
      LOGICAL         CUT

      DATA            ELRESE /'SEG3','TRIA6','TETRA10'/
C --------------------------------------------------------------------
      CALL JEMARQ()

      CALL ELREF4(' ','RIGI',NDIME,NNOP,IBID,IBID,IBID,IBID,IBID,IBID)
      ZXAIN = XXMMVD('ZXAIN')
      CALL TECAEL(IADZI,IAZK24)
      NOMA=ZK24(IAZK24)
      CALL JEVEUO(NOMA//'.DIME','L',JDIM)
      NDIM=ZI(JDIM-1+6)

C     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
C     NDIM EST LA DIMENSION DU MAILLAGE
C     NDIME EST DIMENSION DE L'ELEMENT FINI
C     PAR EXEMPLE, POUR LES ELEMENT DE BORDS D'UN MAILLAGE 3D :
C     NDIME = 2 ALORS QUE NDIM = 3

      DO 10 IN=1,6
        DO 20 J=1,6
          CNSE(IN,J)=0
 20     CONTINUE
 10   CONTINUE

      TYPMA=ELRESE(NDIME)

C     CALCUL DES COORDONNEES ET LSN DU NOEUD 9
      IF (NNOP.EQ.8) THEN
        CALL NDCENT(IGEOM,LSN,X,XLSN)
      ENDIF

      CALL CONARE(TYPMA,AR,NBAR)

      CUT=.FALSE.
      I=1
 1    CONTINUE
C     (1) RECHERCHE D'UN NOEUD PIVOT (LSN NON NULLE)
      IF(CNSET(NNOSE*(IT-1)+I).NE.9.AND.LSN(CNSET(NNOSE*(IT-1)+I)).NE.0
     &                                 .AND.I.LT.NNOSE)THEN
        DO 30 K=I+1,NNOSE
C       (2) PRODUIT DE CE PIVOT PAR LES AUTRES LSN
          IF (CNSET(NNOSE*(IT-1)+K).EQ.9) THEN
            IF ((LSN(CNSET(NNOSE*(IT-1)+I))*XLSN).LT.0.D0) CUT=.TRUE.
          ELSEIF (CNSET(NNOSE*(IT-1)+K).NE.9) THEN
            IF ((LSN(CNSET(NNOSE*(IT-1)+I))*
     &            LSN(CNSET(NNOSE*(IT-1)+K))).LT.0.D0) THEN
              CUT=.TRUE.
            ENDIF
          ENDIF
 30     CONTINUE
        IF (.NOT.CUT) THEN
          NSE=1
          DO 31 IN=1,NNOSE
            CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 31       CONTINUE
        ENDIF
      ELSE
        I=I+1
        GO TO 1
      END IF

C --------------------------------------------------------------------
C     REMPLISSAGE DE LA CONNECTIVITÉ DES SOUS-ELEMENTS TÉTRAS
C                  ALGO BOOK III (26/04/04)
C --------------------------------------------------------------------
      IF (NDIME .EQ. 2 .AND. CUT) THEN

        IF (NINTER .LT.2) THEN
C       PAS DE DECOUPAGE
C         1 SEUL ELEMENT
          NSE=1
          DO 40 IN=1,NNOSE
            CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 40       CONTINUE
C
        ELSEIF (NINTER .EQ.2) THEN
          A1=NINT(AINTER(ZXAIN*(1-1)+1))
          A2=NINT(AINTER(ZXAIN*(2-1)+1))
          IF (NPTS .EQ.0) THEN
C         DECOUPAGE EN 3 ELEMENTS
C         3 SEUL ELEMENT
            NSE=3
            CALL ASSERT(A1.NE.0)
C           101 ET 102 LES 2 POINTS D'INTERSECTION
C           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
C
            DO 50 I=1,2
              DO 51 J=1,2
                IF (AR(A1,I).EQ.AR(A2,J)) THEN
                  A=AR(A1,I)
                  B=AR(A1,3-I)
                  C=AR(A2,3-J)
                  E=AR(6-(A1+A2),3)
                ENDIF
 51           CONTINUE
 50         CONTINUE
            CNSE(1,1)=101
            CNSE(1,2)=102
            CNSE(1,3)=CNSET(NNOSE*(IT-1)+A)
            CNSE(1,4)=205
            CNSE(1,5)=204
            CNSE(1,6)=202
            CNSE(2,1)=101
            CNSE(2,2)=102
            CNSE(2,3)=CNSET(NNOSE*(IT-1)+C)
            CNSE(2,4)=205
            CNSE(2,5)=203
            CNSE(2,6)=206
            CNSE(3,1)=101
            CNSE(3,2)=CNSET(NNOSE*(IT-1)+B)
            CNSE(3,3)=CNSET(NNOSE*(IT-1)+C)
            CNSE(3,4)=201
            CNSE(3,5)=CNSET(NNOSE*(IT-1)+E)
            CNSE(3,6)=206
C
          ELSEIF (NPTS .EQ.1) THEN
C         DECOUPAGE EN 2 ELEMENTS
C         2 SEUL ELEMENT
            NSE=2
            CALL ASSERT(A1.EQ.0.AND.A2.NE.0)
C           101 ET 102 LES 2 POINTS D'INTERSECTION
C           CNSE(1,1)=101
            B = AR(A2,1)
            C = AR(A2,2)
            IF (A2.EQ.1) THEN
              E=6
              E2=5
            ELSEIF (A2.EQ.2) THEN
              E=4
              E2=6
            ELSEIF (A2.EQ.3) THEN
              E=5
              E2=4
            ENDIF
            CNSE(1,1)=NINT(AINTER(ZXAIN*(NPTS-1)+2))
            CNSE(1,2)=102
            CNSE(1,3)=CNSET(NNOSE*(IT-1)+B)
            CNSE(1,4)=203
            CNSE(1,5)=202
            CNSE(1,6)=CNSET(NNOSE*(IT-1)+E)
            CNSE(2,1)=NINT(AINTER(ZXAIN*(NPTS-1)+2))
            CNSE(2,2)=102
            CNSE(2,3)=CNSET(NNOSE*(IT-1)+C)
            CNSE(2,4)=203
            CNSE(2,5)=201
            CNSE(2,6)=CNSET(NNOSE*(IT-1)+E2)

          ELSEIF (NPTS .GE.2) THEN
C         PAS DE DECOUPAGE
C         1 SEUL ELEMENT
            NSE=1
            DO 60 IN=1,NNOSE
              CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 60         CONTINUE
          ENDIF

        ELSEIF (NINTER .EQ.3) THEN

          IF (NPTS .EQ.0) THEN
C           PAS DE DECOUPAGE
C           1 SEUL ELEMENT
            NSE=1
            DO 70 IN=1,NNOSE
             CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 70         CONTINUE
C
          ELSEIF (NPTS .EQ.1) THEN
C           DECOUPAGE EN 3 ELEMENTS
C           3 ELEMENTS
            NSE=3
            A1=NINT(AINTER(ZXAIN*(2-1)+1))
            A2=NINT(AINTER(ZXAIN*(3-1)+1))
C           ON PLACE A,B,C SUR LE TRIA
            DO 80 I=1,2
              DO 81 J=1,2
                IF (AR(A1,I).EQ.AR(A2,J)) THEN
                  A=AR(A1,I)
                  B=AR(A1,3-I)
                  C=AR(A2,3-J)
                  E=AR(6-(A1+A2),3)
                ENDIF
 81           CONTINUE
 80         CONTINUE
            CNSE(1,1)=102
            CNSE(1,2)=103
            CNSE(1,3)=CNSET(NNOSE*(IT-1)+A)
            CNSE(1,4)=207
            CNSE(1,5)=205
            CNSE(1,6)=203
            CNSE(2,1)=102
            CNSE(2,2)=103
            CNSE(2,3)=CNSET(NNOSE*(IT-1)+C)
            CNSE(2,4)=207
            CNSE(2,5)=204
            CNSE(2,6)=206
            CNSE(3,1)=102
            CNSE(3,2)=CNSET(NNOSE*(IT-1)+B)
            CNSE(3,3)=CNSET(NNOSE*(IT-1)+C)
            CNSE(3,4)=202
            CNSE(3,5)=CNSET(NNOSE*(IT-1)+E)
            CNSE(3,6)=206
C
          ELSEIF (NPTS .GE.2) THEN
C         PAS DE DECOUPAGE
C         1 SEUL ELEMENT
            NSE=1
            DO 90 IN=1,NNOSE
              CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 90         CONTINUE

          ENDIF
C           ENDIF SUR NPTS DE NINTER=3

        ELSE

C         1 SEUL ELEMENT
          NSE=1
          DO 100 IN=1,NNOSE
            CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 100      CONTINUE
C
        ENDIF

      ELSEIF (NDIME .EQ. 1 .AND. CUT) THEN

        IF (NINTER .LT.1) THEN
C         PAS DE DECOUPAGE
C         1 SEUL ELEMENT
          NSE=1
          DO 110 IN=1,NNOSE
            CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 110      CONTINUE

         ELSEIF (NINTER .EQ.1) THEN
           A1=NINT(AINTER(ZXAIN*(1-1)+1))
           IF (NPTS .EQ.0) THEN
C          DECOUPAGE EN 2 ELEMENTS
             NSE=2
             CALL ASSERT(A1.NE.0)
             A=AR(A1,1)
             B=AR(A1,2)
C            101 LE POINT D'INTERSECTION
C            ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
             CNSE(1,1)=101
             CNSE(1,2)=CNSET(NNOSE*(IT-1)+A)
             CNSE(1,3)=202
             CNSE(2,1)=101
             CNSE(2,2)=CNSET(NNOSE*(IT-1)+B)
             CNSE(2,3)=201
           ENDIF

         ELSEIF (NINTER .GE.2) THEN
C        PAS DE DECOUPAGE
C        1 SEUL ELEMENT
           NSE=1
           DO 120 IN=1,NNOSE
             CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 120       CONTINUE
C
         ENDIF
       ENDIF
C
C --------------------------------------------------------------------
C             MATRICE DES COORDONNÉES ET FONCTION HEAVYSIDE
C             ALGO BOOK III (28/04/04)
C --------------------------------------------------------------------

      CALL ASSERT(NSE.LE.NSEMAX)
      DO 300 ISE=1,NSE
        HEAV(ISE)=1.D0
        DO 310 IN=1,NDIME+1
          INH=CNSE(ISE,IN)
          IF (INH.LT.100) THEN
            IF (LSN(INH).LT.0.D0) HEAV(ISE)=-1.D0
          ENDIF
 310    CONTINUE
 300  CONTINUE

C     REMARQUE IMPORTANTE :
C     SI ON EST SUR UN ELEMENT DE BORD COINCIDANT AVEC L'INTERCE
C     (NDIME = NDIM - 1 ET NPTS = NINTER = NDIM) ALORS ON NE PEUT PAS
C     DÉTERMINER DE QUEL COTÉ DE L'INTERFACE ON SE TROUVE, CAR ON
C     EST TOUJOURS SUR L'INTERFACE. LA VALEUR DE HEAV(ISE)
C     EST DONC FAUSSE DANS CE CAS : ON MET 99.
C     UNE CORRECTION EST FAITE DANS XORIPE LORS DE L'ORIENTATION
C     DES NORMALES, OU ON EN PROFITE POUR CORRIGER AUSSI HEAV(ISE)
      IF (NDIME.EQ.NDIM-1.AND.NPTS.EQ.NINTER.AND.NINTER.EQ.NDIM) THEN
        CALL ASSERT(NSE.EQ.1)
         HEAV(1)=99.D0
      ENDIF

      CALL JEDEMA()
      END
