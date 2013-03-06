      SUBROUTINE XDECQV(NNOSE,IT,CNSET,LSN,IGEOM,NINTER,NPTS,
     &                          AINTER,NSE,CNSE,HEAV,NSEMAX)
      IMPLICIT NONE

      INCLUDE 'jeveux.h'

      INTEGER       NNOSE,IT,CNSET(*),IGEOM,NINTER,NPTS,NSE,CNSE(6,6)
      INTEGER       NSEMAX
      REAL*8        LSN(*),AINTER(*),HEAV(*)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/03/2013   AUTEUR MARTIN A.MARTIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                  SOMMET
C       AINTER   : INFOS ARETE CORRESPONDATE AU PT INTERSECTION
C
C     SORTIE
C       NSE      : NOMBRE DE SOUS-ÉLÉMENTS (TÉTRAS)
C       CNSE     : CONNECTIVITÉ DES SOUS-ÉLÉMENTS (TÉTRAS)
C       HEAV     : FONCTION HEAVYSIDE CONSTANTE SUR CHAQUE SOUS-ÉLÉMENT
C     ----------------------------------------------------------------
C
      REAL*8          X(3),XLSN,LSNK
      REAL*8          R8PREM
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
C     (1) RECHERCHE D'UN NOEUD PIVOT (LSN NON NULLE)
  1   CONTINUE
      IF (I.LT.NNOSE) THEN
        IF (CNSET(NNOSE*(IT-1)+I).EQ.9
     &      .OR.LSN(CNSET(NNOSE*(IT-1)+I)).EQ.0.D0) THEN
          I=I+1
          GOTO 1
        END IF
      END IF
C     (2) PRODUIT DE CE PIVOT PAR LES AUTRES LSN      
      K=I+1
 30   CONTINUE
      IF (K.LE.NNOSE) THEN      
C       RECUPERATION DE LSN(K) :
C         - CAS PARTICULIER DU NOEUD CENTRAL D'UN Q9
        IF (CNSET(NNOSE*(IT-1)+K).EQ.9) THEN
          LSNK=XLSN
C         - CAS GENERAL
        ELSE
          LSNK=LSN(CNSET(NNOSE*(IT-1)+K))
        ENDIF

        IF (LSN(CNSET(NNOSE*(IT-1)+I))*LSNK.LT.0.D0) THEN
          CUT=.TRUE.
        ELSE
          K=K+1
          GOTO 30
        END IF
      END IF

C     STOCKAGE DE LA CONNECTIVITE D'UN SOUS-ELEMENT NON COUPE        
      IF (.NOT.CUT) THEN
        NSE=1
        DO 31 IN=1,NNOSE
          CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 31     CONTINUE
      ENDIF

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
C     (NDIME = NDIM - 1 ET NPTS = NDIM) ALORS ON NE PEUT PAS
C     DÉTERMINER DE QUEL COTE DE L'INTERFACE ON SE TROUVE, CAR ON
C     EST TOUJOURS SUR L'INTERFACE. LA VALEUR DE HEAV(ISE)
C     EST DONC FAUSSE DANS CE CAS : ON MET 99.
C     UNE CORRECTION EST FAITE DANS XORIPE LORS DE L'ORIENTATION
C     DES NORMALES, OU ON EN PROFITE POUR CORRIGER AUSSI HEAV(ISE)
C     CONTRAIREMENT AU CAS LINEAIRE, ON N'A PAS NPTS = NINTER. EN 2D,
C     ON CONSIDERE DES SEG3 (NPTS = 2 ET NINTER = 3) ET EN 3D, ON 
C     CONSIDERE DES TRIA6 (NPTS = 3 ET NINTER = 6).
      IF (NDIME.EQ.NDIM-1.AND.NPTS.EQ.NDIM) THEN
        HEAV(1)=99.D0
      ENDIF

      CALL JEDEMA()
      END
