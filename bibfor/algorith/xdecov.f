      SUBROUTINE XDECOV(IT,CONNEC,LSN,IGEOM,PINTER,NINTER,NPTS,
     &                                    AINTER,NSE,CNSE,COORSE,HEAV)
      IMPLICIT NONE

      REAL*8        LSN(*)
      INTEGER       IT,CONNEC(6,4),IGEOM,NINTER,NPTS,NSE,CNSE(6,4)
      CHARACTER*24  PINTER,AINTER,COORSE,HEAV
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/03/2008   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE GENIAUT S.GENIAUT
C                      DÉCOUPER LE TETRA EN NSE SOUS-TETRAS
C
C     ENTREE
C       IT       : INDICE DU TETRA EN COURS
C       CONNEC   : CONNECTIVITÉ DES NOEUDS DU TETRA
C       LSN      : VALEURS DE LA LEVEL SET NORMALE
C       IGEOM    : ADRESSE DES COORDONNÉES DES NOEUDS DE L'ELT PARENT
C       PINTER   : COORDONNÉES DES POINTS D'INTERSECTION
C       NINTER   : NB DE POINTS D'INTERSECTION
C       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN NOEUD
C       AINTER   : INFOS ARETE CORRESPONDATE AU PT INTERSECTION
C
C     SORTIE
C       NSE      : NOMBRE DE SOUS-ÉLÉMENTS (TÉTRAS)
C       CNSE     : CONNECTIVITÉ DES SOUS-ÉLÉMENTS (TÉTRAS)
C       COORSE   : COORDONNÉES DES NOEUDS DES SOUS-ÉLÉMENTS
C       HEAV     : FONCTION HEAVYSIDE CONSTANTE SUR CHAQUE SOUS-ÉLÉMENT
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR,DDOT
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      REAL*8          PADIST,P(3),XYZ(4,3),AB(3),AC(3),AD(3),VN(3),PS
      INTEGER         JPTINT,JAINT,JCOSE,JHEAV
      INTEGER         NSEMAX,IN,INH,I,J,AR(12,2),NBAR,ISE,NDIM,IBID
      INTEGER         A1,A2,A3,A4,A,B,C,IADZI,IAZK24,NDIME,JDIM
      CHARACTER*8     TYPMA,NOMA
C ----------------------------------------------------------------------

      CALL JEMARQ()
      CALL ELREF4(' ','RIGI',NDIME,IBID,IBID,IBID,IBID,IBID,IBID,IBID)

      CALL TECAEL(IADZI,IAZK24)
      NOMA=ZK24(IAZK24)
      CALL JEVEUO(NOMA//'.DIME','L',JDIM)
      NDIM=ZI(JDIM-1+6)

C     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
C     NDIM EST LA DIMENSION DU MAILLAGE
C     NDIME EST DIMENSION DE L'ELEMENT FINI
C     PAR EXEMPLE, POUR LES ELEMENT DE BORDS D'UN MAILLAGE 3D :
C     NDIME = 2 ALORS QUE NDIM = 3

      IF (NDIME.EQ.3) THEN
        NSEMAX=6
      ELSEIF (NDIME.EQ.2) THEN
        NSEMAX=3
      ELSEIF (NDIME.EQ.1) THEN
        NSEMAX=2
      ENDIF

      CALL JEVEUO(PINTER,'L',JPTINT)
      CALL JEVEUO(AINTER,'L',JAINT)
      CALL WKVECT(COORSE,'V V R',NDIM*NSEMAX*(NDIM+1),JCOSE)

      DO 10 IN=1,6
        DO 20 J=1,4
          CNSE(IN,J)=0
 20     CONTINUE
 10   CONTINUE

      IF (NDIME .EQ. 2) THEN
        TYPMA='TRIA3'
      ELSEIF  (NDIME.EQ.3) THEN
        TYPMA='TETRA4'
      ELSEIF  (NDIME.EQ.1) THEN
        TYPMA='SEG2'
      ENDIF
      CALL CONARE(TYPMA,AR,NBAR)

C-----------------------------------------------------------------------
C     REMPLISSAGE DE LA CONNECTIVITÉ DES SOUS-ELEMENTS TÉTRAS
C                  ALGO BOOK III (26/04/04)
C-----------------------------------------------------------------------

      IF (NDIME .EQ. 2) THEN


        IF (NINTER .LT. 2) THEN
C         INTER DOUTEUSE
          CALL ASSERT (NPTS.EQ.NINTER)
C         1 SEUL ELEMENT
          NSE=1
          DO 90 IN=1,3
            CNSE(1,IN)=CONNEC(IT,IN)
 90       CONTINUE
        ELSEIF (NINTER .EQ. 2) THEN
          A1=NINT(ZR(JAINT-1+4*(1-1)+1))
          A2=NINT(ZR(JAINT-1+4*(2-1)+1))
          IF (NPTS .EQ. 2) THEN
C           1 SEUL ELEMENT
            NSE=1
            DO 91 IN=1,3
              CNSE(1,IN)=CONNEC(IT,IN)
 91         CONTINUE
          ELSEIF (NPTS .EQ. 1) THEN
C           2 ELEMENTS
            NSE=2
            CALL ASSERT(A1.EQ.0.AND.A2.NE.0)
C           101 et 102 les 2 points d'intersection
            CNSE(1,1)=101
            CNSE(1,2)=102
            CNSE(1,3)=CONNEC(IT,AR(A2,1))
            CNSE(2,1)=101
            CNSE(2,2)=102
            CNSE(2,3)=CONNEC(IT,AR(A2,2))
          ELSE
C           3 ELEMENTS
            NSE=3
            CALL ASSERT(A1.NE.0)
C           101 et 102 les 2 points d'intersection
C           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
            DO 93 I=1,2
              DO 94 J=1,2
                IF (AR(A1,I).EQ.AR(A2,J)) THEN
                  A=AR(A1,I)
                  B=AR(A1,3-I)
                  C=AR(A2,3-J)
                ENDIF
 94           CONTINUE
 93         CONTINUE
            CNSE(1,1)=101
            CNSE(1,2)=102
            CNSE(1,3)=CONNEC(IT,A)
            CNSE(2,1)=101
            CNSE(2,2)=102
            CNSE(2,3)=CONNEC(IT,C)
            CNSE(3,1)=101
            CNSE(3,2)=CONNEC(IT,B)
            CNSE(3,3)=CONNEC(IT,C)
          ENDIF
        ELSEIF (NINTER .EQ. 3) THEN
C         L'INTERFACE COINCIDE AVEC LE TRIA
          CALL ASSERT (NPTS.EQ.NINTER)
C         1 SEUL ELEMENT
          NSE=1
          DO 92 IN=1,3
            CNSE(1,IN)=CONNEC(IT,IN)
 92       CONTINUE
        ELSE
C         TROP DE POINTS D'INTERSECTION
          CALL ASSERT(NINTER.LE.3)
        ENDIF
        
      ELSEIF (NDIME .EQ. 1) THEN

        IF (NINTER .LT. 1) THEN
C         INTER DOUTEUSE
          CALL ASSERT (NPTS.EQ.NINTER)
C         1 SEUL ELEMENT
          NSE=1
          DO 95 IN=1,2
            CNSE(1,IN)=CONNEC(IT,IN)
 95       CONTINUE
        ELSEIF (NINTER .EQ. 1) THEN
          A1=NINT(ZR(JAINT-1+4*(1-1)+1))
          IF (NPTS .EQ. 1) THEN
C           1 SEUL ELEMENT
            NSE=1
            DO 96 IN=1,2
              CNSE(1,IN)=CONNEC(IT,IN)
 96         CONTINUE
          ELSEIF (NPTS .EQ. 0) THEN
C           2 ELEMENTS
            NSE=2
            CALL ASSERT(A1.NE.0)
                  A=AR(A1,1)
                  B=AR(A1,2)

C           101 et 102 les 2 points d'intersection
C           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
            CNSE(1,1)=101
            CNSE(1,2)=CONNEC(IT,A)
            CNSE(2,1)=101
            CNSE(2,2)=CONNEC(IT,B)
          ENDIF
        ELSEIF (NINTER .EQ. 2) THEN
C         L'INTERFACE COINCIDE AVEC LE SEG
          CALL ASSERT (NPTS.EQ.NINTER)
C         1 SEUL ELEMENT
          NSE=1
          DO 97 IN=1,2
            CNSE(1,IN)=CONNEC(IT,IN)
 97       CONTINUE
        ELSE
C         TROP DE POINTS D'INTERSECTION
          CALL ASSERT(NINTER.LE.2)
        ENDIF   



      ELSEIF (NDIME.EQ.3) THEN


        IF (NINTER.LT.3) THEN

C       1°) AVEC MOINS DE TROIS POINTS D'INTERSECTION
C       ---------------------------------------------

C         INTER DOUTEUSE
          CALL ASSERT (NPTS.EQ.NINTER)
C         ON A UN SEUL ELEMENT
          NSE=1
          DO 100 IN=1,4
              CNSE(1,IN)=CONNEC(IT,IN)
 100      CONTINUE

        ELSEIF (NINTER.EQ.3) THEN

C         2°) AVEC TROIS POINTS D'INTERSECTION
C         ------------------------------------
          A1=NINT(ZR(JAINT-1+4*(1-1)+1))
          A2=NINT(ZR(JAINT-1+4*(2-1)+1))
          A3=NINT(ZR(JAINT-1+4*(3-1)+1))

          IF (NPTS.EQ.3) THEN
C           ON A UN SEUL ELEMENT
            NSE=1
            DO 110 IN=1,4
              CNSE(1,IN)=CONNEC(IT,IN)
 110        CONTINUE

          ELSEIF (NPTS.EQ.2) THEN
C           ON A DEUX SOUS-ELEMENTS
            NSE=2
            CALL ASSERT(A1.EQ.0.AND.A2.EQ.0)
C           CONNECTIVITE DES NSE PAR RAPPORT AU NUM DE NOEUDS DU PARENT
C           AVEC 101, 102 ET 103 LES 3 PTS D'INTERSECTION
            CNSE(1,1)=101
            CNSE(1,2)=102
            CNSE(1,3)=103
            CNSE(1,4)=CONNEC(IT,AR(A3,1))
            CNSE(2,1)=101
            CNSE(2,2)=102
            CNSE(2,3)=103
            CNSE(2,4)=CONNEC(IT,AR(A3,2))

          ELSEIF (NPTS.EQ.1) THEN
C           ON A TROIS SOUS-ELEMENTS
            NSE=3
            CALL ASSERT(A1.EQ.0.AND.A2.NE.0)
C           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
            DO 30 I=1,2
              DO 40 J=1,2
                IF (AR(A2,I).EQ.AR(A3,J)) THEN
                   A=AR(A2,I)
                   B=AR(A2,3-I)
                   C=AR(A3,3-J)
                ENDIF
 40           CONTINUE
 30         CONTINUE
            CNSE(1,1)=101
            CNSE(1,2)=102
            CNSE(1,3)=103
            CNSE(1,4)=CONNEC(IT,A)
            CNSE(2,1)=101
            CNSE(2,2)=102
            CNSE(2,3)=103
            CNSE(2,4)=CONNEC(IT,C)
            CNSE(3,1)=101
            CNSE(3,2)=102
            CNSE(3,3)=CONNEC(IT,B)
            CNSE(3,4)=CONNEC(IT,C)

          ELSEIF (NPTS.EQ.0) THEN
C           ON A QUATRE SOUS-ELEMENTS
            NSE=4
            CNSE(1,1)=101
            CNSE(1,2)=102
            CNSE(1,3)=103

C           ON A 4 CONFIG POSSIBLES :
            IF (A1.EQ.1.AND.A2.EQ.2.AND.A3.EQ.3) THEN
C             CONFIGURATION N°1
              CNSE(1,4)=CONNEC(IT,1)
              CALL XPENTE(2,CNSE,103,101,102,CONNEC(IT,4),CONNEC(IT,2),
     &                                                     CONNEC(IT,3))
            ELSEIF (A1.EQ.1.AND.A2.EQ.4.AND.A3.EQ.5) THEN
C             CONFIGURATION N°2
              CNSE(1,4)=CONNEC(IT,2)
              CALL XPENTE(2,CNSE,CONNEC(IT,1),CONNEC(IT,3),CONNEC(IT,4),
     &                                                      101,102,103)
            ELSEIF (A1.EQ.2.AND.A2.EQ.4.AND.A3.EQ.6) THEN
C             CONFIGURATION N°3
              CNSE(1,4)=CONNEC(IT,3)
              CALL XPENTE(2,CNSE,CONNEC(IT,4),CONNEC(IT,2),CONNEC(IT,1),
     &                                                      103,102,101)
            ELSEIF (A1.EQ.3.AND.A2.EQ.5.AND.A3.EQ.6) THEN
C             CONFIGURATION N°4
              CNSE(1,4)=CONNEC(IT,4)
              CALL XPENTE(2,CNSE,CONNEC(IT,1),CONNEC(IT,2),CONNEC(IT,3),
     &                                                      101,102,103)
            ELSE 
C             PROBLEME DE DECOUPAGE À 3 POINTS
              CALL ASSERT(A1.EQ.1.AND.A2.EQ.2.AND.A3.EQ.3)
            ENDIF

          ENDIF

        ELSEIF (NINTER.EQ.4) THEN

C         2°) AVEC QUATRE POINTS D'INTERSECTION
C          -------------------------------------
          A1=NINT(ZR(JAINT-1+4*(1-1)+1))
          A2=NINT(ZR(JAINT-1+4*(2-1)+1))
          A3=NINT(ZR(JAINT-1+4*(3-1)+1))
          A4=NINT(ZR(JAINT-1+4*(4-1)+1))

C         ON A SIX SOUS-ELEMENTS (DANS TOUS LES CAS ?)
          NSE=6
          IF (A1.EQ.1.AND.A2.EQ.2.AND.A3.EQ.5.AND.A4.EQ.6) THEN
C          CONFIGURATION N°1
           CALL XPENTE(1,CNSE,104,102,CONNEC(IT,3),103,101,CONNEC(IT,2))
           CALL XPENTE(4,CNSE,CONNEC(IT,1),101,102,CONNEC(IT,4),103,104)
          ELSEIF (A1.EQ.1.AND.A2.EQ.3.AND.A3.EQ.4.AND.A4.EQ.6) THEN
C          CONFIGURATION N°2
           CALL XPENTE(1,CNSE,101,CONNEC(IT,2),103,102,CONNEC(IT,4),104)
           CALL XPENTE(4,CNSE,102,101,CONNEC(IT,1),104,103,CONNEC(IT,3))
          ELSEIF (A1.EQ.2.AND.A2.EQ.3.AND.A3.EQ.4.AND.A4.EQ.5) THEN
C          CONFIGURATION N°3
           CALL XPENTE(1,CNSE,101,103,CONNEC(IT,3),102,104,CONNEC(IT,4))
           CALL XPENTE(4,CNSE,CONNEC(IT,2),104,103,CONNEC(IT,1),102,101)
          ELSE 
C          PROBLEME DE DECOUPAGE A 4 POINTS
           CALL ASSERT(A1.EQ.1.AND.A2.EQ.2.AND.A3.EQ.5.AND.A4.EQ.6)
          ENDIF
        ENDIF


      ENDIF

C      WRITE(6,*)'NSE ',NSE
C      DO 99 ISE=1,NSE
C          WRITE(6,*)(CNSE(ISE,J),J=1,4)
C 99   CONTINUE
C        WRITE(6,*)'  '

C-----------------------------------------------------------------------
C     VÉRIFICATION DU SENS DES SOUS-ÉLÉMENTS TETRA
C                  ALGO BOOK III (28/04/04)
C-----------------------------------------------------------------------

      IF (NDIME.EQ.3) THEN

        DO 200 ISE=1,NSE
           DO 210 IN=1,4
            INH=CNSE(ISE,IN)
            IF (INH.LT.100) THEN
              DO 220 J=1,3
                XYZ(IN,J)=ZR(IGEOM-1+3*(INH-1)+J)
 220          CONTINUE
            ELSE
              DO 221 J=1,3
                XYZ(IN,J)=ZR(JPTINT-1+3*(INH-100-1)+J)
 221          CONTINUE
            ENDIF
 210      CONTINUE

          DO 230 J=1,3
            AB(J)=XYZ(2,J)-XYZ(1,J)
            AC(J)=XYZ(3,J)-XYZ(1,J)
            AD(J)=XYZ(4,J)-XYZ(1,J)
 230      CONTINUE

          CALL PROVEC(AB,AC,VN)
          PS=DDOT(3,VN,1,AD,1)

          IF (PS.LT.0) THEN
C           WRITE(6,*)'MAUVAIS SENS DU TETRA'
C          ON INVERSE LES NOEUDS 3 ET 4
            INH=CNSE(ISE,3)
            CNSE(ISE,3)=CNSE(ISE,4)
            CNSE(ISE,4)=INH
          ELSE
C            WRITE(6,*)'VERIF SENS OK'
          ENDIF

 200    CONTINUE


      ENDIF

C-----------------------------------------------------------------------
C             MATRICE DES COORDONNÉES ET FONCTION HEAVYSIDE
C             ALGO BOOK III (28/04/04)
C-----------------------------------------------------------------------

      CALL WKVECT(HEAV,'V V R',NSE,JHEAV)

      DO 300 ISE=1,NSE

        ZR(JHEAV-1+ISE)=1.D0
        DO 310 IN=1,NDIME+1
          INH=CNSE(ISE,IN)
          IF (INH.LT.100) THEN
            DO 320 J=1,NDIM
              ZR(JCOSE-1+NDIM*(NDIM+1)*(ISE-1)+NDIM*(IN-1)+J)=
     &         ZR(IGEOM-1+NDIM*(INH-1)+J)
 320        CONTINUE
            IF (LSN(INH).LT.0.D0) ZR(JHEAV-1+ISE)=-1.D0
          ELSE
            DO 330 J=1,NDIM
              ZR(JCOSE-1+NDIM*(NDIM+1)*(ISE-1)+NDIM*(IN-1)+J)=
     &        ZR(JPTINT-1+NDIM*(INH-100-1)+J)
 330        CONTINUE
          ENDIF
 310    CONTINUE

 300  CONTINUE

C     REMARQUE IMPORTANTE :
C     IN ON EST SUR UN ELEMENT DE BORD COINCIDANT AVEC L'INTERCE
C     (NDIME = NDIM - 1 ET NPTS = NINTER > 0) ALORS ON NE PEUT PAS
C     DÉTERMINER DE QUEL COTÉ DE L'INTERFACE ON SE TROUVE, CAR ON
C     EST TOUJOURS SUR L'INTERFACE. LA VALEUR DE ZR(JHEAV-1+ISE)
C     EST DONC FAUSSE DANS CE CAS : on met 99.
C     UNE CORRECTION EST FAITE DANS XORIPE LORS DE L'ORIENTATION
C     DES NORMALES, OU ON EN PROFITE POUR CORRIGER AUSSI ZR(JHEAV-1+ISE)
      IF (NDIME.EQ.NDIM-1.AND.NPTS.EQ.NINTER.AND.NINTER.GT.0) THEN
        CALL ASSERT(NSE.EQ.1)
         ZR(JHEAV-1+1)=99.D0
      ENDIF
      

      CALL JEDEMA()
      END
