      SUBROUTINE XDECOV(NDIM,ELP,NNOP,NNOSE,IT,PINTT,CNSET,
     &                 HEAVT,NCOMP,LSN,FISCO,IGEOM,NFISS,
     &                 IFISS,PINTER,NINTER,NPTS,AINTER,NSE,CNSE,HEAV,
     &                 NFISC)
      IMPLICIT NONE

      REAL*8        LSN(*),PINTT(*)
      INTEGER       NDIM,NNOP,NNOSE,IT,CNSET(*),HEAVT(*),NCOMP,IGEOM
      INTEGER       NINTER,NPTS,NFISS,IFISS,NSE,CNSE(6,6),FISCO(*),NFISC
      CHARACTER*24  PINTER,AINTER,HEAV
      CHARACTER*8   ELP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20 CRP_21 CRS_1404
C                      DÉCOUPER LE TETRA EN NSE SOUS-TETRAS
C
C     ENTREE
C       NNOSE    : NOMBRE DE NOEUDS DU SOUS TETRA
C       IT       : INDICE DU TETRA EN COURS
C       CNSET    : CONNECTIVITÉ DES NOEUDS DU TETRA
C       HEAVT    : FONCTION HEAVYSIDE DES TETRAS
C       LSN      : VALEURS DE LA LEVEL SET NORMALE
C       IGEOM    : ADRESSE DES COORDONNÉES DES NOEUDS DE L'ELT PARENT
C       PINTER   : COORDONNÉES DES POINTS D'INTERSECTION
C       NINTER   : NB DE POINTS D'INTERSECTION
C       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN NOEUD
C       AINTER   : INFOS ARETE CORRESPONDATE AU PT INTERSECTION
C
C     SORTIE
C       NSE      : NOMBRE DE SOUS-ELTS (TETRAS)
C       CNSE     : CONNECTIVITE DES SOUS-ÉLÉMENTS (TETRAS)
C       HEAV     : FONCTION HEAVYSIDE CONSTANTE SUR CHAQUE SOUS-ELT
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
      REAL*8          XYZ(4,3),AB(3),AC(3),AD(3),VN(3),PS,GEOM(3)
      REAL*8          SOMLSN(NFISC+1),FF(NNOP),RBID,RBID2(NDIM)
      INTEGER         JPTINT,JAINT,JHEAV
      INTEGER         IN,INH,I,J,AR(12,3),NBAR,ISE,IBID
      INTEGER         A1,A2,A3,A4,A,B,C,NDIME
      CHARACTER*8     TYPMA,ELRESE(3)
      INTEGER         ZXAIN,XXMMVD,MXSTAC
      LOGICAL         LBID,LTEATT, AXI
      PARAMETER      (MXSTAC=1000)

      DATA            ELRESE /'SEG2','TRIA3','TETRA4'/
C ----------------------------------------------------------------------

      CALL JEMARQ()

C     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
C     (VOIR CRS 1404)
      CALL ASSERT(NNOP.LE.MXSTAC)
      CALL ASSERT(NFISC.LE.MXSTAC)
      CALL ASSERT(NDIM.LE.MXSTAC)


      CALL ELREF4(' ','RIGI',NDIME,IBID,IBID,IBID,IBID,IBID,IBID,IBID)
      ZXAIN = XXMMVD('ZXAIN')

      AXI = LTEATT(' ','AXIS','OUI')
C     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
C     NDIM EST LA DIMENSION DU MAILLAGE
C     NDIME EST DIMENSION DE L'ELEMENT FINI
C     PAR EXEMPLE, POUR LES ELEMENT DE BORDS D'UN MAILLAGE 3D :
C     NDIME = 2 ALORS QUE NDIM = 3

      CALL JEVEUO(PINTER,'L',JPTINT)
      CALL JEVEUO(AINTER,'L',JAINT)

      DO 10 IN=1,6
        DO 20 J=1,6
          CNSE(IN,J)=0
 20     CONTINUE
 10   CONTINUE

      TYPMA=ELRESE(NDIME)
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
          DO 90 IN=1,NNOSE
            CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 90       CONTINUE
        ELSEIF (NINTER .EQ. 2) THEN
          A1=NINT(ZR(JAINT-1+ZXAIN*(1-1)+1))
          A2=NINT(ZR(JAINT-1+ZXAIN*(2-1)+1))
          IF (NPTS .EQ. 2) THEN
C           1 SEUL ELEMENT
            NSE=1
            DO 91 IN=1,NNOSE
              CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 91         CONTINUE
          ELSEIF (NPTS .EQ. 1) THEN
C           2 ELEMENTS
            NSE=2
            CALL ASSERT(A1.EQ.0.AND.A2.NE.0)
            CNSE(1,1)=NINT(ZR(JAINT-1+ZXAIN*(NPTS-1)+2))
            CNSE(1,2)=102
            CNSE(1,3)=CNSET(NNOSE*(IT-1)+AR(A2,1))
            CNSE(2,1)=NINT(ZR(JAINT-1+ZXAIN*(NPTS-1)+2))
            CNSE(2,2)=102
            CNSE(2,3)=CNSET(NNOSE*(IT-1)+AR(A2,2))
          ELSE
C           3 ELEMENTS
            NSE=3
            CALL ASSERT(A1.NE.0)
C           101 ET 102 LES 2 POINTS D'INTERSECTION
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
            CNSE(1,3)=CNSET(NNOSE*(IT-1)+A)
            CNSE(2,1)=101
            CNSE(2,2)=102
            CNSE(2,3)=CNSET(NNOSE*(IT-1)+C)
            CNSE(3,1)=101
            CNSE(3,2)=CNSET(NNOSE*(IT-1)+B)
            CNSE(3,3)=CNSET(NNOSE*(IT-1)+C)
          ENDIF
        ELSEIF (NINTER .EQ. 3) THEN
C         L'INTERFACE COINCIDE AVEC LE TRIA
          CALL ASSERT (NPTS.EQ.NINTER)
C         1 SEUL ELEMENT
          NSE=1
          DO 92 IN=1,NNOSE
            CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
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
            CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 95       CONTINUE
        ELSEIF (NINTER .EQ. 1) THEN
          A1=NINT(ZR(JAINT-1+ZXAIN*(1-1)+1))
          IF (NPTS .EQ. 1) THEN
C           1 SEUL ELEMENT
            NSE=1
            DO 96 IN=1,2
              CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 96         CONTINUE
          ELSEIF (NPTS .EQ. 0) THEN
C           2 ELEMENTS
            NSE=2
            CALL ASSERT(A1.NE.0)
                  A=AR(A1,1)
                  B=AR(A1,2)

C           101 ET 102 LES 2 POINTS D'INTERSECTION
C           ON SE PLACE DANS LA CONF DE REF (VOIR ALGO)
            CNSE(1,1)=101
            CNSE(1,2)=CNSET(NNOSE*(IT-1)+A)
            CNSE(2,1)=101
            CNSE(2,2)=CNSET(NNOSE*(IT-1)+B)
          ENDIF
        ELSEIF (NINTER .EQ. 2) THEN
C         L'INTERFACE COINCIDE AVEC LE SEG
          CALL ASSERT (NPTS.EQ.NINTER)
C         1 SEUL ELEMENT
          NSE=1
          DO 97 IN=1,2
            CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
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
          DO 100 IN=1,NNOSE
              CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 100      CONTINUE

        ELSEIF (NINTER.EQ.3) THEN

C         2°) AVEC TROIS POINTS D'INTERSECTION
C         ------------------------------------
          A1=NINT(ZR(JAINT-1+ZXAIN*(1-1)+1))
          A2=NINT(ZR(JAINT-1+ZXAIN*(2-1)+1))
          A3=NINT(ZR(JAINT-1+ZXAIN*(3-1)+1))

          IF (NPTS.EQ.3) THEN
C           ON A UN SEUL ELEMENT
            NSE=1
            DO 110 IN=1,NNOSE
              CNSE(1,IN)=CNSET(NNOSE*(IT-1)+IN)
 110        CONTINUE

          ELSEIF (NPTS.EQ.2) THEN
C           ON A DEUX SOUS-ELEMENTS
            NSE=2
            CALL ASSERT(A1.EQ.0.AND.A2.EQ.0)
            CALL ASSERT(NINT(ZR(JAINT-1+2)).GT.0.AND.
     &                  NINT(ZR(JAINT-1+ZXAIN+2)).GT.0)

C           CONNECTIVITE DES NSE PAR RAPPORT AU NUM DE NOEUDS DU PARENT
C           AVEC 101, 102 ET 103 LES 3 PTS D'INTERSECTION
C           ON REMPLACE 101 ET 102 PAR LES NUMEROS DES NOEUDS COUP�S
            CNSE(1,1)=NINT(ZR(JAINT-1+2))
            CNSE(1,2)=NINT(ZR(JAINT-1+ZXAIN+2))
            CNSE(1,3)=103
            CNSE(1,4)=CNSET(NNOSE*(IT-1)+AR(A3,1))
            CNSE(2,1)=NINT(ZR(JAINT-1+2))
            CNSE(2,2)=NINT(ZR(JAINT-1+ZXAIN+2))
            CNSE(2,3)=103
            CNSE(2,4)=CNSET(NNOSE*(IT-1)+AR(A3,2))

          ELSEIF (NPTS.EQ.1) THEN
C           ON A TROIS SOUS-ELEMENTS
            NSE=3
            CALL ASSERT(A1.EQ.0.AND.A2.NE.0)
            CALL ASSERT(NINT(ZR(JAINT-1+2)).GT.0)
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
C           ON REMPLACE 101 PAR LE NUMERO DU NOEUD COUP�
            CNSE(1,1)=NINT(ZR(JAINT-1+2))
            CNSE(1,2)=102
            CNSE(1,3)=103
            CNSE(1,4)=CNSET(NNOSE*(IT-1)+A)
            CNSE(2,1)=NINT(ZR(JAINT-1+2))
            CNSE(2,2)=102
            CNSE(2,3)=103
            CNSE(2,4)=CNSET(NNOSE*(IT-1)+C)
            CNSE(3,1)=NINT(ZR(JAINT-1+2))
            CNSE(3,2)=102
            CNSE(3,3)=CNSET(NNOSE*(IT-1)+B)
            CNSE(3,4)=CNSET(NNOSE*(IT-1)+C)

          ELSEIF (NPTS.EQ.0) THEN
C           ON A QUATRE SOUS-ELEMENTS
            NSE=4
            CNSE(1,1)=101
            CNSE(1,2)=102
            CNSE(1,3)=103

C           ON A 4 CONFIG POSSIBLES :
            IF (A1.EQ.1.AND.A2.EQ.2.AND.A3.EQ.3) THEN
C             CONFIGURATION N°1
              CNSE(1,4)=CNSET(NNOSE*(IT-1)+1)
              CALL XPENTE(2,CNSE,103,101,102,CNSET(NNOSE*(IT-1)+4),
     &                    CNSET(NNOSE*(IT-1)+2),CNSET(NNOSE*(IT-1)+3))
            ELSEIF (A1.EQ.1.AND.A2.EQ.4.AND.A3.EQ.5) THEN
C             CONFIGURATION N°2
              CNSE(1,4)=CNSET(NNOSE*(IT-1)+2)
              CALL XPENTE(2,CNSE,CNSET(NNOSE*(IT-1)+1),
     &                    CNSET(NNOSE*(IT-1)+3),CNSET(NNOSE*(IT-1)+4),
     &                    101,102,103)
            ELSEIF (A1.EQ.2.AND.A2.EQ.4.AND.A3.EQ.6) THEN
C             CONFIGURATION N°3
              CNSE(1,4)=CNSET(NNOSE*(IT-1)+3)
              CALL XPENTE(2,CNSE,CNSET(NNOSE*(IT-1)+4),
     &                    CNSET(NNOSE*(IT-1)+2),CNSET(NNOSE*(IT-1)+1),
     &                    103,102,101)
            ELSEIF (A1.EQ.3.AND.A2.EQ.5.AND.A3.EQ.6) THEN
C             CONFIGURATION N°4
              CNSE(1,4)=CNSET(NNOSE*(IT-1)+4)
              CALL XPENTE(2,CNSE,CNSET(NNOSE*(IT-1)+1),
     &                    CNSET(NNOSE*(IT-1)+2),CNSET(NNOSE*(IT-1)+3),
     &                    101,102,103)
            ELSE
C             PROBLEME DE DECOUPAGE À 3 POINTS
              CALL ASSERT(A1.EQ.1.AND.A2.EQ.2.AND.A3.EQ.3)
            ENDIF

          ENDIF

        ELSEIF (NINTER.EQ.4) THEN

C         2°) AVEC QUATRE POINTS D'INTERSECTION
C          -------------------------------------
          A1=NINT(ZR(JAINT-1+ZXAIN*(1-1)+1))
          A2=NINT(ZR(JAINT-1+ZXAIN*(2-1)+1))
          A3=NINT(ZR(JAINT-1+ZXAIN*(3-1)+1))
          A4=NINT(ZR(JAINT-1+ZXAIN*(4-1)+1))

C         ON A SIX SOUS-ELEMENTS (DANS TOUS LES CAS ?)
          NSE=6
          IF (A1.EQ.1.AND.A2.EQ.2.AND.A3.EQ.5.AND.A4.EQ.6) THEN
C          CONFIGURATION N°1
           CALL XPENTE(1,CNSE,104,102,CNSET(NNOSE*(IT-1)+3),
     &                 103,101,CNSET(NNOSE*(IT-1)+2))
           CALL XPENTE(4,CNSE,CNSET(NNOSE*(IT-1)+1),101,102,
     &                 CNSET(NNOSE*(IT-1)+4),103,104)
          ELSEIF (A1.EQ.1.AND.A2.EQ.3.AND.A3.EQ.4.AND.A4.EQ.6) THEN
C          CONFIGURATION N°2
           CALL XPENTE(1,CNSE,101,CNSET(NNOSE*(IT-1)+2),103,
     &                 102,CNSET(NNOSE*(IT-1)+4),104)
           CALL XPENTE(4,CNSE,102,101,CNSET(NNOSE*(IT-1)+1),
     &                 104,103,CNSET(NNOSE*(IT-1)+3))
          ELSEIF (A1.EQ.2.AND.A2.EQ.3.AND.A3.EQ.4.AND.A4.EQ.5) THEN
C          CONFIGURATION N°3
           CALL XPENTE(1,CNSE,101,103,CNSET(NNOSE*(IT-1)+3),
     &                 102,104,CNSET(NNOSE*(IT-1)+4))
           CALL XPENTE(4,CNSE,CNSET(NNOSE*(IT-1)+2),104,103,
     &                 CNSET(NNOSE*(IT-1)+1),102,101)
          ELSE
C          PROBLEME DE DECOUPAGE A 4 POINTS
           CALL ASSERT(A1.EQ.1.AND.A2.EQ.2.AND.A3.EQ.5.AND.A4.EQ.6)
          ENDIF
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     VÉRIFICATION DU SENS DES SOUS-ÉLÉMENTS TETRA
C                  ALGO BOOK III (28/04/04)
C-----------------------------------------------------------------------

      IF (NDIME.EQ.3) THEN

        DO 200 ISE=1,NSE
           DO 210 IN=1,NNOSE
            INH=CNSE(ISE,IN)
            IF (INH.LT.100) THEN
              DO 220 J=1,3
                XYZ(IN,J)=ZR(IGEOM-1+NDIM*(INH-1)+J)
 220          CONTINUE
            ELSEIF (INH.GT.100.AND.INH.LT.1000) THEN
              DO 221 J=1,3
                XYZ(IN,J)=ZR(JPTINT-1+NDIM*(INH-100-1)+J)
 221          CONTINUE
            ELSE
              DO 222 J=1,3
                 XYZ(IN,J)=PINTT(NDIM*(INH-1001)+J)
 222          CONTINUE
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
C          MAUVAIS SENS DU TETRA, ON INVERSE LES NOEUDS 3 ET 4
            INH=CNSE(ISE,3)
            CNSE(ISE,3)=CNSE(ISE,4)
            CNSE(ISE,4)=INH
          ENDIF

 200    CONTINUE


      ENDIF

C-----------------------------------------------------------------------
C             MATRICE DES COORDONNÉES ET FONCTION HEAVYSIDE
C             ALGO BOOK III (28/04/04)
C-----------------------------------------------------------------------

      CALL WKVECT(HEAV,'V V R',NSE*IFISS,JHEAV)
      DO 300 ISE=1,NSE
        DO 310 I =1,IFISS-1
C ----- ON RECOPIE LES VALEURS PR�C�DENTES
          ZR(JHEAV-1+IFISS*(ISE-1)+I)=HEAVT(NCOMP*(I-1)+IT)
 310    CONTINUE
C ----- ON TRAITE LA FISSURE COURANTE
        CALL VECINI(NFISC+1,0.D0,SOMLSN)
        DO 320 IN=1,NNOSE
          INH=CNSE(ISE,IN)
          IF (INH.LT.100) THEN
            DO 325 I = 1,NFISC
              SOMLSN(I) = SOMLSN(I)+LSN((INH-1)*NFISS+FISCO(2*I-1))
 325        CONTINUE
            SOMLSN(NFISC+1) = SOMLSN(NFISC+1)+LSN((INH-1)*NFISS+IFISS)
          ELSE
C           RECUP DE LA G�OMETRIE
            IF (INH.GT.1000) THEN
              DO 330 J=1,NDIM
                GEOM(J) = PINTT(NDIM*(INH-1001)+J)
 330          CONTINUE
            ELSEIF (INH.LT.1000) THEN
              DO 340 J=1,NDIM
                GEOM(J) = ZR(JPTINT-1+NDIM*(INH-101)+J)
 340          CONTINUE
            ENDIF
C           CALCUL DES FF


            CALL REEREF(ELP,AXI, NNOP,IBID,IGEOM,GEOM,IBID,LBID,
     &              NDIM,RBID,RBID, RBID,
     &              IBID,IBID,IBID,IBID,IBID,IBID,RBID,RBID,'NON',
     &              RBID2,FF,RBID,RBID,RBID,RBID)

            DO 350 J=1,NNOP
              DO 355 I=1,NFISC
                SOMLSN(I)=SOMLSN(I)+FF(J)*LSN((J-1)*NFISS+FISCO(2*I-1))
 355          CONTINUE
              SOMLSN(NFISC+1) = SOMLSN(NFISC+1)+FF(J)
     &                          *LSN((J-1)*NFISS+IFISS)
 350        CONTINUE
          ENDIF
 320    CONTINUE
C
C       MISE � ZERO POUR LA FONCTION JONCTION AU NIVEAU DU BRANCHEMENT
C
        DO 360 I =1,NFISC
          IF (FISCO(2*I)*SOMLSN(I).GT.0.D0) GOTO 300
 360    CONTINUE
C
        IF (SOMLSN(NFISC+1).LT.0.D0) THEN
          ZR(JHEAV-1+IFISS*ISE) = -1.D0
        ELSEIF (SOMLSN(NFISC+1).GT.0.D0) THEN
          ZR(JHEAV-1+IFISS*ISE) = +1.D0
        ELSE
C       REMARQUE IMPORTANTE :
C       SI ON EST SUR UN ELEMENT DE BORD COINCIDANT AVEC L'INTERCE
C       (NDIME = NDIM - 1 ET NPTS = NINTER = NDIM) ALORS ON NE PEUT PAS
C       DÉTERMINER DE QUEL COTÉ DE L'INTERFACE ON SE TROUVE, CAR
C       ON EST TOUJOURS SUR L'INTERFACE. LA VALEUR DE ZR(JHEAV-1+ISE)
C       EST DONC FAUSSE DANS CE CAS : ON MET 99.
C       UNE CORRECTION EST FAITE DANS XORIPE LORS DE L'ORIENTATION DES
C       NORMALES, OU ON EN PROFITE POUR CORRIGER AUSSI ZR(JHEAV-1+ISE)
          CALL ASSERT(NDIME.EQ.NDIM-1.AND.NPTS.EQ.NDIM.AND.NSE.EQ.1)
          ZR(JHEAV-1+IFISS*ISE) = 99.D0
        ENDIF

 300  CONTINUE

      CALL JEDETR(AINTER)

      CALL JEDEMA()
      END
