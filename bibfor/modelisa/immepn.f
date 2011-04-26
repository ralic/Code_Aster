      SUBROUTINE IMMEPN(NBCNX,XYZMA,X3DCA,ITETRA,XBAR,IMMER)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
C  DESCRIPTION : TENTATIVE D'IMMERSION D'UN NOEUD CABLE X3DCA(3) DANS
C  -----------   UNE MAILLE PENTAEDRE APPARTENANT A LA STRUCTURE BETON
C                APPELANT : IMMENO
C
C  IN     : NBCNX  : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DE LA MAILLE PENTAEDRE DANS
C                    LAQUELLE EST TENTEE L'IMMERSION
C  IN     : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
C                    TABLEAU DES COORDONNEES DES NOEUDS DE LA MAILLE
C                    PENTAEDRE DANS LAQUELLE EST TENTEE L'IMMERSION
C  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU NOEUD CABLE
C  OUT    : ITETRA : INTEGER , SCALAIRE
C                    SI IMMERSION REUSSIE : INDICATEUR DU SOUS-DOMAINE
C                    TETRAEDRE AUQUEL APPARTIENT LE NOEUD CABLE
C                    ITETRA = 1 OU 2 OU 3
C  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 4
C                    SI IMMERSION REUSSIE : COORDONNEES BARYCENTRIQUES
C                    DU NOEUD CABLE DANS LE SOUS-DOMAINE TETRAEDRE
C                    AUQUEL IL APPARTIENT
C  OUT    : IMMER  : INTEGER , SCALAIRE
C                    INDICE D'IMMERSION
C                    IMMER = -1  IMMERSION NON REUSSIE
C                    IMMER =  0  LE NOEUD CABLE EST A L'INTERIEUR
C                                DE LA MAILLE
C                    IMMER = 100 + 10 * NUMERO DE FACE
C                                LE NOEUD CABLE EST SUR UNE FACE
C                                DE LA MAILLE
C                    IMMER = 100 + 10 * NUMERO DE FACE + NUMERO D'ARETE
C                                LE NOEUD CABLE EST SUR UNE ARETE
C                                DE LA MAILLE
C                    IMMER =  2  LE NOEUD CABLE COINCIDE AVEC UN DES
C                                NOEUDS DE LA MAILLE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER       NBCNX, ITETRA, IMMER
      REAL*8        XYZMA(3,*), X3DCA(*), XBAR(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER       IDC, ID(8),II, J, KTEST
      REAL*8        D, DX, DY, DZ
      INTEGER       F1(4),F2(4),F3(4)
C
      REAL*8        R8PREM
      LOGICAL       FACNP1,FACNP2,FACNP3
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C     INDICATEUR SI DES FACES NON PLANES A 4 NOEUDS ONT DU ETRE
C     RENUMEROTEES AFIN QUE LA SURFACE DECRITE DEVIENNE ENVELOPPE
C     CONVEXE DU VOLUME TETRAEDRE
C
      FACNP1=.FALSE.
      FACNP2=.FALSE.
      FACNP3=.FALSE.

CCCC    ORIENTATION PREMIERE FACE QUADRANGULAIRE NOEUDS 4-1-2-5

      F1(1)=4
      F1(2)=1
      F1(3)=2
      F1(4)=5

      CALL COTFAC(XYZMA,F1(1),F1(2),F1(3),3,XYZMA(1,F1(4)),IDC)
      IF (IDC.LT.0) THEN
         II   =F1(4)
         F1(4)=F1(1)
         F1(1)=F1(2)
         F1(2)=F1(3)
         F1(3)=II
         FACNP1=.TRUE.
      ENDIF

CCCC    ORIENTATION DEUXIEME FACE QUADRANGULAIRE NOEUDS 5-2-3-6

      F2(1)=5
      F2(2)=2
      F2(3)=3
      F2(4)=6

      CALL COTFAC(XYZMA,F2(1),F2(2),F2(3),1,XYZMA(1,F2(4)),IDC)
      IF (IDC.LT.0) THEN
         II   =F2(4)
         F2(4)=F2(1)
         F2(1)=F2(2)
         F2(2)=F2(3)
         F2(3)=II
         FACNP2=.TRUE.
      ENDIF

CCCC    ORIENTATION TROISIEME FACE QUADRANGULAIRE NOEUDS 3-1-4-6

      F3(1)=3
      F3(2)=1
      F3(3)=4
      F3(4)=6

      CALL COTFAC(XYZMA,F3(1),F3(2),F3(3),2,XYZMA(1,F3(4)),IDC)
      IF (IDC.LT.0) THEN
         II   =F3(4)
         F3(4)=F3(1)
         F3(1)=F3(2)
         F3(2)=F3(3)
         F3(3)=II
         FACNP3=.TRUE.
      ENDIF



      II=0
CCCC    POSITION COTE INTERNE PREMIERE FACE QUAD (2 PLANS)
      CALL COTFAC(XYZMA,F1(1),F1(2),F1(3),3,
     &            X3DCA(1),ID(1))
      IF(ID(1).GE.0) THEN
        II=II+1
        CALL COTFAC(XYZMA,F1(3),F1(4),F1(1),3,
     &              X3DCA(1),ID(2))
CCCC    POSITION COTE INTERNE DEUXIEME FACE QUAD (2 PLANS)
        IF(ID(2).GE.0) THEN
          II=II+1
          CALL COTFAC(XYZMA,F2(1),F2(2),F2(3),1,
     &                X3DCA(1),ID(3))
          IF(ID(3).GE.0) THEN
            II=II+1
            CALL COTFAC(XYZMA,F2(3),F2(4),F2(1),1,
     &                  X3DCA(1),ID(4))
CCCC    POSITION COTE INTERNE TROISIEME FACE QUAD (2 PLANS)
            IF(ID(4).GE.0) THEN
              II=II+1
              CALL COTFAC(XYZMA,F3(1),F3(2),F3(3),2,
     &                    X3DCA(1),ID(5))
              IF(ID(5).GE.0) THEN
                II=II+1
                CALL COTFAC(XYZMA,F3(3),F3(4),F3(1),2,
     &                      X3DCA(1),ID(6))
CCCC    POSITION COTE INTERNE QUATRIEME FACE TRIA (1 PLAN)
                IF(ID(6).GE.0) THEN
                  II=II+1
                  CALL COTFAC(XYZMA,4,5,6,3,
     &                        X3DCA(1),ID(7))
CCCC    POSITION COTE INTERNE CINQUIEME FACE TRIA (1 PLAN)
                  IF(ID(7).GE.0) THEN
                    II=II+1
                    CALL COTFAC(XYZMA,1,2,3,4,
     &                          X3DCA(1),ID(8))
                      IF(ID(8).GE.0) II=II+1
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      NOEUD A L EXTERIEUR DU VOLUME DE LA MAILLE
C      ON A TROUVE : ON RESSORT COMPLETEMENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF (II.LT.8) THEN
C
        IMMER=-1
        GOTO 9999
C
      ELSE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        KTEST=0
        DO 10 J=1,8
           KTEST=KTEST+ID(J)
10      CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      NOEUD IMMERGE DANS LE VOLUME DE LA MAILLE
C      CALCUL DES COORDONNES BARYCENTRIQUES SAUF SI
C      COINCIDENCE AVEC NOEUD MILIEU SI MAILLE HEXA20 OU HEXA27
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
        IF (KTEST.GT.5) THEN
C
           IF (NBCNX.EQ.15) THEN
              DO 20 J=7,15,1
                DX = XYZMA(1,J) - X3DCA(1)
                DY = XYZMA(2,J) - X3DCA(2)
                DZ = XYZMA(3,J) - X3DCA(3)
                D  = DX*DX + DY*DY + DZ*DZ
                IF ( D.LT.R8PREM() ) THEN
                  IMMER=2
                  GOTO 9999
                ENDIF
20            CONTINUE
           ENDIF
C
C   TEST D'APPARTENANCE A UN SOUS-DOMAINE TETRAEDRE, PAR DETERMINATION
C   DES COORDONNEES BARYCENTRIQUES (DECOUPAGE DU PENTA EN TROIS TETRAS)
C
C.....TETRAEDRE 2-3-4-5
C
           ITETRA = 1
           CALL TSTBAR(4,XYZMA(1,2),XYZMA(1,3),XYZMA(1,4),
     &                 XYZMA(1,5),X3DCA(1),XBAR(1),IMMER)
           IF ( IMMER.GE.0 ) GOTO 9999
C
C.... TETRAEDRE 3-4-5-6
C
           ITETRA = 2
           CALL TSTBAR(4,XYZMA(1,3),XYZMA(1,4),XYZMA(1,5),
     &                 XYZMA(1,6),X3DCA(1),XBAR(1),IMMER)
           IF ( IMMER.GE.0 ) GOTO 9999
C
C.... TETRAEDRE 1-2-3-4
C
           ITETRA = 3
           CALL TSTBAR(4,XYZMA(1,1),XYZMA(1,2),XYZMA(1,3),
     &                 XYZMA(1,4),X3DCA(1),XBAR(1),IMMER)
           IF ( IMMER.GE.0 ) GOTO 9999
C
C  DANS LE CAS DE FACES REORIENTEE (FACNP VRAI) ON TESTE LA PRESENCE
C  DANS LES PETITS TETRAEDRES DEFINIS PAR CHAQUE FACE.
C
C.... TETRAEDRE 4-1-2-5
C
           IF (FACNP1) THEN
              ITETRA = 4
              CALL TSTBAR(4,XYZMA(1,4),XYZMA(1,1),XYZMA(1,2),
     &                    XYZMA(1,5),X3DCA(1),XBAR(1),IMMER)
              IF ( IMMER.GE.0 ) GOTO 9999
           ENDIF
C
C.... TETRAEDRE 5-2-3-6
C
           IF (FACNP2) THEN
              ITETRA = 5
              CALL TSTBAR(4,XYZMA(1,5),XYZMA(1,2),XYZMA(1,3),
     &                    XYZMA(1,6),X3DCA(1),XBAR(1),IMMER)
              IF ( IMMER.GE.0 ) GOTO 9999
           ENDIF
C
C.... TETRAEDRE 3-1-4-6
C
           IF (FACNP3) THEN
              ITETRA = 6
              CALL TSTBAR(4,XYZMA(1,3),XYZMA(1,1),XYZMA(1,4),
     &                    XYZMA(1,6),X3DCA(1),XBAR(1),IMMER)
              IF ( IMMER.GE.0 ) GOTO 9999
           ENDIF

           IF (IMMER.LT.0) THEN
            CALL U2MESS('F','MODELISA4_72')
           ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      NOEUD COINCIDANT AVEC UN NOEUD SOMMET -APPARTIENT A + DE 3 PLANS
C      ON A TROUVE : ON RESSORT COMPLETEMENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
        ELSE
C
           IMMER=2
           GOTO 9999
C
        ENDIF
      ENDIF

C
9999  CONTINUE
C
C --- FIN DE IMMEPN.
      END
