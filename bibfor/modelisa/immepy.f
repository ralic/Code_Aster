      SUBROUTINE IMMEPY(NBCNX,XYZMA,X3DCA,ITETRA,XBAR,IMMER)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
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
C-----------------------------------------------------------------------
C  DESCRIPTION : TENTATIVE D'IMMERSION D'UN NOEUD CABLE X3DCA(3) DANS
C  -----------   UNE MAILLE PYRAMIDE APPARTENANT A LA STRUCTURE BETON
C                APPELANT : IMMENO
C
C  IN     : NBCNX  : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DE LA MAILLE PYRAMIDE DANS
C                    LAQUELLE EST TENTEE L'IMMERSION
C  IN     : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
C                    TABLEAU DES COORDONNEES DES NOEUDS DE LA MAILLE
C                    PYRAMIDE DANS LAQUELLE EST TENTEE L'IMMERSION
C  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU NOEUD CABLE
C  OUT    : ITETRA : INTEGER , SCALAIRE
C                    SI IMMERSION REUSSIE : INDICATEUR DU SOUS-DOMAINE
C                    TETRAEDRE AUQUEL APPARTIENT LE NOEUD CABLE
C                    ITETRA = 1 OU 2
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
      REAL*8        D, DX, DY, DZ
      REAL*8        R8PREM
      INTEGER       KTEST,F1(4),IDC,ID(6),II,J
      LOGICAL       FACNP1
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      FACNP1=.FALSE.

CCCC    ORIENTATION FACE QUADRANGULAIRE NOEUDS 1-2-3-4

      F1(1)=1
      F1(2)=2
      F1(3)=3
      F1(4)=4

      CALL COTFAC(XYZMA,F1(1),F1(2),F1(3),5,XYZMA(1,F1(4)),IDC)
      IF (IDC.LT.0) THEN
         II   =F1(4)
         F1(4)=F1(1)
         F1(1)=F1(2)
         F1(2)=F1(3)
         F1(3)=II
         FACNP1=.TRUE.
      ENDIF


      II=0
CCCC    POSITION COTE INTERNE PREMIERE FACE (2 PLANS)
      CALL COTFAC(XYZMA,F1(1),F1(2),F1(3),5,
     &            X3DCA(1),ID(1))
      IF(ID(1).GE.0) THEN
        II=II+1
        CALL COTFAC(XYZMA,F1(3),F1(4),F1(1),5,
     &              X3DCA(1),ID(2))

CCCC    POSITION COTE INTERNE DEUXIEME FACE (1 PLAN)
        IF(ID(2).GE.0) THEN
          II=II+1
          CALL COTFAC(XYZMA,1,2,5,3,
     &                X3DCA(1),ID(3))

CCCC    POSITION COTE INTERNE TROISIEME FACE (1 PLAN)
          IF(ID(3).GE.0) THEN
            II=II+1
            CALL COTFAC(XYZMA,2,3,5,4,
     &                  X3DCA(1),ID(4))

CCCC    POSITION COTE INTERNE QUATRIEME FACE (1 PLAN)
            IF(ID(4).GE.0) THEN
              II=II+1
              CALL COTFAC(XYZMA,3,4,5,1,
     &                    X3DCA(1),ID(5))

CCCC    POSITION COTE INTERNE CINQUIEME FACE (1 PLAN)
              IF(ID(5).GE.0) THEN
                II=II+1
                CALL COTFAC(XYZMA,4,1,5,2,
     &                      X3DCA(1),ID(6))
                IF(ID(6).GE.0) II=II+1
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
      IF (II.LT.6) THEN
C
        IMMER=-1
        GOTO 9999
C
      ELSE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        KTEST=0
        DO 10 J=1,6
           KTEST=KTEST+ID(J)
10      CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      NOEUD IMMERGE DANS LE VOLUME DE LA MAILLE
C      CALCUL DES COORDONNES BARYCENTRIQUES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
        IF (KTEST.GT.3) THEN
C
           IF (NBCNX.EQ.13) THEN
              DO 20 J=6,13,1
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
C     TEST D'APPARTENANCE A UN SOUS-DOMAINE TETRAEDRE, PAR DETERMINATION
C     DES COORDONNEES BARYCENTRIQUES (DECOUPAGE DU PYRA EN DEUX TETRAS)
C
C.....TETRAEDRE 1-2-3-5
C
           ITETRA = 1
           CALL TSTBAR(4,XYZMA(1,1),XYZMA(1,2),XYZMA(1,3),
     &                 XYZMA(1,5),X3DCA(1),XBAR(1),IMMER)
           IF ( IMMER.GE.0 ) GOTO 9999
C
C.... TETRAEDRE 1-3-4-5
C
           ITETRA = 2
           CALL TSTBAR(4,XYZMA(1,1),XYZMA(1,3),XYZMA(1,4),XYZMA(1,5),
     &                   X3DCA(1),XBAR(1),IMMER)
           IF ( IMMER.GE.0 ) GOTO 9999
C
C  DANS LE CAS DE FACES REORIENTEE (FACNP1 VRAI) ON TESTE LA PRESENCE
C  DANS LES PETITS TETRAEDRES DEFINIS PAR CHAQUE FACE.
C
C.... TETRAEDRE 1-2-3-4
C
           IF (FACNP1) THEN
              ITETRA = 3
              CALL TSTBAR(4,XYZMA(1,1),XYZMA(1,2),XYZMA(1,3),
     &                    XYZMA(1,4),X3DCA(1),XBAR(1),IMMER)
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
C --- FIN DE IMMEPY.
      END
