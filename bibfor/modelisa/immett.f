      SUBROUTINE IMMETT(NBCNX,XYZMA,X3DCA,ITETRA,XBAR,IMMER)
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
C  -----------   UNE MAILLE TETRAEDRE APPARTENANT A LA STRUCTURE BETON
C                APPELANT : IMMENO
C
C  IN     : NBCNX  : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DE LA MAILLE TETRAEDRE DANS
C                    LAQUELLE EST TENTEE L'IMMERSION
C  IN     : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
C                    TABLEAU DES COORDONNEES DES NOEUDS DE LA MAILLE
C                    TETRAEDRE DANS LAQUELLE EST TENTEE L'IMMERSION
C  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU NOEUD CABLE
C  OUT    : ITETRA : INTEGER , SCALAIRE
C                    SI IMMERSION REUSSIE : ITETRA = 1
C  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 4
C                    SI IMMERSION REUSSIE : COORDONNEES BARYCENTRIQUES
C                    DU NOEUD CABLE DANS LE TETRAEDRE
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
      INTEGER       IDC, ID(4),II, J, KTEST
      REAL*8        D, DX, DY, DZ
C
      REAL*8        R8PREM
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      II=0
CCCC    POSITION COTE INTERNE PREMIERE FACE (1 PLAN)
      CALL COTFAC(XYZMA,1,2,3,4,X3DCA(1),ID(1))
CCCC    POSITION COTE INTERNE DEUXIEME FACE (1 PLAN)
      IF(ID(1).GE.0) THEN
        II=II+1
        CALL COTFAC(XYZMA,2,3,4,1,X3DCA(1),ID(2))
CCCC    POSITION COTE INTERNE TROISIEME FACE (1 PLAN)
        IF(ID(2).GE.0) THEN
          II=II+1
          CALL COTFAC(XYZMA,3,4,1,2,X3DCA(1),ID(3))
CCCC    POSITION COTE INTERNE QUATRIEME FACE (1 PLAN)
          IF(ID(3).GE.0) THEN
            II=II+1
            CALL COTFAC(XYZMA,1,2,4,3,X3DCA(1),ID(4))
            IF(ID(4).GE.0) II=II+1
          ENDIF
        ENDIF
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      NOEUD A L EXTERIEUR DU VOLUME DE LA MAILLE
C      ON A TROUVE : ON RESSORT COMPLETEMENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF (II.LT.4) THEN
C
        IMMER=-1
        GOTO 9999
C
      ELSE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        KTEST=0
        DO 10 J=1,4
           KTEST=KTEST+ID(J)
10      CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      NOEUD IMMERGE DANS LE VOLUME DE LA MAILLE
C      CALCUL DES COORDONNES BARYCENTRIQUES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
        IF (KTEST.GT.1) THEN
C
           IF (NBCNX.EQ.10) THEN
              DO 20 J=5,10,1
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
C     TEST D'APPARTENANCE A UN SOUS-DOMAINE TETRAEDRE PAR DETERMINATION
C     DES COORDONNEES BARYCENTRIQUES (DECOUPAGE D HEXA EN CINQ TETRAS)
C
C.....TETRAEDRE 1-2-3-4
C
           ITETRA = 1
           CALL TSTBAR(4,XYZMA(1,1),XYZMA(1,2),XYZMA(1,3),
     &                 XYZMA(1,4),X3DCA(1),XBAR(1),IMMER)
           IF ( IMMER.GE.0 ) GOTO 9999

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
C --- FIN DE IMMETT.
      END
