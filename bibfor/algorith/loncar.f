      SUBROUTINE LONCAR(NDIM,TYPMA,COORD,L)
      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      INTEGER       NDIM
      REAL*8        COORD(*),L
      CHARACTER*8   TYPMA

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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

C                      LONGUEUR CARACTÉRISTIQUE D'UNE MAILLE

C     ENTREE
C       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
C       TYPMA   : TYPE DE MAILLE
C       COORD   : COORDONNÉES DES NOEUDS (X Y Z SI NDIM = 3
C                                         X Y   SI NDIM = 2)
C
C     SORTIE
C       L      : LONGUEUR CARACTÉRISTIQUE
C     ------------------------------------------------------------------
C
      INTEGER        I
      REAL*8         AR(3),M(3)
C ----------------------------------------------------------------------

      CALL JEMARQ()

C     ATTENTION,
C     NDIM EST LA DIMENSION DU MAILLAGE
C     POUR LES MAILLES DE BORD, CE N'EST PAS LA DIMENSION DE LA MAILLE

      CALL ASSERT(NDIM.EQ.2.OR.NDIM.EQ.3)

      IF (TYPMA(1:4).EQ.'HEXA') THEN

C       LA LONGUEUR CARACTÉRISTIQUE EST LA GRANDE DIAGONALE N1-N7
        L=SQRT( (COORD(1)-COORD(19))**2 + (COORD(2)-COORD(20))**2
     &                                  + (COORD(3)-COORD(21))**2 )

      ELSEIF (TYPMA(1:5).EQ.'PENTA') THEN

C       LA LONGUEUR CARACTÉRISTIQUE EST ((N3-N1)*(N3-N2)*(N3-N6))^(1/3)
        AR(1)=SQRT((COORD(7)-COORD(1))**2  + (COORD(8)-COORD(2))**2
     &                                     + (COORD(9)-COORD(3))**2 )
        AR(2)=SQRT((COORD(7)-COORD(4))**2  + (COORD(8)-COORD(5))**2
     &                                     + (COORD(9)-COORD(6))**2 )
        AR(3)=SQRT((COORD(7)-COORD(16))**2 + (COORD(8)-COORD(17))**2
     &                                     + (COORD(9)-COORD(18))**2 )
        L=(AR(1)*AR(2)*AR(3))**(1.D0/3.D0)

      ELSEIF (TYPMA(1:5).EQ.'PYRAM') THEN

C       M : MILIEU DE LA FACE QUADRANGLE
        DO 5 I=1,3
          M(I) = (   COORD(3*(1-1)+I) + COORD(3*(2-1)+I)
     &             + COORD(3*(3-1)+I) + COORD(3*(4-1)+I)  ) / 4.D0
 5      CONTINUE

C       LA LONGUEUR CARACTÉRISTIQUE EST ((N1-N3)*(M-N5))^(1/2)
        AR(1)=SQRT(   (COORD(3*(3-1)+1)-COORD(3*(1-1)+1))**2
     &              + (COORD(3*(3-1)+2)-COORD(3*(1-1)+2))**2
     &              + (COORD(3*(3-1)+3)-COORD(3*(1-1)+3))**2 )
        AR(2)=SQRT(   (            M(1)-COORD(3*(5-1)+1))**2
     &              + (            M(2)-COORD(3*(5-1)+2))**2
     &              + (            M(3)-COORD(3*(5-1)+3))**2 )
        L=SQRT(AR(1)*AR(2))

      ELSEIF (TYPMA(1:5).EQ.'TETRA') THEN

C       LA LONGUEUR CARACTÉRISTIQUE EST ((N1-N2)*(N1-N3)*(N1-N4))^(1/3)
        DO 10 I=1,3
          AR(I)=SQRT((COORD(1)-COORD(3*I+1))**2 +
     &        (COORD(2)-COORD(3*I+2))**2 + (COORD(3)-COORD(3*I+3))**2 )
 10     CONTINUE
        L=(AR(1)*AR(2)*AR(3))**(1.D0/3.D0)

      ELSEIF (TYPMA(1:2).EQ.'QU') THEN

C     LA LONGUEUR CARACTÉRISTIQUE EST ((N1-N2)*(N1-N3))^(1/2)
        DO 20 I=1,2
          AR(I) = (COORD(1)-COORD(NDIM*I+1))**2 +
     &            (COORD(2)-COORD(NDIM*I+2))**2
          IF (NDIM.EQ.3) AR(I) = AR(I) + (COORD(3)-COORD(NDIM*I+3))**2
 20     CONTINUE
        L=(SQRT(AR(1)*AR(2)))**(1.D0/2.D0)

      ELSEIF (TYPMA(1:2).EQ.'TR') THEN

C     LA LONGUEUR CARACTÉRISTIQUE EST ((N1-N2)*(N1-N3))^(1/2)
        DO 30 I=1,2
          AR(I) = (COORD(1)-COORD(NDIM*I+1))**2 +
     &            (COORD(2)-COORD(NDIM*I+2))**2
          IF (NDIM.EQ.3) AR(I) = AR(I) + (COORD(3)-COORD(NDIM*I+3))**2
 30     CONTINUE
        L=(SQRT(AR(1)*AR(2)))**(1.D0/2.D0)

      ELSEIF (TYPMA(1:2).EQ.'SE') THEN

C       LA LONGUEUR CARACTÉRISTIQUE EST (N1-N2)^(1/2)
        L = (COORD(1)-COORD(NDIM+1))**2 + (COORD(2)-COORD(NDIM+2))**2
        IF (NDIM.EQ.3) L = L + (COORD(3)-COORD(NDIM+3))**2
        L = SQRT(L)

      ELSE

C       TYPE D'ELEMENT FINI PAS TRAITE
        CALL ASSERT(.FALSE.)

      ENDIF

      CALL JEDEMA()
      END
