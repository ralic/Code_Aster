      SUBROUTINE RRLDC(A,NORDRE,X,NVES)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C***********************************************************************
C    A. COMTE                                 DATE 31/07/91
C-----------------------------------------------------------------------
C  BUT:  RESOLUTION DE L'EQUATION MATRICIELLE
      IMPLICIT NONE
C                 A*X=B
C
C  OU A EST UNE MATRICE COMPLEXE FACTORISEE LDLT PAR TRLDC
C
C-----------------------------------------------------------------------
C
C A        /I/: MATRICE CARRE COMPLEXE TRIANGULEE LDLT
C NORDRE   /I/: DIMENSION DE LA MATRICE A
C X        /M/: MATRICE IN:SECONDS MEMBRES   OUT:SOLUTIONS
C NVEC     /I/: NOMBRE DE VECTEURS SECOND MEMBRE
C
C-----------------------------------------------------------------------
C
      COMPLEX * 16  A(*),X(NORDRE,NVES),R8VAL
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IDIAG ,ILIGN1 ,ILIGN2 ,IN ,INDIAG ,NORDRE 
      INTEGER NV ,NVES 
C-----------------------------------------------------------------------
      ILIGN1 = 1
      ILIGN2 = NORDRE
C
C     RESOLUTION DESCENDANTE
      DO 25 NV = 1 , NVES
         DO 20 IN = ILIGN1 , ILIGN2-1
            R8VAL = - X (IN,NV)
            DO 21 I = IN+1 , ILIGN2
                  IDIAG=I*(I-1)/2+1
                  X(I,NV) = X(I,NV) + R8VAL*DCONJG(A(IDIAG+I-IN))
 21         CONTINUE
 20      CONTINUE
 25   CONTINUE
C
C     RESOLUTION DIAGONALE
      DO 39 NV = 1 , NVES
         DO 33 IN = ILIGN1 , ILIGN2
            INDIAG = IN*(IN-1)/2+1
            X ( IN , NV ) = X ( IN , NV ) / A(INDIAG)
 33      CONTINUE
 39   CONTINUE
C
C     RESOLUTION REMONTANTE
      DO 45 NV = 1 , NVES
         DO 40 IN = ILIGN2 , ILIGN1+1 , -1
            INDIAG = IN*(IN-1)/2+1
            R8VAL = - X ( IN , NV )
            DO 41 I = 1 , IN-1
               X(I,NV) = X(I,NV) + R8VAL*A(INDIAG+IN-I)
 41         CONTINUE
 40      CONTINUE
 45   CONTINUE
C
      END
