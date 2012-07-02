         SUBROUTINE TRLDC(A,NORDRE,IERR)
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
      IMPLICIT NONE
C
C    A. COMTE                                 DATE 31/07/91
C-----------------------------------------------------------------------
C  BUT: FACTORISATION LDLT EN PLACE D'UNE MATRICE COMPLEXE
C  ET HERMITIENNE STOCKEE TRIANGULAIRE SUPERIEURE
C
C  CODE RETOUR  SI =0 RAS
C               SI NON NUL ALORS EST EGAL AU RANG DU PIVOT NUL TROUVE
C
C-----------------------------------------------------------------------
C
C A        /M/: MATRICE COMPLEXE A FACTORISER
C NORDRE   /I/: DIMENSION DE LA MATRICE
C IERR     /O/: CODE RETOUR
C
C-----------------------------------------------------------------------
C
      COMPLEX*16   A(*) , R8VAL
      REAL*8       EPSI,XMOD,XMAX,ZERO
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IBM ,IDIAG ,IERR ,IN ,INDIAG ,JN 
      INTEGER JNDIAG ,NORDRE 
      REAL*8 R8GAEM 
C-----------------------------------------------------------------------
      DATA ZERO /0.D+00/
C-----------------------------------------------------------------------
C
      EPSI=1.D0/R8GAEM()
      XMAX=ZERO
      IERR   = 0
      DO 100 IN = 1 , NORDRE
         INDIAG = IN*(IN-1)/2+1
         IF ( IN .EQ. 1 ) GO TO 50
C
C        UTILISATION  DES  LIGNES  (1) A (IN-1)
         DO 40 JN = 1 , IN-1
            JNDIAG = JN*(JN-1)/2+1
C
            IF ( JN .EQ. 1 ) GO TO 36
            IBM   = JN - 1
C
            R8VAL = A (INDIAG+IN-JN)
            DO 30 I = 1 , IBM
               IDIAG = I*(I-1)/2+1
               R8VAL = R8VAL -
     &              DCONJG(A(JNDIAG+JN-I))*A(INDIAG+IN-I)*A(IDIAG)
 30         CONTINUE
            A ( INDIAG+IN-JN ) = R8VAL
C
 36         CONTINUE
             A (INDIAG+IN-JN ) = A (INDIAG+IN-JN ) / A(JNDIAG)
 40      CONTINUE
C
 50      CONTINUE
C
C        UTILISATION  DE LA LIGNE IN ( CALCUL DU TERME PIVOT)
         IBM    = IN - 1
C
         R8VAL = A (INDIAG)
         DO 85 I = 1 , IBM
            IDIAG = I*(I-1)/2+1
            R8VAL = R8VAL -
     &              DCONJG(A(INDIAG+IN-I))*A(INDIAG+IN-I)*A(IDIAG)
 85      CONTINUE
         A (INDIAG) = R8VAL
         XMOD=DBLE(R8VAL)**2+DIMAG(R8VAL)**2
         IF(XMOD.GT.XMAX) XMAX=XMOD
         IF ( (XMOD/XMAX) .LT. EPSI ) THEN
           CALL U2MESS('I','ALGORITH10_98')
           IERR   = IN
           GOTO 9999
         ENDIF
C
 100  CONTINUE
C
 9999 CONTINUE
      END
