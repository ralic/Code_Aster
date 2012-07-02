      SUBROUTINE TRLDS( A , NMAX, NORDRE , IERR )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
CA
CA    ARGUMENTS :
CA    ---------
CA <> A      : MATRICE CARREE
CA -> NMAX   : DIMENSION REELLE DE LA MATRICE A
CA -> NORDRE : DIMENSION DE LA MATRICE A
CA <- IERR   : NUMERO DE LA LIGNE POUR LAQUELLE UN PIVOT NUL EST OBTENU
CA             0 SI PAS DE PIVOT NUL
CA
CA    BUT :
CA    ---
CA    TRIANGULATION EN PLACE DE LA MATRICE CARREE A
      REAL*8              A(NMAX,NORDRE) , R8VAL
C
C-----------------------------------------------------------------------
      INTEGER I ,IBM ,IERR ,IFM ,IN ,IUNIFI ,JN 
      INTEGER NMAX ,NORDRE 
C-----------------------------------------------------------------------
      IERR   = 0
      DO 100 IN = 1 , NORDRE
         IF ( IN .EQ. 1 ) GO TO 50
C
C        UTILISATION  DES  LIGNES  (1) A (IN-1)
         DO 40 JN = 1 , IN-1
C
            IF ( JN .EQ. 1 ) GO TO 36
            IBM   = JN - 1
C
            R8VAL = A ( JN , IN )
            DO 30 I = 1 , IBM
               R8VAL = R8VAL - A ( JN , I ) * A ( I , IN ) * A(I,I)
 30         CONTINUE
            A ( JN , IN ) = R8VAL
C
            R8VAL = A ( IN , JN )
            DO 35 I = 1 , IBM
               R8VAL = R8VAL - A ( IN , I ) * A ( I , JN ) * A(I,I)
 35         CONTINUE
            A ( IN , JN ) = R8VAL
C
 36         CONTINUE
            A ( JN , IN ) = A ( JN , IN ) / A(JN,JN)
            A ( IN , JN ) = A ( IN , JN ) / A(JN,JN)
 40      CONTINUE
C
 50      CONTINUE
C
C        UTILISATION  DE LA LIGNE IN ( CALCUL DU TERME PIVOT)
         IBM    = IN - 1
C
         R8VAL = A ( IN , IN )
         DO 85 I = 1 , IBM
            R8VAL = R8VAL - A ( IN , I ) * A ( I , IN ) * A(I,I)
 85      CONTINUE
         A ( IN , IN ) = R8VAL
         IF ( R8VAL .EQ. 0.D00 ) THEN
           IERR   = IN
           IFM=IUNIFI('MESSAGE')
           WRITE(IFM,*) ' TRLDS : PIVOT NUL A LA LIGNE ',IN
           GOTO 9999
         ENDIF
C
 100  CONTINUE
C
 9999 CONTINUE
      END
