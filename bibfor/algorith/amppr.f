      SUBROUTINE AMPPR(AMAT,NB1,NB2,BMAT,N1,N2,I,J)
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
      IMPLICIT NONE
C
C***********************************************************************
C    P. RICHARD     DATE 12/03/91
C-----------------------------------------------------------------------
C  BUT:  AJOUTER UNE MATRICE PLEINE REELLE A UNE MATRICEPLEINE REELLE
C            A UNE MATRICE PLEINE REELLE
C-----------------------------------------------------------------------
C
C AMAT     /M/: MATRICE RECEPTRICE
C NB1      /I/: NB DE LIGNES DE LA MATRICE RECEPTRICE
C NB2      /I/: NB DE COLONNES DE LA MATRICE RECEPTRICE
C BMAT     /M/: MATRICE PLEINE A AJOUTER
C N1       /I/: NB DE LIGNE DE LA MATRICE A AJOUTER
C N2       /I/: NB DE COLONNE DE LA MATRICE A AJOUTER
C I        /I/: INDICE DU PREMIER TERME DANS RECEPTRICE
C J        /I/: INDICE DE COLONNE TERME  DANS RECEPTRICE
C
C-----------------------------------------------------------------------
C
      REAL*8   AMAT(NB1,NB2),BMAT(N1,N2)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IDEB ,IFIN ,II ,IIDEB ,IIFIN ,J 
      INTEGER JDEB ,JFIN ,JJ ,JJDEB ,JJFIN ,N1 ,N2 
      INTEGER NB1 ,NB2 
C-----------------------------------------------------------------------
      JDEB=J
      JFIN=MIN(J+N2-1,NB2)
      IF(JFIN.LT.JDEB) GOTO 9999
      JJDEB=JDEB-J+1
      JJFIN=JFIN-J+1
C
      IDEB=I
      IFIN=MIN(I+N1-1,NB1)
      IF(IFIN.LT.IDEB) GOTO 9999
      IIDEB=IDEB-I+1
      IIFIN=IFIN-I+1
C
      DO 10 II=IIDEB,IIFIN
        DO 20 JJ=JJDEB,JJFIN
          AMAT(I+II-1,J+JJ-1)=AMAT(I+II-1,J+JJ-1)+BMAT(II,JJ)
 20     CONTINUE
 10   CONTINUE
C
 9999 CONTINUE
      END
