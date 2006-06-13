      SUBROUTINE MINCLU(DIME,MA1,DIM1,MM1,SOM1,MA2,DIM2,PAN2,IR)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C A_UTIL
C ----------------------------------------------------------------------
C     TEST APPROCHE D'INCLUSION DE LA MAILLE MA1 DANS LA MAILLE MA2
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER  DIME     : DIMENSION DE L'ESPACE
C INTEGER  MA1      : MAILLE 1
C INTEGER  DIM1(*)  : SD BOITE.DIME ASSOCIEE A MA1 (CF BOITE)
C REAL*8   MM1(*)   : SD BOITE.MINMAX ASSOCIEE A MA1 (CF BOITE)
C REAL*8   SOM1(*)  : SD BOITE.SOMMET ASSOCIEE A MA1 (CF BOITE)
C INTEGER  MA2      : MAILLE 2
C INTEGER  DIM2(*)  : SD BOITE.DIME ASSOCIEE A MA2 (CF BOITE)
C REAL*8   PAN2(*)  : SD BOITE.PAN ASSOCIEE A MA2 (CF BOITE)
C
C VARIABLE DE SORTIE
C LOGICAL  IR       : .TRUE. SI LES BOITES ENGLOBANT MA1 SONT INCLUSES 
C                            DANS LE CONVEXE (ENGLOBANT/INSCRIT) DE MA2
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE (CF BOITE)
      REAL*8   PREC
      PARAMETER (PREC = 1.D-6)

C --- VARIABLES
      INTEGER  DIME,MA1,DIM1(*),MA2,DIM2(*)
      INTEGER  I,J,K,NSOM,NPAN,P1,P2,ID(3)
      REAL*8   MM1(2,DIME,*),SOM1(DIME,*)
      REAL*8   PAN2(DIME+2,*),R
      LOGICAL  IR

C --- INCLUSION CONVEXE EXTERIEUR

      P1 = DIM1(2+2*MA1)
      NSOM = DIM1(4+2*MA1) - P1
      P2 = DIM2(1+2*MA2)
      NPAN = DIM2(3+2*MA2) - P2

      DO 10 I = 1, NSOM       

        DO 10 J = 1, NPAN
    
          R = PAN2(DIME+2,P2-1+J)
 
          DO 20 K = 1, DIME
            R = R + SOM1(K,P1-1+I)*PAN2(K,P2-1+J)
 20       CONTINUE

          IF (R.GT.PREC) GOTO 30

 10   CONTINUE
          
      IR = .TRUE.
      GOTO 100

 30   CONTINUE

C --- INCLUSION MINMAX

      DO 40 I = 1, DIME
        ID(I) = 1
 40   CONTINUE

      DO 50 I = 1, 2**DIME
       
        DO 60 J = 1, NPAN        
  
          R = PAN2(DIME+2,P2-1+J)
        
          DO 70 K = 1, DIME
            R = R + MM1(ID(K),K,MA1)*PAN2(K,P2-1+J) 
 70       CONTINUE

          IF (R.GT.PREC) GOTO 90

 60     CONTINUE

        K = 1

 80     CONTINUE
        J = 3 - ID(K)
        ID(K) = J
        K = K + 1
        IF (J.EQ.1) GOTO 80

 50   CONTINUE

      IR = .TRUE.
      GOTO 100

C --- FIN

 90   CONTINUE

      IR = .FALSE.

 100  CONTINUE

      END
