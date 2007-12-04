      SUBROUTINE INVALM(D,LL,MM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
C-------------------------------------------------------
C         INVERSION D UNE MATRICE MM,MM CONTENUE DS UNE MATRICE LL,LL
C          CETTE MATRICE EST DS LE COIN SUPERIAUR GAUCHE

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 D(1)

      DO 50 N = 1,MM
        NN = LL* (N-1) + N
        DIAG = D(NN)
        DIAG = 1.D0/DIAG
        DO 10 J = 1,MM
          NJ = LL* (N-1) + J
          D(NJ) = -D(NJ)*DIAG
   10   CONTINUE
        DO 40 I = 1,MM
          IN = LL* (I-1) + N
          IF (I.EQ.N) GO TO 30
          DO 20 J = 1,MM
            IF (J.EQ.N) GO TO 20
            NJ = LL* (N-1) + J
            IJ = LL* (I-1) + J
            D(IJ) = D(IJ) + D(IN)*D(NJ)
   20     CONTINUE
   30     CONTINUE
          D(IN) = D(IN)*DIAG
   40   CONTINUE
        D(NN) = DIAG
   50 CONTINUE
      END
