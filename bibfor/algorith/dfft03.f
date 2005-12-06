      SUBROUTINE DFFT03( NTHPO,CR0,CR1,CR2,CR3,CI0,CI1,CI2,CI3 )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/05/96   AUTEUR KXBADNG T.FRIOU 
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
C----------------------------------------------------------------------
C
C DESCRIPTION : ROUTINE D ITERATIONS EN BASE 4
C -----------
C
C ******************   DECLARATION DES VARIABLES   *********************
C
      IMPLICIT NONE
C
C ARGUMENTS
C ---------
      INTEGER NTHPO
      REAL*8 CR0(*), CR1(*), CR2(*), CR3(*)
      REAL*8 CI0(*), CI1(*), CI2(*), CI3(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER K
      REAL*8 R1, R2, R3, R4, FI1, FI2, FI3, FI4
C
C ******************   DEBUT DU CODE EXECUTABLE   **********************
C
      DO 10 K = 1, NTHPO, 4
        R1 = CR0(K) + CR2(K)
        R2 = CR0(K) - CR2(K)
        R3 = CR1(K) + CR3(K)
        R4 = CR1(K) - CR3(K)
        FI1 = CI0(K) + CI2(K)
        FI2 = CI0(K) - CI2(K)
        FI3 = CI1(K) + CI3(K)
        FI4 = CI1(K) - CI3(K)
        CR0(K) = R1 + R3
        CI0(K) = FI1 + FI3
        CR1(K) = R1 - R3
        CI1(K) = FI1 - FI3
        CR2(K) = R2 - FI4
        CI2(K) = FI2 + R4
        CR3(K) = R2 + FI4
        CI3(K) = FI2 - R4
  10  CONTINUE
C
      END
