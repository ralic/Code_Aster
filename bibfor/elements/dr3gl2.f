      SUBROUTINE DR3GL2(P,AG,AL)
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
C     ------------------------------------------------------------------
C          CHANGEMENT DE REPERE 3D  LOCAL => GLOBAL        H. BUNG 06-97
C     ------------------------------------------------------------------
C          P     : MATRICE DE PASSAGE  LOCAL => GENERAL
C                  DETERMINEE PAR LA ROUTINE DR3P
C          AG    : MATRICE DANS LE REPERE GENERAL
C          AL    : MATRICE DANS LE REPERE LOCAL
      IMPLICIT NONE
C---   VARIABLES GLOBALES
      REAL *8 P(3,3),AG(3,3),AL(3,3)
      INTEGER L
      REAL *8 SS

C---     ON CALCULE   (AG) = (P)  * (AL) * (P)T
C      CALL ZDANUL(AG,9)
      CALL R8INIR(9,0.D0,AG,1)
      DO 10 L=1,3

C--- M = 1 ---
             SS=AL(1,1)*P(L,1)+AL(1,2)*P(L,2)+AL(1,3)*P(L,3)
C  - K=1,3 -
               AG(1,L)=AG(1,L)+P(1,1)*SS
               AG(2,L)=AG(2,L)+P(2,1)*SS
               AG(3,L)=AG(3,L)+P(3,1)*SS
C--- M = 2 ---
             SS=AL(2,1)*P(L,1)+AL(2,2)*P(L,2)+AL(2,3)*P(L,3)
C  - K=1,3 -
               AG(1,L)=AG(1,L)+P(1,2)*SS
               AG(2,L)=AG(2,L)+P(2,2)*SS
               AG(3,L)=AG(3,L)+P(3,2)*SS
C--- M = 3 ---
             SS=AL(3,1)*P(L,1)+AL(3,2)*P(L,2)+AL(3,3)*P(L,3)
C  - K=1,3 -
               AG(1,L)=AG(1,L)+P(1,3)*SS
               AG(2,L)=AG(2,L)+P(2,3)*SS
               AG(3,L)=AG(3,L)+P(3,3)*SS
10    CONTINUE
C
      END
