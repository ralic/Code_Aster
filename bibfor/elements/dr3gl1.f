      SUBROUTINE DR3GL1(P,AG,AL)
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
C          CHANGEMENT DE REPERE 3D  GLOBAL => LOCAL        H. BUNG 06-97
C     ------------------------------------------------------------------
C          P     : MATRICE DE PASSAGE  LOCAL => GENERAL
C                  DETERMINEE PAR LA ROUTINE DR3P
C          AG    : MATRICE DANS LE REPERE GENERAL
C          AL    : MATRICE DANS LE REPERE LOCAL
      IMPLICIT NONE
C---   VARIABLES GLOBALES
      REAL *8 P(3,3),AG(3,3),AL(3,3)
C---   VARIABLES LOCALES
      INTEGER L
      REAL *8 SS

C---     ON CALCULE   (AL) = (P)T * (AG) * (P)
C      CALL ZDANUL(AL,9)
      CALL R8INIR(9,0.D0,AL,1)
      DO 10 L=1,3
C---  M = 1 ---
             SS=AG(1,1)*P(1,L)+AG(1,2)*P(2,L)+AG(1,3)*P(3,L)
C   - K=1,3 -
               AL(1,L)=AL(1,L)+P(1,1)*SS
               AL(2,L)=AL(2,L)+P(1,2)*SS
               AL(3,L)=AL(3,L)+P(1,3)*SS
C---  M = 2 ---
             SS=AG(2,1)*P(1,L)+AG(2,2)*P(2,L)+AG(2,3)*P(3,L)
C   - K=1,3 -
               AL(1,L)=AL(1,L)+P(2,1)*SS
               AL(2,L)=AL(2,L)+P(2,2)*SS
               AL(3,L)=AL(3,L)+P(2,3)*SS
C---  M = 3 ---
             SS=AG(3,1)*P(1,L)+AG(3,2)*P(2,L)+AG(3,3)*P(3,L)
C   - K=1,3 -
               AL(1,L)=AL(1,L)+P(3,1)*SS
               AL(2,L)=AL(2,L)+P(3,2)*SS
               AL(3,L)=AL(3,L)+P(3,3)*SS
10    CONTINUE
C
      END
