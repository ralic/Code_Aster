      SUBROUTINE DR3GLM(P,AG,AL,LOOP)
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
C          CHANGEMENT DE REPERE 3D  G => L              
C     ------------------------------------------------------------------
C          P     : MATRICE DE PASSAGE  LOCAL => GENERAL
C                  DETERMINEE PAR LA ROUTINE DR3P
C          AG    : MATRICE DANS LE REPERE GENERAL
C          AL    : MATRICE DANS LE REPERE LOCAL
C          LOOP  : LOOP=1 : G => L   ;   LOOP=2 : L => G
C        EXEMPLE :  (AG) = (P)  * (AL) * (P)T  POUR LOOP=2
C                   (AL) = (P)T * (AG) * (P)   POUR LOOP=1
      IMPLICIT NONE
      INTEGER LOOP
      REAL *8 P(3,3),AG(9),AL(9)
      IF(LOOP.EQ.1)THEN
C---     ON CALCULE   (AL) = (P)T * (AG) * (P)
         CALL DR3GL1(P,AG,AL)
      ENDIF
      IF(LOOP.EQ.2)THEN
C---     ON CALCULE   (AG) = (P)  * (AL) * (P)T
         CALL DR3GL2(P,AG,AL)
      ENDIF
      IF(LOOP.NE.1.AND.LOOP.NE.2)THEN
         CALL UTMESS('F','DR3GLM','PB1')
      ENDIF
      END
