      SUBROUTINE NMFIFI(NPG,TYPMOD,GEOM,SIGMA,FINT)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/03/2005   AUTEUR LAVERNE J.LAVERNE 
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

      IMPLICIT NONE
      INTEGER NPG 
      REAL*8 GEOM(2,4),SIGMA(2,NPG),FINT(8)
      CHARACTER*8 TYPMOD(2)
     
C-----------------------------------------------------------------------
C
C BUT: 
C      CALCUL DES FINT = ( B_T SIGMA) POUR L'OPTION FORC_NODA
C      SUBROUTINE APPELEE DANS LE TE0202
C
C IN  : GEOM,SIGMA,NPG,TYPMOD
C OUT : FINT
C-----------------------------------------------------------------------

      LOGICAL AXI
      INTEGER I,J,KPG
      REAL*8 B(2,8),POIDS   
C-----------------------------------------------------------------------


C    INITIALISATION
      AXI = TYPMOD(1).EQ.'AXIS'
      CALL R8INIR(8 , 0.D0, FINT,1)
      

C    BOUCLE SUR LES POINTS DE GAUSS

      DO 11 KPG=1,NPG

C      CALCUL DU POIDS ET DE LA MATRICE B 
        CALL NMFISA(AXI,GEOM,KPG,POIDS,B)
      
C      CALCUL DES FINT = ( B_T SIGMA ) :
        DO 20 I=1,8
          DO 40 J=1,2          
            FINT(I) = FINT(I) +  POIDS*B(J,I)*SIGMA(J,KPG)
 40       CONTINUE
 20     CONTINUE
 
 11   CONTINUE
      END
