      SUBROUTINE NMFISA(AXI,GEOM,KPG,POIDS,B)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/05/2010   AUTEUR LAVERNE J.LAVERNE 
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
      LOGICAL AXI
      INTEGER KPG
      REAL*8  GEOM(2,4),POIDS,B(2,8)
           
C-----------------------------------------------------------------------
C
C BUT:
C     POUR LE POINT DE GAUSS KPG :
C     CALCUL DU POIDS DU POINT DE GAUSS
C     CALCUL DE LA MATRICE B DONNANT LES SAUT PAR ELEMENTS A PARTIR DES 
C     DEPLACEMENTS AUX NOEUDS : SU = B U
C
C REMARQUE : 
C
C   LA MATRICE B INCLUE LE CHANGEMENT DE REPERE LOCAL/GLOBAL :
C     U    = DEPLACEMENT DANS LE REPERE GLOBAL
C     ULOC = DEPLACEMENT DANS LE REPERE LOCAL A L'ELEMENT
C
C   B S'ECRIT SOUS LA FORME : B = BTILD RTILD
C   AVEC :
C            SU   = BTILD ULOC  
C            ULOC = RTILD U  
C
C
C IN  : AXI TRUE EN AXI
C IN  : GEOM,KPG
C OUT : POIDS, B
C   
C-----------------------------------------------------------------------
      REAL*8  CO,SI, C, S,COEF(2), AIRE, RAYON
C-----------------------------------------------------------------------

      COEF(1) = 0.5D0*(1.D0 + SQRT(3.D0)/3.D0)
      COEF(2) = 0.5D0*(1.D0 - SQRT(3.D0)/3.D0)

C -- CALCUL DU POIDS DU POINT DE GAUSS

      AIRE = SQRT( (GEOM(1,2)-GEOM(1,1))**2 + (GEOM(2,2)-GEOM(2,1))**2 )
      
C     DISTANCE DU PG A L'AXE DE REVOLUTION (I.E ABSCISSE DU PG)
      RAYON = GEOM(1,1)*COEF(KPG) + GEOM(1,2)*(1.D0-COEF(KPG))
      IF (AXI) AIRE = AIRE*RAYON
      
      POIDS = AIRE/2

C -- CALCUL DE LA MATRICE B

      CALL R8INIR(16, 0.D0, B ,1)
      
      CO =   (GEOM(2,2) - GEOM(2,1))  
      SI =  -(GEOM(1,2) - GEOM(1,1))

      C = CO / SQRT(CO*CO + SI*SI)
      S = SI / SQRT(CO*CO + SI*SI)
               
C SAISIE DE LA MATRICE B : APPLICATION LINEAIRE DONNANT LE SAUT DE 
C DEPLACEMENT DANS L'ELEMENT (SU_N,SU_T) A PARTIR DES DEPLACEMENTS
C AUX NOEUDS :      

      B(1,1) =   C*COEF(KPG) 
      B(1,2) =   S*COEF(KPG)
      B(1,3) =   C*(1.D0-COEF(KPG))
      B(1,4) =   S*(1.D0-COEF(KPG))
      B(1,5) =  -C*(1.D0-COEF(KPG))
      B(1,6) =  -S*(1.D0-COEF(KPG))
      B(1,7) =  -C*COEF(KPG)
      B(1,8) =  -S*COEF(KPG)

      B(2,1) =   -S*COEF(KPG)
      B(2,2) =    C*COEF(KPG)
      B(2,3) =   -S*(1.D0-COEF(KPG))
      B(2,4) =    C*(1.D0-COEF(KPG))
      B(2,5) =    S*(1.D0-COEF(KPG))
      B(2,6) =   -C*(1.D0-COEF(KPG))
      B(2,7) =    S*COEF(KPG)
      B(2,8) =   -C*COEF(KPG)
      
      END
