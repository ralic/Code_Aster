      SUBROUTINE NMFISA(GEOM,B)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2002   AUTEUR LAVERNE J.LAVERNE 
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
      REAL*8  GEOM(2,4),B(2,8)
     
C***********************************************************************
C
C BUT:
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
C IN  : GEOM
C OUT : B
C I/O :
C   
C*********************************************************************



      REAL*8  C, S

      C = GEOM(1,2) - GEOM(1,1)
      S = GEOM(2,2) - GEOM(2,1)
            
      C = C / SQRT(C*C+S*S)
      S = S / SQRT(C*C+S*S)
      
      
C SAISIE DE LA MATRICE B : APPLICATION LINEAIRE DONNANT LE SAUT DE 
C DEPLACEMENT DANS L'ELEMENT (SU_N,SU_T) A PARTIR DES DEPLACEMENTS
C AUX NOEUDS :

      B(1,1) =  S/2
      B(1,2) = -C/2
      B(1,3) =  S/2
      B(1,4) = -C/2
      B(1,5) = -S/2
      B(1,6) =  C/2
      B(1,7) = -S/2
      B(1,8) =  C/2
      
      B(2,1) = -C/2
      B(2,2) = -S/2
      B(2,3) = -C/2
      B(2,4) = -S/2
      B(2,5) =  C/2
      B(2,6) =  S/2
      B(2,7) =  C/2
      B(2,8) =  S/2
      
      END
