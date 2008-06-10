      SUBROUTINE NMGPIN(NDIM,NNO,AXI,VU)
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR MICHEL S.MICHEL 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      
      LOGICAL AXI
      INTEGER NDIM,NNO,VU(3,27)
C ----------------------------------------------------------------------
C        INITIALISATION POUR LES ELEMENTS EN GRANDES DEFORMATIONS
C ----------------------------------------------------------------------
C IN  NDIM  DIMENSION DE L'ESPACE
C IN  NNO   NOMBRE DE NOEUDS
C IN  AXI   INDICATEUR DE MODELISATION AXISYMETRIQUE
C OUT VU    RENVOIE L'INDICE DU DDL CORRESPONDANT A (I,N)
C ----------------------------------------------------------------------
      INTEGER N,I
C ----------------------------------------------------------------------

      DO 10 N = 1,NNO
        DO 20 I = 1,NDIM
          VU(I,N) = I + NDIM*(N-1)
 20     CONTINUE
 10   CONTINUE
      
      IF (AXI) THEN
        DO 30 N = 1,NNO
          VU(3,N) = VU(1,N)
 30     CONTINUE
      END IF
    
      END     
