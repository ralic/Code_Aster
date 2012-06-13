      SUBROUTINE CFRESA(NDIM,FCTC,NORM,
     &                  RNX,RNY,RNZ,RN)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      INTEGER      NDIM
      REAL*8       FCTC(3)
      REAL*8       NORM(3)
      REAL*8       RNX
      REAL*8       RNY
      REAL*8       RNZ
      REAL*8       RN
C
C ======================================================================
C ROUTINE APPELEE PAR : CFRESU
C ======================================================================
C
C CALCUL DES REACTIONS NORMALES DE CONTACT
C
C IN  NDIM   : DIMENSION DU MODELE
C IN  FCTC   : FORCES DE CONTACT NODALES
C IN  NORM   : NORMALE
C OUT RNX    : FORCE DE REACTION NORMALE PROJETEE SUR X
C OUT RNY    : FORCE DE REACTION NORMALE PROJETEE SUR Y
C OUT RNZ    : FORCE DE REACTION NORMALE PROJETEE SUR Z
C OUT RN     : FORCE DE REACTION NORMALE RESULTANTE

C
C
C
C 
      REAL*8 PROJ
C
C ----------------------------------------------------------------------
C
      PROJ = FCTC(1) * NORM(1) + FCTC(2) * NORM(2)

      IF (NDIM.EQ.3) THEN
        PROJ = PROJ + FCTC(3) * NORM(3)
      ENDIF 

      RNX = PROJ * NORM(1)
      RNY = PROJ * NORM(2)
      RNZ = 0.D0

      IF (NDIM.EQ.3) THEN
        RNZ = PROJ * NORM(3)
      ENDIF 

      RN = SQRT(RNX**2+RNY**2+RNZ**2)

      END
