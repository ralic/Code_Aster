      SUBROUTINE CFRESA(NDIM,FCTC,NORM,
     &                  RNX,RNY,RNZ,RN)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/02/2005   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
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
