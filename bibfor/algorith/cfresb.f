      SUBROUTINE CFRESB(NDIM,LAG2D,TYPLIA,
     &                  FCTF,TANG,
     &                  RTX,RTY,RTZ)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/01/2006   AUTEUR MABBAS M.ABBAS 
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
      LOGICAL      LAG2D
      CHARACTER*2  TYPLIA
      REAL*8       FCTF(3)
      REAL*8       TANG(6)
      REAL*8       RTX
      REAL*8       RTY
      REAL*8       RTZ

C
C ======================================================================
C ROUTINE APPELEE PAR : CFRESU
C ======================================================================
C
C CALCUL DES FORCES TANGENTIELLES DE CONTACT/FROTTEMENT
C
C IN  NDIM   : DIMENSION DU MODELE
C IN  LAG2D  : VAUT .TRUE. SI LAGRANGIEN 2D
C IN  TYPLIA : TYPE DE LIAISON (F0/F1/F2/GL)
C                'F0': FROTTEMENT (2D) OU FROTTEMENT SUIVANT LES DEUX 
C                  DIRECTIONS SIMULTANEES (3D)
C                'F1': FROTTEMENT SUIVANT LA PREMIERE DIRECTION (3D)
C                'F2': FROTTEMENT SUIVANT LA SECONDE DIRECTION (3D)
C                'GL': POUR LE CALCUL DES FORCES DE GLISSEMENT
C IN  FCTF   : FORCES DE FROTTEMENT NODALES
C IN  TANG   : TANGENTES
C OUT RTX    : FORCE TANGENTIELLE PROJETEE SUR X
C OUT RTY    : FORCE TANGENTIELLE PROJETEE SUR Y
C OUT RTZ    : FORCE TANGENTIELLE PROJETEE SUR Z
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
      REAL*8 PROJ1,PROJ2
C
C ----------------------------------------------------------------------
C
      IF (LAG2D) THEN
          PROJ1   = FCTF(1) * TANG(1) + FCTF(2) * TANG(2)
          RTX = PROJ1 * TANG(1) 
          RTY = PROJ1 * TANG(2) 
        GOTO 100
      ENDIF

      IF ((TYPLIA.EQ.'F0').OR.(TYPLIA.EQ.'GL')) THEN
        PROJ1 = FCTF(1) * TANG(1) + FCTF(2) * TANG(2)
        PROJ2 = FCTF(1) * TANG(4) + FCTF(2) * TANG(5)
        IF (NDIM.EQ.3) THEN
          PROJ1 = PROJ1 + FCTF(3) * TANG(3)
          PROJ2 = PROJ2 + FCTF(3) * TANG(6)
        ENDIF
      ELSE IF (TYPLIA.EQ.'F1') THEN
        PROJ1 = FCTF(1) * TANG(1) + FCTF(2) * TANG(2)
        PROJ2 = 0.D0
        IF (NDIM.EQ.3) THEN
          PROJ1 = PROJ1 + FCTF(3) * TANG(3)
        ENDIF
      ELSE IF (TYPLIA.EQ.'F2') THEN
        PROJ1 = 0.D0
        PROJ2 = FCTF(1) * TANG(4) + FCTF(2) * TANG(5)
        IF (NDIM.EQ.3) THEN
          PROJ1 = 0.D0
          PROJ2 = PROJ2 + FCTF(3) * TANG(6)
        ENDIF
      ENDIF

      RTX = PROJ1 * TANG(1) + PROJ2 * TANG(4)
      RTY = PROJ1 * TANG(2) + PROJ2 * TANG(5)
      RTZ = 0.D0
      IF (NDIM.EQ.3) THEN
        RTZ = PROJ1 * TANG(3) + PROJ2 * TANG(6)
      ENDIF

100   CONTINUE

      END
