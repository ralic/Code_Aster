      SUBROUTINE MMJACG(ALIAS ,JGEOM ,FF    ,DFF   ,LAXIS ,
     &                  NNE   ,NDIM  ,JACOBI)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/04/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  ALIAS
      INTEGER      JGEOM
      REAL*8       FF(9) ,DFF(2,9)
      LOGICAL      LAXIS
      INTEGER      NNE   ,NDIM
      REAL*8       JACOBI
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DU JACOBIEN D'UN ELEMENT
C
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : NOM DE L'ELEMENT
C IN  JGEOM  : ADRESSE DES COORDONNEES INITIALES DES NOEUDS DE L'ELEMENT
C IN  FF     : FONCTIONS DE FORMES EN XI,YI
C IN  DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME EN XI YI
C IN  LAXIS  : SI PROBLEME AXISYMETRIQUE
C IN  NNE    : NOMBRE DE NOEUDS ESCLAVES
C IN  NDIM   : DIMENSION DU PROBLEME
C OUT JACOBI : VALEUR DU JACOBIEN
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER INOE  ,IDIM
      REAL*8  GEOMAC(9,3)
      REAL*8  XX
      REAL*8  DXDS,DYDS,DZDS
      REAL*8  DXDE,DXDK,DYDE,DYDK,DZDE,DZDK
C
C ----------------------------------------------------------------------
C
      DXDS = 0.D0
      DYDS = 0.D0
      DZDS = 0.D0
      DXDE = 0.D0
      DYDE = 0.D0
      DZDE = 0.D0
      DXDK = 0.D0
      DYDK = 0.D0
      DZDK = 0.D0
C
C --- COORDONNEES DES NOEUDS DANS LA CONFIGURATION DE REFERENCE
C
      DO 100 INOE = 1,NNE
        DO 110 IDIM = 1,NDIM
          GEOMAC(INOE,IDIM) = ZR(JGEOM+(INOE-1)*NDIM+IDIM-1)
 110    CONTINUE
 100  CONTINUE
C
      IF (ALIAS(1:5).EQ.'SE2') THEN
        DO 10 INOE = 1,2
          IF(NDIM.EQ.2) THEN
            DXDS = DXDS + GEOMAC(INOE,1)*DFF(1,INOE)
            DYDS = DYDS + GEOMAC(INOE,2)*DFF(1,INOE)
          ELSEIF(NDIM.EQ.3) THEN
            DXDS = DXDS + GEOMAC(INOE,1)*DFF(1,INOE)
            DYDS = DYDS + GEOMAC(INOE,2)*DFF(1,INOE)
            DZDS = DZDS + GEOMAC(INOE,3)*DFF(1,INOE)
          ENDIF
   10   CONTINUE
        IF (LAXIS) THEN
          XX = 0.D0
          DO 7 INOE=1,2
            XX = XX+ GEOMAC(INOE,1)*FF(INOE)
7         CONTINUE
          IF (XX.EQ.0.D0) THEN
            XX=0.01D-5
            CALL U2MESS('A','CONTACT2_14')
          ENDIF
          JACOBI = SQRT(DXDS**2+DYDS**2)*ABS(XX)
        ELSE
          JACOBI = SQRT(DXDS**2+DYDS**2+DZDS**2)
        END IF
      ELSE IF (ALIAS(1:5).EQ.'SE3') THEN
        DO 20 INOE = 1,3
          DXDS = DXDS + GEOMAC(INOE,1)*DFF(1,INOE)
          DYDS = DYDS + GEOMAC(INOE,2)*DFF(1,INOE)
   20   CONTINUE
         IF (LAXIS) THEN
           XX = 0.D0
           DO 3 INOE=1,3
             XX = XX+ GEOMAC(INOE,1)*FF(INOE)
3          CONTINUE
           IF (XX.EQ.0.D0) THEN
             XX=0.01D-5
             CALL U2MESS('A','CONTACT2_14')
           ENDIF
           JACOBI = SQRT(DXDS**2+DYDS**2)*ABS(XX)
         ELSE
           JACOBI = SQRT(DXDS**2+DYDS**2)
         END IF
      ELSE IF (ALIAS(1:5).EQ.'TR3') THEN
        DO 30 INOE = 1,3
          DXDE = DXDE + GEOMAC(INOE,1)*DFF(1,INOE)
          DXDK = DXDK + GEOMAC(INOE,1)*DFF(2,INOE)
          DYDE = DYDE + GEOMAC(INOE,2)*DFF(1,INOE)
          DYDK = DYDK + GEOMAC(INOE,2)*DFF(2,INOE)
          DZDE = DZDE + GEOMAC(INOE,3)*DFF(1,INOE)
          DZDK = DZDK + GEOMAC(INOE,3)*DFF(2,INOE)
   30   CONTINUE
        JACOBI = SQRT((DYDE*DZDK-DZDE*DYDK)**2+
     &                (DZDE*DXDK-DXDE*DZDK)**2+
     &                (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE IF (ALIAS.EQ.'TR6') THEN
        DO 40 INOE = 1,6
          DXDE = DXDE + GEOMAC(INOE,1)*DFF(1,INOE)
          DXDK = DXDK + GEOMAC(INOE,1)*DFF(2,INOE)
          DYDE = DYDE + GEOMAC(INOE,2)*DFF(1,INOE)
          DYDK = DYDK + GEOMAC(INOE,2)*DFF(2,INOE)
          DZDE = DZDE + GEOMAC(INOE,3)*DFF(1,INOE)
          DZDK = DZDK + GEOMAC(INOE,3)*DFF(2,INOE)
   40   CONTINUE
        JACOBI = SQRT((DYDE*DZDK-DZDE*DYDK)**2+
     &                (DZDE*DXDK-DXDE*DZDK)**2+
     &                (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE IF (ALIAS(1:5).EQ.'QU4') THEN
        DO 50 INOE = 1,4
          DXDE = DXDE + GEOMAC(INOE,1)*DFF(1,INOE)
          DXDK = DXDK + GEOMAC(INOE,1)*DFF(2,INOE)
          DYDE = DYDE + GEOMAC(INOE,2)*DFF(1,INOE)
          DYDK = DYDK + GEOMAC(INOE,2)*DFF(2,INOE)
          DZDE = DZDE + GEOMAC(INOE,3)*DFF(1,INOE)
          DZDK = DZDK + GEOMAC(INOE,3)*DFF(2,INOE)
   50   CONTINUE
        JACOBI = SQRT((DYDE*DZDK-DZDE*DYDK)**2+
     &                (DZDE*DXDK-DXDE*DZDK)**2+
     &                (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE IF (ALIAS.EQ.'QU8') THEN
        DO 60 INOE = 1,8
          DXDE = DXDE + GEOMAC(INOE,1)*DFF(1,INOE)
          DXDK = DXDK + GEOMAC(INOE,1)*DFF(2,INOE)
          DYDE = DYDE + GEOMAC(INOE,2)*DFF(1,INOE)
          DYDK = DYDK + GEOMAC(INOE,2)*DFF(2,INOE)
          DZDE = DZDE + GEOMAC(INOE,3)*DFF(1,INOE)
          DZDK = DZDK + GEOMAC(INOE,3)*DFF(2,INOE)
   60   CONTINUE
        JACOBI = SQRT((DYDE*DZDK-DZDE*DYDK)**2+
     &                (DZDE*DXDK-DXDE*DZDK)**2+
     &                (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE IF (ALIAS.EQ.'QU9') THEN
        DO 70 INOE = 1,9
          DXDE = DXDE + GEOMAC(INOE,1)*DFF(1,INOE)
          DXDK = DXDK + GEOMAC(INOE,1)*DFF(2,INOE)
          DYDE = DYDE + GEOMAC(INOE,2)*DFF(1,INOE)
          DYDK = DYDK + GEOMAC(INOE,2)*DFF(2,INOE)
          DZDE = DZDE + GEOMAC(INOE,3)*DFF(1,INOE)
          DZDK = DZDK + GEOMAC(INOE,3)*DFF(2,INOE)
   70   CONTINUE
        JACOBI = SQRT((DYDE*DZDK-DZDE*DYDK)**2+
     &                (DZDE*DXDK-DXDE*DZDK)**2+
     &                (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      END
