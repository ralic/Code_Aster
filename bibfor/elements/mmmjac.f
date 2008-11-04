      SUBROUTINE MMMJAC(ALIAS ,IGEOM ,FF    ,DFF   ,IAXIS ,
     &                  NDIM  ,JAC   )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/11/2008   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER      IGEOM
      REAL*8       FF(9)
      REAL*8       DFF(2,9)      
      INTEGER      NDIM      
      INTEGER      IAXIS
      REAL*8       JAC
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
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  IGEOM  : ADRESSE JEVEUX POUR LE VECTEUR GEOMETRIE ACTUALISEE
C IN  FF     : FONCTIONS DE FORMES EN XI,YI
C IN  DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME EN XI YI
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  IAXIS  : VAUT 1 SI PROBLEME AXISYMETRIQUE
C OUT JAC    : VALEUR DU JACOBIEN
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
      INTEGER I
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
      IF (ALIAS(1:5).EQ.'SE2') THEN
        DO 10 I = 1,2
          IF(NDIM.EQ.2) THEN
            DXDS = DXDS + ZR(IGEOM-1+2*(I-1)+1)*DFF(1,I)
            DYDS = DYDS + ZR(IGEOM-1+2*(I-1)+2)*DFF(1,I)
          ELSEIF(NDIM.EQ.3) THEN
            DXDS = DXDS + ZR(IGEOM-1+3*(I-1)+1)*DFF(1,I)
            DYDS = DYDS + ZR(IGEOM-1+3*(I-1)+2)*DFF(1,I)
            DZDS = DZDS + ZR(IGEOM-1+3*(I-1)+3)*DFF(1,I)
          ENDIF
   10   CONTINUE
        IF (IAXIS.EQ.1) THEN
          XX = 0.D0
          DO 7 I=1,2
            XX = XX+ ZR(IGEOM-1+2*(I-1)+1)*FF(I)
7         CONTINUE
          IF (XX.EQ.0.D0) THEN
            XX=0.01D-5
          ENDIF  
          JAC = SQRT(DXDS**2+DYDS**2)*ABS(XX)
        ELSE
          JAC = SQRT(DXDS**2+DYDS**2+DZDS**2)
        END IF
      ELSE IF (ALIAS(1:5).EQ.'SE3') THEN
        DO 20 I = 1,3
          DXDS = DXDS + ZR(IGEOM-1+2*(I-1)+1)*DFF(1,I)
          DYDS = DYDS + ZR(IGEOM-1+2*(I-1)+2)*DFF(1,I)
   20   CONTINUE
         IF (IAXIS.EQ.1) THEN
           XX = 0.D0
           DO 3 I=1,3
             XX = XX+ ZR(IGEOM-1+2*(I-1)+1)*FF(I)
3          CONTINUE
           IF (XX.EQ.0.D0) THEN
             XX=0.01D-5
           ENDIF
           JAC = SQRT(DXDS**2+DYDS**2)*ABS(XX)
         ELSE
           JAC = SQRT(DXDS**2+DYDS**2)
         END IF
      ELSE IF (ALIAS(1:5).EQ.'TR3') THEN
        DO 30 I = 1,3
          DXDE = DXDE + ZR(IGEOM+3*I-3)*DFF(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*DFF(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*DFF(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*DFF(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*DFF(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*DFF(2,I)
   30   CONTINUE
        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE IF (ALIAS.EQ.'TR6') THEN       
        DO 40 I = 1,6
          DXDE = DXDE + ZR(IGEOM+3*I-3)*DFF(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*DFF(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*DFF(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*DFF(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*DFF(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*DFF(2,I)
   40   CONTINUE
        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE IF (ALIAS(1:5).EQ.'QU4') THEN
        DO 50 I = 1,4
          DXDE = DXDE + ZR(IGEOM+3*I-3)*DFF(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*DFF(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*DFF(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*DFF(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*DFF(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*DFF(2,I)
   50   CONTINUE
        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE IF (ALIAS.EQ.'QU8') THEN
        DO 60 I = 1,8
          DXDE = DXDE + ZR(IGEOM+3*I-3)*DFF(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*DFF(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*DFF(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*DFF(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*DFF(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*DFF(2,I)
   60   CONTINUE
        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE IF (ALIAS.EQ.'QU9') THEN
        DO 70 I = 1,9
          DXDE = DXDE + ZR(IGEOM+3*I-3)*DFF(1,I)
          DXDK = DXDK + ZR(IGEOM+3*I-3)*DFF(2,I)
          DYDE = DYDE + ZR(IGEOM+3*I-2)*DFF(1,I)
          DYDK = DYDK + ZR(IGEOM+3*I-2)*DFF(2,I)
          DZDE = DZDE + ZR(IGEOM+3*I-1)*DFF(1,I)
          DZDK = DZDK + ZR(IGEOM+3*I-1)*DFF(2,I)
   70   CONTINUE
        JAC = SQRT((DYDE*DZDK-DZDE*DYDK)**2+ (DZDE*DXDK-DXDE*DZDK)**2+
     &        (DXDE*DYDK-DYDE*DXDK)**2)
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      END
