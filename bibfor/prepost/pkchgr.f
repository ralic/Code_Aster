      SUBROUTINE PKCHGR ( VO, VE, VECTY, NBVAL, DXS, DYS, DZS, DXI,
     +                    DYI, DZI, ABSC, SYMECH )
      IMPLICIT   NONE
      INTEGER             NBVAL      
      CHARACTER*8        SYMECH
      REAL*8              VO(3), VE(3), VECTY(3), DXS(*), DYS(*), 
     +                    DZS(*), DXI(*), DYI(*), DZI(*), ABSC(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 01/02/2005   AUTEUR GALENNE E.GALENNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
C     OPERATEUR POST_K1_K2_K3: CHANGEMENT DE REPERE
C     ------------------------------------------------------------------
C
      INTEGER      I, J, K, IFM, NIV
      REAL*8       V1X, V1Y, V1Z, V2X, V2Y, V2Z, V3X, V3Y, V3Z, 
     +             V1N, V1P, V2N, NEWS(3), NEWI(3), OLDS(3), OLDI(3),
     +             PGL(3,3)
C
C DEB ------------------------------------------------------------------
C
      CALL INFNIV ( IFM , NIV )
      IF ( NIV .EQ. 2 )  WRITE(IFM,1038)
C
      V2X = VE(1) - VO(1)
      V2Y = VE(2) - VO(2)
      V2Z = VE(3) - VO(3)
      V2N = SQRT( V2X**2 + V2Y**2 + V2Z**2 )
      V2X = V2X / V2N
      V2Y = V2Y / V2N
      V2Z = V2Z / V2N
C
      V1X = VECTY(1)
      V1Y = VECTY(2)
      V1Z = VECTY(3)
C
C     --- PROJECTION / NORMALISATION
      V1P = V2X*V1X + V2Y*V1Y + V2Z*V1Z
      V1X = V1X - V1P*V2X
      V1Y = V1Y - V1P*V2Y
      V1Z = V1Z - V1P*V2Z
      V1N = SQRT( V1X**2 + V1Y**2 + V1Z**2 )
      V1X = V1X / V1N
      V1Y = V1Y / V1N
      V1Z = V1Z / V1N
C
C     --- 3IEME VECTEUR
      V3X = V1Y*V2Z - V2Y*V1Z
      V3Y = V1Z*V2X - V2Z*V1X
      V3Z = V1X*V2Y - V2X*V1Y
C
C     --- MATRICE DE PASSAGE
C
C     1 : VECTEUR NORMAL AU PLAN DE LA FISSURE
C         ORIENTE LEVRE INFERIEURE VERS LEVRE SUPERIEURE
C     2 : VECTEUR NORMAL AU FOND DE FISSURE EN M
C     3 : VECTEUR TANGENT AU FOND DE FISSURE EN M
C
      PGL(1,1) = V1X
      PGL(1,2) = V1Y
      PGL(1,3) = V1Z
      PGL(2,1) = V2X
      PGL(2,2) = V2Y
      PGL(2,3) = V2Z
      PGL(3,1) = V3X
      PGL(3,2) = V3Y
      PGL(3,3) = V3Z
C
      DO 20 I = 1 , NBVAL
         OLDS(1) = DXS(I)
         OLDS(2) = DYS(I)
         OLDS(3) = DZS(I)
         NEWS(1) = 0.D0
         NEWS(2) = 0.D0
         NEWS(3) = 0.D0
         IF (SYMECH .EQ. 'SANS' ) THEN
           OLDI(1) = DXI(I)
           OLDI(2) = DYI(I)
           OLDI(3) = DZI(I)
           NEWI(1) = 0.D0
           NEWI(2) = 0.D0
           NEWI(3) = 0.D0
         ENDIF
         DO 22 J = 1 , 3
            DO 24 K = 1 , 3
               NEWS(K) = NEWS(K) + PGL(K,J) * OLDS(J)
               IF (SYMECH .EQ. 'SANS' ) THEN
                 NEWI(K) = NEWI(K) + PGL(K,J) * OLDI(J)
               ENDIF
 24         CONTINUE
 22      CONTINUE
         DXS(I) = NEWS(1)
         DYS(I) = NEWS(2)
         DZS(I) = NEWS(3)
         IF (SYMECH .EQ. 'SANS' ) THEN
           DXI(I) = NEWI(1)
           DYI(I) = NEWI(2)
           DZI(I) = NEWI(3)
         ELSE
           DXI(I) = -1*NEWS(1)
           DYI(I) = NEWS(2)
           DZI(I) = NEWS(3)
         ENDIF
         IF ( NIV .EQ. 2 ) THEN
            WRITE(IFM,1040) ABSC(I), OLDS(1), OLDS(2), OLDS(3)
            WRITE(IFM,1042) NEWS(1), NEWS(2), NEWS(3)
            IF (SYMECH .EQ. 'SANS' ) THEN
              WRITE(IFM,1044) OLDI(1), OLDI(2), OLDI(3)
              WRITE(IFM,1042) NEWI(1), NEWI(2), NEWI(3)
            ENDIF
         ENDIF
 20   CONTINUE
C
 1038 FORMAT(23X,'ABSC_CURV        DX            DY            DZ')
 1040 FORMAT(1P,' DEPL_SUP GLOBAL : ',4(2X,E12.5))
 1042 FORMAT(1P,'           LOCAL : ',14X,3(2X,E12.5))
 1044 FORMAT(1P,' DEPL_INF GLOBAL : ',14X,3(2X,E12.5))
C
      END
