      SUBROUTINE XUREFA(XYZ,EPS)
      IMPLICIT NONE 

      REAL*8       XYZ(3),EPS(6)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2005   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
C          BUT : CALCUL DE L EPS(UREF) ANALYTIQUEMENT
C                    
C   IN        XYZ      : POINT DE CALCUL
C  OUT        EPS      : DEFORMATION DE UREF
C   
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C

      REAL*8    Y,Z,R,DRDY,DRDZ,T,DTDY,DTDZ,EPSTAB(3,3),E,R8DEPI,RAC
      REAL*8    DU1DR,DU1DT,DU1DY,DU1DZ,DU2DR,DU2DT,DU2DY,DU2DZ,RAC2
      PARAMETER (E=1.D5)
C ----------------------------------------------------------------------

      RAC2 = SQRT(2.D0)
  
      Y=XYZ(2)
      Z=XYZ(3)


      IF (0.EQ.1) THEN

      RAC=SQRT(R8DEPI())


      R=SQRT((Y-0.5D0)**2+(Z-0.5D0)**2)
      DRDY= 0.5D0 * (2*Y-1.D0) / R
      DRDZ= 0.5D0 * (2*Z-1.D0) / R

      T=ATAN2(Z-0.5D0,0.5D0-Y)
      DTDY=(Z-0.5D0) / (R*R)
      DTDZ=(0.5D0-Y) / (R*R)
      
      DU1DR=1.D0/(E*RAC*2.D0*SQRT(R)) * COS(T/2.D0)*(3.D0-COS(T))
      DU1DT=SQRT(R)/(E*RAC)*
     &      (COS(T/2.D0)*SIN(T)-SIN(T/2.D0)*(3.D0-COS(T))/2.D0)

      DU1DY=DU1DR*DRDY+DU1DT*DTDY
      DU1DZ=DU1DR*DRDZ+DU1DT*DTDZ      

      DU2DR=1.D0/(E*RAC*2.D0*SQRT(R)) * SIN(T/2.D0)*(3.D0-COS(T))
      DU2DT=SQRT(R)/(E*RAC)*
     &      (SIN(T/2.D0)*SIN(T)+COS(T/2.D0)*(3.D0-COS(T))/2.D0)

      DU2DY=DU2DR*DRDY+DU2DT*DTDY
      DU2DZ=DU2DR*DRDZ+DU2DT*DTDZ      

      EPSTAB(1,1)=0.D0
      EPSTAB(2,2)=-1.D0*DU1DY
      EPSTAB(2,1)=0.D0
      EPSTAB(3,3)=DU2DZ
      EPSTAB(3,1)=0.D0
      EPSTAB(3,2)=0.5D0 * ( DU2DY - DU1DZ )

      ENDIF

      IF (2.EQ.2) THEN

      EPSTAB(1,1)=0.D0
      EPSTAB(2,2)=0.D0
      EPSTAB(2,1)=0.D0
      EPSTAB(3,3)=-1.D-6
      EPSTAB(3,1)=0.D0
      EPSTAB(3,2)=0.D0

      ENDIF

      IF (0.EQ.3) THEN

      EPSTAB(1,1)=0.D0
      EPSTAB(2,2)=-4.D0/1.D5*Z*Z
      EPSTAB(2,1)=0.D0
      EPSTAB(3,3)=-4.D0*Y/1.D5*(1.D0-Y)
      EPSTAB(3,1)=0.D0
      EPSTAB(3,2)=0.D0

      ENDIF

      EPS(1) = EPSTAB(1,1)
      EPS(2) = EPSTAB(2,2)
      EPS(4) = EPSTAB(2,1)*RAC2
      EPS(3) = EPSTAB(3,3)
      EPS(5) = EPSTAB(3,1)*RAC2
      EPS(6) = EPSTAB(3,2)*RAC2




      END
