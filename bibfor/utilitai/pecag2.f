      SUBROUTINE PECAG2 (NDIM,NSYMX,NSYMY,NP,XYP,VALE,VALPAR )
      IMPLICIT NONE
      INTEGER  NDIM,NP
      REAL*8   VALE(*), VALPAR(*), XYP(2)
      LOGICAL  NSYMX, NSYMY
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 10/12/2001   AUTEUR JMBHH01 J.M.PROIX 
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
C     OPERATEUR   POST_ELEM
C     TRAITEMENT DU MOT CLE-FACTEUR "CARA_GEOM"
C     ------------------------------------------------------------------
C
      INTEGER  I, NPERM, NBVEC, NITJAC, ITYPE, IORDRE
      REAL*8   AR(6), BR(6), VECPRO(3,3), VALPRO(3), ANGL(3)
      REAL*8   EPSI, TOL, TOLDYN, JAC(3), DX, DY
      REAL*8   R8RDDG, V1(3), V2(3), V3(3)
      REAL*8   AIRE, CDGX, CDGY, CDGZ, IXX, IYY, IZZ, IXY, IXZ, IYZ
      REAL*8   IXPRIN, IYPRIN, IZPRIN, ALPHA, BETA, GAMMA,SINA,COSA
      REAL*8   IXXP,IYYP,IXYP,IXPRIP,IYPRIP,XP,YP,XGP,YGP,XGPP,YGPP
      REAL*8   IXR2,IYR2,IXPRI2,IYPRI2,IXPR2P,IYPR2P
C     ------------------------------------------------------------------
C
      EPSI = 1.D-12
C
      AIRE   = VALE(1)
      CDGX   = VALE(2)
      CDGY   = VALE(3)
      CDGZ   = VALE(4)
      IXX    = VALE(5)
      IYY    = VALE(6)
      IZZ    = VALE(7)
      IXY    = VALE(8)
      IXZ    = VALE(9)
      IYZ    = VALE(10)
      IXR2   = VALE(26)
      IYR2   = VALE(27)
      IXPRI2 = VALE(28)
      IYPRI2 = VALE(29)
      IF ( NDIM .EQ. 2 ) THEN
         VALPAR(1) = AIRE
         VALPAR(2) = CDGX
         VALPAR(3) = CDGY
         VALPAR(4) = IXX
         VALPAR(5) = IYY
         VALPAR(6) = IXY
      ELSEIF ( NDIM .EQ. 3 ) THEN
         DO 10 I = 1 , 10
            VALPAR(I) = VALE(I)
 10      CONTINUE
      ENDIF
C
      IF ( NSYMX  .AND.  .NOT. NSYMY ) THEN
         CDGY = 0.D0
         DY = VALE(3)
         IXX = 2*IXX + 2*AIRE*DY*DY
         IYY = 2*IYY
         IZZ = 2*IZZ + 2*AIRE*DY*DY
         AIRE = 2 * AIRE
      ELSEIF ( NSYMY  .AND.  .NOT. NSYMX ) THEN
         CDGX = 0.D0
         DX = VALE(2)
         IXX = 2*IXX
         IYY = 2*IYY + 2*AIRE*DX*DX
         IZZ = 2*IZZ + 2*AIRE*DX*DX
         AIRE = 2 * AIRE
      ELSEIF ( NSYMX  .AND.  NSYMY ) THEN
         CDGX = 0.D0
         CDGY = 0.D0
         DX = VALE(2)
         DY = VALE(3)
         IXX = 4*IXX + 4*AIRE*DY*DY
         IYY = 4*IYY + 4*AIRE*DX*DX
         IZZ = 4*IZZ + 4*AIRE*(DX*DX + DY*DY)
         IXY = 0.D0
         AIRE = 4 * AIRE
      ENDIF
      IF ( NDIM .EQ. 2 ) THEN
         VALPAR(12) = AIRE
         VALPAR(13) = CDGX
         VALPAR(14) = CDGY
         VALPAR(15) = IXX
         VALPAR(16) = IYY
         VALPAR(17) = IXY
         VALPAR(36) = IXR2
         VALPAR(37) = IYR2
      ELSEIF ( NDIM .EQ. 3 ) THEN
         VALPAR(18) = AIRE
         VALPAR(19) = CDGX
         VALPAR(20) = CDGY
         VALPAR(21) = CDGZ
         VALPAR(22) = IXX
         VALPAR(23) = IYY
         VALPAR(24) = IZZ
         VALPAR(25) = IXY
         VALPAR(26) = IXZ
         VALPAR(27) = IYZ
      ENDIF
C
C     TRAITEMENT DE ORIG_INER
C
      IF ( NP .NE. 0 ) THEN
         IF ( NDIM .EQ. 2 ) THEN
            XP = XYP(1)
            YP = XYP(2)
            XGP = XP - CDGX
            YGP = YP - CDGY
            IXXP = IXX + AIRE*YGP*YGP
            IYYP = IYY + AIRE*XGP*XGP
            IXYP = IXY + AIRE*XGP*YGP
            IXPR2P = IXR2 - YGP*(3.0D0*IXX+IYY)
     +               - AIRE*YGP*(XGP*XGP+YGP*YGP) - 2.0D0*XGP*IXY
            IYPR2P = IYR2 - XGP*(3.0D0*IYY+IXX)
     +               - AIRE*XGP*(XGP*XGP+YGP*YGP) - 2.0D0*YGP*IXY
            VALPAR(21) = XP
            VALPAR(22) = YP
            VALPAR(23) = IXXP
            VALPAR(24) = IYYP
            VALPAR(25) = IXYP
         ENDIF
      ENDIF
C
      IF ( ABS(IXX).LT.EPSI .AND. ABS(IYY).LT.EPSI .AND.
     +     ABS(IZZ).LT.EPSI .AND. ABS(IXY).LT.EPSI .AND.
     +     ABS(IXZ).LT.EPSI .AND. ABS(IYZ).LT.EPSI ) THEN

         IXPRIN = 0.D0
         IYPRIN = 0.D0
         IZPRIN = 0.D0
         ALPHA  = 0.D0
         BETA   = 0.D0
         GAMMA  = 0.D0
         IXPRI2 = 0.D0
         IYPRI2 = 0.D0
      ELSE
         AR(1) = IXX
         AR(2) = - IXY
         AR(3) = - IXZ
         AR(4) = IYY
         AR(5) = - IYZ
         AR(6) = IZZ
         BR(1) = 1.D0
         BR(2) = 0.D0
         BR(3) = 0.D0
         BR(4) = 1.D0
         BR(5) = 0.D0
         BR(6) = 1.D0
         NBVEC = 3
         NPERM = 12
         TOL = 1.D-10
         TOLDYN = 1.D-2
         ITYPE = 2
         IORDRE = 2
         CALL JACOBI(NBVEC,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,JAC,
     &               NITJAC,ITYPE,IORDRE)
         IF (VALPRO(1).LT.VALPRO(2)) THEN
           V1(1) = 0.D0
           V1(2) = 0.D0
           V1(3) = 0.D0
           V2(1) = VECPRO(1,1)
           V2(2) = VECPRO(2,1)
           V2(3) = VECPRO(3,1)
           V3(1) = VECPRO(1,2)
           V3(2) = VECPRO(2,2)
           V3(3) = VECPRO(3,2)
           CALL ORIEN2 ( V1, V2, V3, ANGL )
           IXPRIN = VALPRO(1)
           IYPRIN = VALPRO(2)
           IZPRIN = VALPRO(3)
           ALPHA  = ANGL(1) * R8RDDG()
           BETA   = ANGL(2) * R8RDDG()
           GAMMA  = ANGL(3) * R8RDDG()
         ELSE
           V1(1) = 0.D0
           V1(2) = 0.D0
           V1(3) = 0.D0
           V2(1) = VECPRO(1,2)
           V2(2) = VECPRO(2,2)
           V2(3) = VECPRO(3,2)
           V3(1) = VECPRO(1,1)
           V3(2) = VECPRO(2,1)
           V3(3) = VECPRO(3,1)
           CALL ORIEN2 ( V1, V2, V3, ANGL )
           IXPRIN = VALPRO(2)
           IYPRIN = VALPRO(1)
           IZPRIN = VALPRO(3)
           ALPHA  = ANGL(1) * R8RDDG()
           BETA   = ANGL(2) * R8RDDG()
           GAMMA  = ANGL(3) * R8RDDG()
           IXPRI2 = -SIN(ANGL(1))*IYR2 + COS(ANGL(1))*IXR2
           IYPRI2 =  COS(ANGL(1))*IYR2 + SIN(ANGL(1))*IXR2
         ENDIF
      ENDIF
      IF ( NDIM .EQ. 2 ) THEN
         VALPAR(18) = IXPRIN
         VALPAR(19) = IYPRIN
         VALPAR(20) = ALPHA
         VALPAR(38) = IXPRI2
         VALPAR(39) = IYPRI2
         VALPAR(40) = IXPR2P
         VALPAR(41) = IYPR2P
      ELSEIF ( NDIM .EQ. 3 ) THEN
         VALPAR(26) = IXPRIN
         VALPAR(27) = IYPRIN
         VALPAR(28) = IZPRIN
         VALPAR(29) = ALPHA
         VALPAR(30) = BETA
         VALPAR(31) = GAMMA
      ENDIF
C
      END
