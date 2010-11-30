      SUBROUTINE DFDDE(EPS,ENDO,NDIM,LAMBDA,MU,DFDE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/10/2004   AUTEUR GODARD V.GODARD 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER           NDIM
      REAL*8              EPS(6), LAMBDA, MU
      REAL*8              DFDE(6),ENDO
C
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDO_ORTH_BETON
C     CALCUL DE LA DERIVEE DE LA FORCE THERMODYNAMIQUE(ENDO COMPRESSION)
C     PAR RAPPORT A LA DEFORMATION:DFD/DEPS
C
C     FD=(1-ENDO)(LAMBDA/2*(TR(E).H(-TR(E)))**2+MU*TR(E-**2))-ECROD*ENDO
C     IN  NDIM      : DIMENSION 3(3D) OU 2(2D)
C     IN  EPS        : DEFORMATION
C     IN  ENDO     : ENDOMMAGEMENT DE COMPRESSION
C     IN  LAMBDA   : /
C     IN  MU       : / COEFFICIENTS DE LAME
C     OUT DFDE      : DFD/DEPS
C ----------------------------------------------------------------------

      REAL*8              TREPS,RTEMP,RAC2
      REAL*8              TU(6),VPE(3)
      REAL*8              VALEPS(3),VECEPS(3,3),PHID
      INTEGER           I,J,K,T(3,3)
      
      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3

      RAC2=SQRT(2.D0)


      PHID = 2.D0*(1.D0-ENDO)

      CALL R8INIR(6,0.D0,DFDE,1)

      TREPS=0.D0
      TREPS=EPS(1)+EPS(2)+EPS(3)

      IF (TREPS.LT.0.D0) THEN
        DO 8 I=1,NDIM 
          DFDE(T(I,I))=DFDE(T(I,I))+PHID*LAMBDA*TREPS
 8      CONTINUE
      ENDIF

      CALL DIAGO3(EPS,VECEPS,VALEPS)
      CALL R8INIR(3,0.D0,VPE,1)

      DO 819 I=1,NDIM
        IF (VALEPS(I).LT.0.D0) THEN
          VPE(I)=VALEPS(I)
        ELSE
          VPE(I)=0.D0
        ENDIF
 819  CONTINUE
 
      CALL R8INIR(6,0.D0,TU,1)
      DO 20 I=1,NDIM
        DO 21 J=I,NDIM
          DO 22 K=1,NDIM
            TU(T(I,J))=TU(T(I,J))+VECEPS(I,K)*VPE(K)*VECEPS(J,K)
 22       CONTINUE
 21     CONTINUE
 20   CONTINUE

      DO 23 I=1,NDIM
        DO 24 J=I,NDIM
          IF (J.EQ.I) THEN
            RTEMP=1.D0
          ELSE
            RTEMP=RAC2
          ENDIF
            DFDE(T(I,J))=DFDE(T(I,J))+2.D0*MU*PHID*TU(T(I,J))*RTEMP
  24    CONTINUE
  23  CONTINUE

      
      END
