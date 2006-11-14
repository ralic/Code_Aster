      FUNCTION DCARGU(C)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/11/2006   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C-----------------------------------------------------------------------
C CALCUL DE L'ARGUMENT D'UN NOMBRE COMPLEXE
C PAR CONVENTION ON AFFECTE 0.D0 SI C = (0.D0,0.D0)
C-----------------------------------------------------------------------
C  IN : C : NOMBRE COMPLEXE DONT ON VEUT CALCULER L'ARGUMENT
C-----------------------------------------------------------------------
      REAL*8     DCARGU
      COMPLEX*16 C
C-----------------------------------------------------------------------
      PI  = R8PI()
C
      IF (DBLE(C).EQ.0.D0) THEN
        IF (DIMAG(C).GT.0.D0) THEN
          DCARGU = PI/2.D0
        ELSE IF (DIMAG(C).LT.0.D0) THEN
          DCARGU = -PI/2.D0
        ELSE
          DCARGU = 0.D0
        ENDIF
      ELSE IF (DBLE(C).GT.0.D0) THEN
        DCARGU = DBLE(ATAN2(DIMAG(C),DBLE(C)))
      ELSE IF (DBLE(C).LT.0.D0) THEN
        DCARGU = DBLE(ATAN2(DIMAG(C),DBLE(C))) + PI
      ENDIF
      IF (DCARGU.LT.0.D0) DCARGU = DCARGU + 2.D0*PI
C
      END
