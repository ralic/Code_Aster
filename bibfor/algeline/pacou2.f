      FUNCTION PACOU2(X,FVEC,VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,
     +                VECR5,VECI1,VG,INDIC,NBM,NMODE,N)
      IMPLICIT NONE
C ---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C-----------------------------------------------------------------------
C
C ARGUMENTS
C ---------
      INCLUDE 'jeveux.h'
      INTEGER N
      REAL*8 X(*),FVEC(*),AMOR(*),VG,MASG(*)
      REAL*8 VECR1(*),VECR2(*),VECR3(*),VECR4(*),VECR5(*)
      INTEGER VECI1(*)
      CHARACTER*8 TYPFLU
C ----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,INDIC ,NBM ,NMODE 
      REAL*8 PACOU2 ,SUM 
C-----------------------------------------------------------------------
      CALL PACOUF(X,FVEC,VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +            VECI1,VG,INDIC,NBM,NMODE)
      SUM = 0.0D0
      DO 11 I = 1,N
        SUM = SUM + FVEC(I)*FVEC(I)
   11 CONTINUE
      PACOU2 = 0.5D0*SUM
C
      END
