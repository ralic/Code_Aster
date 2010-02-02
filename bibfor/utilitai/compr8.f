      FUNCTION COMPR8 (A,COMP,B,EPS,CRIT)
      IMPLICIT  NONE

      LOGICAL      COMPR8
      REAL*8       A,B,EPS
      INTEGER      CRIT
      CHARACTER*2  COMP

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 01/02/2010   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C
C FONCTION COMPARANT 2 REELS (REAL*8) ENTRE EUX
C     =.TRUE. SI    A.COMP.B A LA PRECISION PREC DONNEE
C
C     A = B EN ABSOLU   <=> |A-B| <= EPS
C     A = B EN RELATIF  <=> |A-B| <= EPS.MIN(|A|,|B|)

C     A <= B EN ABSOLU  <=> A <= B + EPS
C     A <= B EN RELATIF <=> A <= B + EPS.MIN(|A|,|B|)
C
C     A < B EN ABSOLU   <=> A <= B - EPS
C     A < B EN RELATIF  <=> A <= B - EPS.MIN(|A|,|B|)
C
C     A >= B EN ABSOLU  <=> A >= B - EPS
C     A >= B EN RELATIF <=> A >= B - EPS.MIN(|A|,|B|)
C
C     A > B EN ABSOLU   <=> A >= B + EPS
C     A > B EN RELATIF  <=> A >= B + EPS.MIN(|A|,|B|)
C
C ----------------------------------------------------------------------
C
C IN   A      : REEL A GAUCHE DU SIGNE
C IN   B      : REEL A DROITE DU SIGNE
C IN   EPS    : PRECISION
C IN   CRIT   : CRITERE (=0 si ABSOLU ou 1 si RELATIF)
C IN   COMP   : TYPE DE COMPARAISON ENTRE REELS : =, <, >, >=, <=
C OUT  COMPR8 : TRUE SI LA RELATION EST VERIFIEE

      REAL*8       R8MAEM,R8PREM,MINAB,MIN,TOLE

      COMPR8=.FALSE.
      MINAB = MIN(ABS(A),ABS(B))

C     --------------------
C     TESTS PRELIMINAIRES
C     --------------------

C     TEST DE LA PRECISION (POSITIVE OU NULLE)
      CALL ASSERT(EPS.GE.0.D0)
   
      CALL ASSERT(CRIT.EQ.0.OR.CRIT.EQ.1)

C     --------------------
C     COMPARAISONS
C     --------------------

      IF (CRIT.EQ.0) TOLE=EPS      
      IF (CRIT.EQ.1) TOLE=EPS*MINAB

      IF (COMP.EQ.'EQ') THEN
      
        IF (ABS(A-B).LE.TOLE) COMPR8=.TRUE.

      ELSEIF (COMP.EQ.'LE') THEN

        IF (A.LE.B+TOLE) COMPR8=.TRUE.

      ELSEIF (COMP.EQ.'LT') THEN

        IF (A.LE.B-TOLE) COMPR8=.TRUE.

      ELSEIF (COMP.EQ.'GE') THEN

        IF (A.GE.B-TOLE) COMPR8=.TRUE.

      ELSEIF (COMP.EQ.'GT') THEN

        IF (A.GE.B+TOLE) COMPR8=.TRUE.
        
      ELSE

        CALL ASSERT(.FALSE.)
        
      ENDIF


      END
