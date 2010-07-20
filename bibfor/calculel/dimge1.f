      INTEGER FUNCTION DIMGE1(IGE1,IGE2)
      IMPLICIT NONE
      INTEGER IGE1,IGE2
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 19/07/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     -- SERT AU CALCUL DE DIM_GEOM D'UN LIGREL
C    IN:
C       IGE1 : VALEUR DE DIM_GEOM (/1/2/3/120/023/103/123)
C       IGE2 : VALEUR DE DIM_GEOM (/1/2/3/120/023/103/123)
C    OUT:
C       DIMGE1   : "CUMUL" DE IGE1 ET IGE2

C ----------------------------------------------------------------------
      INTEGER I1(3),I2(3),I3(3),K
C DEB ------------------------------------------------------------------

C     -- SI IGE1 (OU IGE2) = 0, C'EST FACILE :
      IF (IGE1.EQ.0) THEN
        DIMGE1=IGE2
        GOTO 9999
      ENDIF
      IF (IGE2.EQ.0) THEN
        DIMGE1=IGE1
        GOTO 9999
      ENDIF

C     -- ON DECODE IGE1 DANS I1 :
      DO 1, K=1,3
        I1(K)=0
 1    CONTINUE
      IF (IGE1.GT.10) THEN
        I1(1)=IGE1/100
        I1(3)=MOD(IGE1,10)
        I1(2)=(MOD(IGE1,100) -I1(3))/10
      ELSE
        CALL ASSERT(IGE1.GE.0.AND.IGE1.LE.3)
        IF (IGE1.EQ.1) I1(1)=1
        IF (IGE1.EQ.2) I1(2)=2
        IF (IGE1.EQ.3) I1(3)=3
      ENDIF

C     -- ON DECODE IGE2 DANS I2 :
      DO 2, K=1,3
        I2(K)=0
 2    CONTINUE
      IF (IGE2.GT.10) THEN
        I2(1)=IGE2/100
        I2(3)=MOD(IGE2,10)
        I2(2)=(MOD(IGE2,100) -I2(3))/10
      ELSE
        CALL ASSERT(IGE2.GE.0.AND.IGE2.LE.3)
        IF (IGE2.EQ.1) I2(1)=1
        IF (IGE2.EQ.2) I2(2)=2
        IF (IGE2.EQ.3) I2(3)=3
      ENDIF

C     -- ON CALCULE I3 :
      DO 3, K=1,3
        CALL ASSERT(I1(K).EQ.0 .OR.  I1(K).EQ.K)
        CALL ASSERT(I2(K).EQ.0 .OR.  I2(K).EQ.K)
        I3(K)=MAX(I1(K),I2(K))
 3    CONTINUE

C     -- ON RECODE LE RESULTAT :
      IF ((I3(1)+I3(2)+I3(3)).GT.1) THEN
        DIMGE1=100*I3(1)
        DIMGE1=DIMGE1+10*I3(2)
        DIMGE1=DIMGE1+1*I3(3)
      ELSE
        IF (I3(1).EQ.1) DIMGE1=1
        IF (I3(2).EQ.1) DIMGE1=2
        IF (I3(3).EQ.1) DIMGE1=3
      ENDIF

9999  CONTINUE

      END
