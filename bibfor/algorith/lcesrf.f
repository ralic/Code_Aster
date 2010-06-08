      REAL*8 FUNCTION LCESRF(MODE,A) 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/04/2010   AUTEUR MICHEL S.MICHEL 
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
      IMPLICIT NONE      
      INTEGER MODE
      REAL*8  A
C ----------------------------------------------------------------------
C  CALCUL DES FONCTIONS R ET F POUR LA ENDO_SCALAIRE AVEC GRAD_VARI
C ----------------------------------------------------------------------
C  MODE    FONCTION RECHERCHEE
C           1: R(A)
C           2: DRDA(A)
C           3: DFDA(A)
C           4: D2RDA2(A)
C           5: D2FDA2(A)
C           6: RESOLUTION DE G := DRDA(A)*WEPS + DFDA(A) - PHI + R*A = 0
C  A       VALEUR DE L'ARGUMENT A
C ----------------------------------------------------------------------
      INTEGER NRAC
      REAL*8 P0,P1,P2,P3,P4,RAC(4)
      REAL*8 GAMMA,DRDA0,DFDA0,WEPS,PHI,R
      COMMON /LCES/ GAMMA,DRDA0,DFDA0,WEPS,PHI,R
C ----------------------------------------------------------------------
      
      IF (MODE.EQ.1) THEN
        LCESRF = ((1-A)/(1+GAMMA*A))**2 
      
      ELSE IF (MODE.EQ.2) THEN
        LCESRF = -2*(1+GAMMA)/(1+GAMMA*A)**3 * (1-A)

      ELSE IF (MODE.EQ.3) THEN
        LCESRF = DFDA0
        
      ELSE IF (MODE.EQ.4) THEN
        LCESRF =2*(1+GAMMA)/(1+GAMMA*A)**4 
     &         * ( (GAMMA+1)+2*GAMMA*(1-A) )
        
      ELSE IF (MODE.EQ.5) THEN
        LCESRF = 0
        
      ELSE IF (MODE.EQ.6) THEN
        P0 = (DFDA0-PHI)                           - 2*(1+GAMMA)*WEPS
        P1 = (DFDA0-PHI)*3*GAMMA    + R            + 2*(1+GAMMA)*WEPS
        P2 = (DFDA0-PHI)*3*GAMMA**2 + 3*R*GAMMA
        P3 = (DFDA0-PHI)*GAMMA**3   + 3*R*GAMMA**2
        P4 =                          R*GAMMA**3
        CALL ZEROP4(P3/P4,P2/P4,P1/P4,P0/P4,RAC,NRAC)
        IF (NRAC.EQ.0) CALL U2MESS('F','ALGORITH17_9')
        LCESRF = RAC(1)
        
      ELSE 
        CALL ASSERT(.FALSE.)
        
      END IF
      
      END
