        SUBROUTINE NORTON(NVI,VINI,COEFT,NMAT,SIGI,DVIN,IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ==================================================================
C      MODELE VISCOPLASTIQUE DE NORTON
C ==================================================================
C     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
C     INTERNES DU MODELE
C       IN  NVI     :  NOMBRE DE VARIABLES INTERNES
C           VINI    :  VARIABLES INTERNES A T
C           COEFT   :  COEFFICIENTS MATERIAU INELASTIQUE A T
C           NMAT    :  DIMENSION MAXI DE COEFT 
C           SIGP    :  CONTRAINTES A L'INSTANT COURANT, AVEC SQRT(2)
C           DEPS    :  INCREMENT DE DEFORMATIONS, AVEC SQRT(2)
C     OUT:
C           DVIN    :  DERIVEES DES VARIABLES INTERNES A T
C           IRET    :  CODE RETOUR =0 SI OK, =1 SI PB
C     ----------------------------------------------------------------
      INTEGER IRET,ITENS,NDI,NMAT,NVI,NDT
      REAL*8 COEFT(NMAT),VINI(NVI),DVIN(NVI),SMX(6),SIGI(6)
      REAL*8 DP,N,UNSURK,GRJ2V,EPSI,R8MIEM,LCNRTS,DEPS(6)
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT,    NDI
C     ----------------------------------------------------------------

      IRET=0
C     INITIALISATION DES DERIVEES DES VARIABLES INTERNES A ZERO      
      CALL R8INIR(7,0.D0,DVIN,1)
C
C --    COEFFICIENTS MATERIAU 
      N      = COEFT(1)
      UNSURK = COEFT(2)

C     ZERO NUMERIQUE ABSOLU
      EPSI=R8MIEM()    
      
C------------ CALCUL DU TENSEUR DEVIATORIQUE DES CONTRAINTES ---

      CALL LCDEVI(SIGI , SMX )
      
C------------CALCUL DU DEUXIEME INVARIANT DE CONTRAINTE  -------

      GRJ2V  = LCNRTS(SMX )
            
C------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
C
      IF (GRJ2V .GT. EPSI) THEN 
           
         DP=(GRJ2V*UNSURK)**N    
              
C        INUTILE DE CALCULER DES DEFORMATIONS PLASTIQUES MINUSCULES
         IF (DP .GT. 1.D-10) THEN
         
            DO 12 ITENS=1,NDT
               DVIN(ITENS)=1.5D0*DP*SMX(ITENS)/GRJ2V
   12       CONTINUE
            DVIN(7)=DP   
            
         ENDIF
         
      ENDIF
      END
