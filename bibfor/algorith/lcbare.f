      SUBROUTINE LCBARE(MATE,OPTION,SU,SIGMA,DSIDEP,VIM,VIP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2002   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================


      IMPLICIT NONE   
      INTEGER MATE
      REAL*8 SU(2),SIGMA(2),DSIDEP(2,2),VIM(*),VIP(*)
      CHARACTER*16 OPTION

C-----------------------------------------------------------------------
C
C BUT: CALCUL DE LA CONTRAINTE AINSI QUE SA DERIVEE PAR RAPPORT AU SAUT 
C   DE DEPLACEMENT A PARTIR D'UNE LOI DE COMPORTEMENT DE TYPE BARENBLATT
C
C   LA CONTRAINTE DERIVE DU POTENTIEL KAPPA FONCTION DE LA NORME DU 
C   SAUT DE DEPLACEMENT SUNO :
C
C   SUR (0,LAMBDA(       : KAPPA (SUNO) = COEF2*(SUNO)**2 + CSTE    
C   SUR (LAMBDA,+INFINI( : KAPPA (SUNO) = K*(1-EXP(-SIGMAC * SUNO / K))
C
C IN  : SU, MATE , OPTION, VIM 
C OUT : SIGMA , DSIDEP , VIP
C I/O : 
C
C-----------------------------------------------------------------------

      LOGICAL RESI, RIGI, ELAS
      REAL*8 SUNO,COEF1,COEF2,COEF3,SUNODT,SUNODN,
     &       SIGMAC,K,LAMBDA,VALRES(3)
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
                            
C RECUPERATION DES PARAMETRES PHYSIQUES :

      NOMRES(1) = 'GC'
      NOMRES(2) = 'SIGM_C'
      NOMRES(3) = 'SAUT_C'
      
      CALL RCVALA ( MATE,'RUPT_FRAG',0,' ',0.D0,3,
     &                 NOMRES,VALRES,CODRET, 'F ' )

      K      = VALRES(1)      
      SIGMAC = VALRES(2)  
      LAMBDA = VALRES(3)
 
C OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE :
 
      RESI = (OPTION.EQ.'FULL_MECA') .OR. (OPTION.EQ.'RAPH_MECA')
      RIGI = (OPTION.EQ.'FULL_MECA') .OR. (OPTION.EQ.'RIGI_MECA_TANG')
     
C CALCUL DE LA NORME DU SAUT :
        
      SUNO = SQRT(SU(1)*SU(1) + SU(2)*SU(2))
      
C VALEURS INTERMEDIAIRES :      
      
      COEF1 = EXP(- SIGMAC * SUNO / K)
      
      COEF2 =  ( 0.5D0*SIGMAC / (VIM(1)+LAMBDA) )
     &      * EXP( -SIGMAC * (VIM(1)+LAMBDA)/K ) 

C (VIM(1) + LAMBDA)  EST UN SEUIL VARIABLE QUI S'AJUSTE SUR SUNO 
C POUR DECHARGER ELASTIQUEMENT SI SUNO (I.E. LE CHARGEMENT) DIMINUE

C CALCUL DES CONTRAINTES :

      IF (RESI) THEN
                      
        IF ( SUNO .LE. (VIM(1) + LAMBDA) ) THEN        
          ELAS = .TRUE.
          SIGMA(1) = 2*COEF2*SU(1)    
          SIGMA(2) = 2*COEF2*SU(2)    
          VIP(1) = VIM(1)
          VIP(2) = 0.D0
        ELSE 
          ELAS = .FALSE.   
          SIGMA(1) = SIGMAC * SU(1) * COEF1 / SUNO
          SIGMA(2) = SIGMAC * SU(2) * COEF1 / SUNO            
          VIP(1) = SUNO - LAMBDA
          VIP(2) = 1.D0  
        ENDIF
      ENDIF 


C CALCUL DES DERIVEES DES CONTRAINTES PAR RAPPORT AU SAUT :
      
      IF (RIGI) THEN
      
        IF (OPTION.EQ.'RIGI_MECA_TANG') ELAS = (NINT(VIM(2)) .EQ. 0)
      
        IF (ELAS) THEN
          DSIDEP(1,1) = 2*COEF2     
          DSIDEP(2,2) = 2*COEF2     
          DSIDEP(1,2) = 0.D0       
          DSIDEP(2,1) = 0.D0       
         
        ELSE
          SUNODT = SU(1) / SUNO   
          SUNODN = SU(2) / SUNO              
          COEF3  = SIGMAC/K + 1.D0/SUNO
          
          DSIDEP(1,1) =   SIGMAC*COEF1 * (1-SU(1)*SUNODT*COEF3) / SUNO
          DSIDEP(1,2) = - SIGMAC*SU(1)*SUNODN*COEF1*COEF3 / SUNO
          DSIDEP(2,1) = - SIGMAC*SU(2)*SUNODT*COEF1*COEF3 / SUNO
          DSIDEP(2,2) =   SIGMAC*COEF1 * (1-SU(2)*SUNODN*COEF3) / SUNO
        ENDIF 
        
      ENDIF
      
      
      
      

      END
