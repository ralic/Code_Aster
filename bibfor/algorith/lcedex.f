      SUBROUTINE LCEDEX(OPTION,IMATE,NPG,LGPG,S,Q,VIM,VIP,ALPHAP,DALFS)
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2005   AUTEUR LAVERNE J.LAVERNE 
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
      
      IMPLICIT NONE

      INTEGER  IMATE, NPG, LGPG
      REAL*8   S(2),Q(2,2),DALFS(2,2),ALPHAP(2)
      REAL*8   VIM(LGPG,NPG),VIP(LGPG,NPG)
      CHARACTER*16  OPTION
      
C-----------------------------------------------------------------------
C     COMPORTEMENT DE L'ELEMENT A DISCONTINUITE POUR LA LOI 
C    'ZONE COHESIVE' EXPONENTIELLE : CZM_EXP
C
C IN 
C     NPG     : NOMBRE DE POINTS DE GAUSS
C     LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C     IMATE   : ADRESSE DU MATERIAU CODE
C     COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
C                               (2) = NB VARIABLES INTERNES / PG
C                               (3) = HYPOTHESE SUR LES DEFORMATIONS
C     OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C     VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C     S,Q     : QUANTITES CINEMATIQUES NECESSAIRES POUR CALCUL DU SAUT
C
C OUT 
C     ALPHAP  : SAUT A L'INSTANT PLUS
C     VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
C     DALFS   : DEVIVEE DU SAUT PAR RAPPORT A S 
C
C-----------------------------------------------------------------------
      
      LOGICAL RESI,RIGI,ELAS      
      INTEGER I,J,KPG
      REAL*8  COEF1,COEF2,COEF3
      REAL*8  SIGMC,GC,LC,SEUIL,NORMA
      REAL*8  DSIALF(2,2),H(2,2),DET            
      REAL*8  VALRES(2)
      CHARACTER*2 CODRET(2)
      CHARACTER*8 NOMRES(2)
      
C - INITIALISATIONS :
C -------------------

      RESI = OPTION.EQ.'RAPH_MECA' .OR. OPTION.EQ.'FULL_MECA'
      RIGI = OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RIGI_MECA_TANG'
                  
      COEF1=0.D0
      COEF2=0.D0 
      COEF3=0.D0 

C RECUPERATION DES PARAMETRES PHYSIQUES :
C ---------------------------------------
      NOMRES(1) = 'GC'
      NOMRES(2) = 'SIGM_C'
      
      CALL RCVALA ( IMATE,' ','RUPT_FRAG',0,' ',0.D0,2,
     &                 NOMRES,VALRES,CODRET, 'F ' )

      GC    = VALRES(1)      
      SIGMC = VALRES(2)
      LC     = GC/SIGMC        
      SEUIL  = VIM(3,1)
                  
C CALCUL DU SAUT DANS L'ELEMENT : 'ALPHAP' 
C -----------------------------------------

      IF (RESI) THEN    
        CALL NMEDAL(ALPHAP,SIGMC,GC,S,Q,SEUIL)      
      ENDIF

      IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
        ALPHAP(1)=VIM(1,1)
        ALPHAP(2)=VIM(2,1)
      ENDIF


C  MISE A JOUR DES VARIABLES INTERNES
C -----------------------------------
C VI1 = alpha(1)
C VI2 = alpha(2)
C VI3 = seuil
C VI4 = regime (adoucissant : 1 ou decharge : 0)
C VI5 = pourcentage d'énergie dissipee
C VI6 = contrainte normale
C VI7 = contrainte tangentielle
                  
      IF (RESI) THEN
      
        NORMA = SQRT( ALPHAP(1)**2 + ALPHAP(2)**2 )
      
        IF ( NORMA .LE. SEUIL ) THEN
        
          ELAS=.TRUE.
          DO 67 KPG=1,NPG
            VIP(1,KPG) = ALPHAP(1)
            VIP(2,KPG) = ALPHAP(2)
            VIP(3,KPG) = VIM(3,KPG)
            VIP(4,KPG) = 0.D0 
            VIP(5,KPG) = VIM(5,KPG)
            VIP(6,KPG) = S(1) + Q(1,1)*ALPHAP(1) + Q(1,2)*ALPHAP(2)
            VIP(7,KPG) = S(2) + Q(2,1)*ALPHAP(1) + Q(2,2)*ALPHAP(2)
  67      CONTINUE  
                            
        ELSE
        
          ELAS=.FALSE.
          DO 68 KPG=1,NPG
            VIP(1,KPG) = ALPHAP(1)
            VIP(2,KPG) = ALPHAP(2)
            VIP(3,KPG) = NORMA
            VIP(4,KPG) = 1.D0
            VIP(5,KPG) = 1.D0 - EXP(-NORMA/LC)
            VIP(6,KPG) = S(1) + Q(1,1)*ALPHAP(1) + Q(1,2)*ALPHAP(2) 
            VIP(7,KPG) = S(2) + Q(2,1)*ALPHAP(1) + Q(2,2)*ALPHAP(2)
  68      CONTINUE  
                     
        ENDIF
        
      ENDIF     

      
C CALCUL DE DALFS TERME LIE AU COMPORTEMENT, NECESSAIRE POUR LA 
C MATRICE TANGENTE :
C----------------------------------------------------------------
      
      IF (RIGI) THEN

C       CALCUL DE LA DERIVEE DU VECTEUR CONTRAINTE 
C       PAR RAPPORT AU SAUT : 'DSIALF'
     
        IF (OPTION.EQ.'RIGI_MECA_TANG') ELAS=(NINT(VIM(4,1)).EQ.0)
                          
        IF (ELAS) THEN
       
          IF (SEUIL .EQ. 0.D0) THEN
            CALL R8INIR(4, 0.D0,DSIALF ,1)
          ELSE                                    
            COEF1 = SIGMC*EXP(-SIGMC*SEUIL/GC)/SEUIL           
            CALL R8INIR(4, 0.D0,DSIALF ,1)
            DSIALF(1,1) = COEF1   
            DSIALF(2,2) = COEF1
          ENDIF
     
        ELSE 
       
          NORMA = SQRT( ALPHAP(1)**2 + ALPHAP(2)**2 )
          COEF2 = SIGMC*EXP(-SIGMC*NORMA/GC)/NORMA
          COEF3 = - COEF2*( (1/NORMA) + SIGMC/GC )/NORMA
                  
          CALL R8INIR(4, 0.D0,DSIALF ,1)
          DSIALF(1,1) = COEF2 + COEF3*ALPHAP(1)*ALPHAP(1) 
          DSIALF(1,2) =         COEF3*ALPHAP(1)*ALPHAP(2)
          DSIALF(2,1) =         COEF3*ALPHAP(2)*ALPHAP(1)
          DSIALF(2,2) = COEF2 + COEF3*ALPHAP(2)*ALPHAP(2)
                                
        ENDIF 
      
C       CALCUL DES DALFS : DERIVEE DE ALPHA PAR RAPPORT A S : 
C       DALFS  = (DSIALF  - Q  )^-1
      
        IF ( (ELAS) .AND. (SEUIL .EQ. 0.D0) ) THEN   
              
          CALL R8INIR(4, 0.D0,DALFS,1)
         
        ELSE       

          CALL R8INIR(4 , 0.D0,H    ,1)
          CALL R8INIR(4 , 0.D0,DALFS ,1)

          DO 71 I=1,2
            DO 72 J=1,2
               H(I,J) = DSIALF(I,J) - Q(I,J)
   72       CONTINUE
   71     CONTINUE
  
          DET  =  H(1,1)*H(2,2) -  H(1,2)**2
       
          IF ( ABS(DET) .LE. 1.D-16 )
     &     CALL UTMESS('F','LCEDEX','MATRICE H NON INVERSIBLE') 
    
          DALFS(1,1) =  H(2,2)/DET
          DALFS(2,2) =  H(1,1)/DET
          DALFS(1,2) = -H(1,2)/DET
          DALFS(2,1) = -H(1,2)/DET 
           
        ENDIF
        
      ENDIF 
      
      END
