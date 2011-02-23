      SUBROUTINE LCEIFA(FAMI,KPG,KSP,MAT,OPTION,MU,SU,DELTAP,
     &                  DDEDT,VIM,VIP,R)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/02/2011   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE LAVERNE J.LAVERNE
      IMPLICIT NONE
      CHARACTER*16 OPTION
      INTEGER MAT,KPG,KSP
      REAL*8  MU(3),SU(3),DELTAP(6),DDEDT(6,6),VIM(*),VIP(*),R      
      CHARACTER*(*)  FAMI

C-----------------------------------------------------------------------
C            LOI DE COMPORTEMENT COHESIVE DE FATIGUE CZM_FAT_MIX 
C            POUR LES ELEMENTS D'INTERFACE 2D ET 3D. 
C
C IN : FAMI,KPG,KSP,MAT,OPTION
C      MU  : LAGRANGE
C      SU  : SAUT DE U
C      VIM : VARIABLES INTERNES
C      
C OUT : DELTAP : DELTA, SOLUTION DE LA MINIMISATION 
C       DDEDT : D(DELTA)/DT
C       VIP   : VARIABLES INTERNES MISES A JOUR
C       R     : PENALISATION DU LAGRANGE 
C-----------------------------------------------------------------------

      LOGICAL RESI, RIGI, ELAS
      INTEGER REGIME
      REAL*8  SC,GC,DC,DC0,C,H,KA,SK,RK,VAL(4),KAP,GAP,INCR
      REAL*8  DN,TN,T(3),DDNDTN,DELTAM
      CHARACTER*2 COD(4)
      CHARACTER*8 NOM(4)
      CHARACTER*1 POUM
      
      DATA NOM /'GC','SIGM_C','PENA_LAGR','RIGI_GLIS'/
C-----------------------------------------------------------------------


C OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE

      RESI = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RAPH'
      RIGI = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RIGI'
      ELAS = OPTION(11:14).EQ.'ELAS' 

C RECUPERATION DES PARAMETRES PHYSIQUES

      IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
        POUM = '-'
      ELSE
        POUM = '+'
      ENDIF

      CALL RCVALB(FAMI,KPG,KSP,POUM,MAT,' ','RUPT_FRAG',0,' ',
     &            0.D0,4,NOM,VAL,COD,'F ')

      GC   = VAL(1)      
      SC   = VAL(2)  
      DC0  = 2*GC/SC
      H    = SC/DC0
      R    = H * VAL(3)
      C    = H * VAL(4)
      
C -- INITIALISATION

C  CUMUL D'OUVERTURE
      KA = VIM(1)   
      DELTAM = VIM(7)  

      SK   = MAX( 0.D0 , SC*(1.D0 - KA/DC0) )
      RK   = MAX( 0.D0 , SC*(1.D0 - (KA-DELTAM)/DC0) )
      DC   = DC0 - KA + DELTAM

C    FORCES COHESIVES AUGMENTEES       
      T(1) = MU(1) + R*SU(1)
      T(2) = MU(2) + R*SU(2)
      T(3) = MU(3) + R*SU(3)
      TN   = T(1)      
 
C -- CALCUL DE DELTA          
      
C    SI RIGI_MECA_*      
      IF (.NOT. RESI) THEN
        REGIME = NINT(VIM(2))
        GOTO 5000
      END IF
                        
C    CONTACT   
      IF (TN .LT. 0.D0) THEN
        REGIME = -1
        DN = 0
        
C    SURFACE LIBRE (SOUS CONTRAINTE)   
      ELSE IF (TN .LT. R*DELTAM) THEN
        REGIME = 3
        DN = TN/R

C    ADHERENCE (INITIALE OU COURANTE)          
      ELSE IF (TN .LE. R*DELTAM + SK) THEN
        REGIME = 0
        DN = DELTAM
        
C    ENDOMMAGEMENT                    
      ELSE IF (TN .LT. R*DC) THEN
        REGIME = 1
        DN = (TN-RK)/(R-H)
        
C    SURFACE LIBRE FINALE (RUPTURE)
      ELSE 
        REGIME = 2
        DN = TN/R
      END IF

      CALL R8INIR(6, 0.D0, DELTAP,1)
C    COMPOSANTE DE L'OUVERTURE :        
      DELTAP(1) = DN
C    COMPOSANTES DE CISAILLEMENT : ELASTIQUE
      DELTAP(2) = T(2)/(C+R)
      DELTAP(3) = T(3)/(C+R)
      
      
C -- ACTUALISATION DES VARIABLES INTERNES
C   V1 :  PLUS GRANDE NORME DU SAUT (SATURE A DC) 
C   V2 :  REGIME DE LA LOI
C        -1 : CONTACT
C         0 : ADHERENCE INITIALE OU COURANTE 
C         1 : DISSIPATION
C         2 : SURFACE LIBRE FINALE (RUPTURE)   
C         3 : SURFACE LIBRE (SOUS CONTRAINTE)  
C   V3 :  INDICATEUR D'ENDOMMAGEMENT  
C         0 : SAIN 
C         1 : ENDOMMAGE
C         2 : CASSE   
C   V4 :  SEUIL, POURCENTAGE D'ENERGIE DISSIPEE
C   V5 :  VALEUR DE L'ENERGIE DISSIPEE (V4*GC)
C   V6 :  NON UTILISEE POUR CETTE LOI
C   V7 A V9 : VALEURS DE DELTA

      INCR = MAX(0.D0, DELTAP(1) - DELTAM)
      KAP = MIN( KA + INCR , DC0 )
      
      GAP = KAP/DC0 * (2 - KAP/DC0)
      GAP = MAX(0.D0,GAP)
      GAP = MIN(1.D0,GAP)

      VIP(1) = KAP
      VIP(2) = REGIME

        IF (KAP.EQ.0.D0) THEN
          VIP(3) = 0.D0
        ELSEIF (KAP.EQ.DC0) THEN
          VIP(3) = 2.D0
        ELSE
          VIP(3) = 1.D0        
        ENDIF
      
      VIP(4) = GAP
      VIP(5) = GC*VIP(4)
      VIP(6) = 0.D0       
      VIP(7) = DELTAP(1)
      VIP(8) = DELTAP(2)
      VIP(9) = DELTAP(3)


C -- MATRICE TANGENTE

 5000 CONTINUE
      IF (.NOT. RIGI) GOTO 9999
      
C    AJUSTEMENT POUR PRENDRE EN COMPTE *_MECA_ELAS
      IF (ELAS) THEN
        IF (REGIME.EQ.1) REGIME = 0
      END IF

      CALL R8INIR(36, 0.D0, DDEDT,1)

      DDEDT(2,2) = 1/(C+R)
      DDEDT(3,3) = 1/(C+R)
 
      IF (REGIME .EQ. 0) THEN
        DDNDTN = 0
      ELSE IF (REGIME .EQ. 1) THEN
        DDNDTN = 1/(R-H)
      ELSE IF (REGIME .EQ. 2) THEN
        DDNDTN = 1/R
      ELSE IF (REGIME .EQ. 3) THEN
        DDNDTN = 1/R
      ELSE IF (REGIME .EQ. -1) THEN
        DDNDTN = 0
      END IF
      DDEDT(1,1) = DDNDTN
      
 9999 CONTINUE
 
      END
