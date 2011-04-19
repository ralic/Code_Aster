      SUBROUTINE LCEITC(FAMI,KPG,KSP,MAT,OPTION,MU,SU,DE,
     &                  DDEDT,VIM,VIP,R)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      INTEGER MAT,KPG,KSP
      REAL*8  MU(3),SU(3),DE(6),DDEDT(6,6)
      REAL*8  VIM(*),VIP(*),R
      CHARACTER*16 OPTION
      CHARACTER*(*)  FAMI

C-----------------------------------------------------------------------
C            LOI DE COMPORTEMENT COHESIVE CZM_TAC_MIX 
C            POUR LES ELEMENTS D'INTERFACE 2D ET 3D. 
C
C IN : FAMI,KPG,KSP,MAT,OPTION
C      MU  : LAGRANGE
C      SU  : SAUT DE U
C      VIM : VARIABLES INTERNES
C      
C OUT : DE    : DELTA, SOLUTION DE LA MINIMISATION 
C       DDEDT : D(DELTA)/DT
C       VIP   : VARIABLES INTERNES MISES A JOUR
C       R     : PENALISATION DU LAGRANGE 
C-----------------------------------------------------------------------

      LOGICAL RESI, RIGI, ELAS
      INTEGER REGIME,REGM,I,J
      REAL*8  SC,GC,DC,H,KA,KAP,GA,GAP,SK,VAL(3)
      REAL*8  T(3),PR(3,3),TPO(3),TNO,LBD,D,TMP
      INTEGER COD(3)
      CHARACTER*8 NOM(3)
      CHARACTER*1 POUM
      
      DATA NOM /'GC','SIGM_C','PENA_LAG'/
C-----------------------------------------------------------------------


C -- INITIALISATION

C    OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
      RESI = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RAPH'
      RIGI = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RIGI'
      ELAS = OPTION(11:14).EQ.'ELAS' 
      
C    RECUPERATION DES PARAMETRES PHYSIQUES

      IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
        POUM = '-'
      ELSE
        POUM = '+'
      ENDIF

      CALL RCVALB(FAMI,KPG,KSP,POUM,MAT,' ','RUPT_FRAG',0,' ',
     &            0.D0,3,NOM,VAL,COD,2)
      
      GC   = VAL(1)      
      SC   = VAL(2)  
      DC   = 2*GC/SC
      H    = SC/DC
      R    = H * VAL(3)
      
C    LECTURE DES VARIABLES INTERNES
      GA   = VIM(4) 
      REGM = NINT(VIM(2))
      
C    CALCUL DE KAPPA : KA = DC*(1-SQRT(1-GA))
      TMP  = SQRT(MAX(0.D0,1-GA))
      TMP  = DC*(1-TMP)
      TMP  = MAX(0.D0,TMP)
      TMP  = MIN(DC,TMP)
      KA   = TMP      
      SK   = MAX(0.D0,SC - H*KA)

C -- CALCUL DU SECOND MEMBRE 
       
C    FORCE COHESIVE AUGMENTEE       
      
      T(1) = MU(1) + R*SU(1)
      T(2) = MU(2) + R*SU(2)
      T(3) = MU(3) + R*SU(3)

C    PROJECTEUR DE LA COMPOSANTE NORMALE
      CALL R8INIR(9,0.D0,PR,1)
      
      PR(1,1) = 1
      PR(2,2) = 1      
      PR(3,3) = 1
      
      IF (T(1).LT.0.D0) PR(1,1) = 0

C    PROJECTION DE LA COMPOSANTE NORMALE
      TPO(1) = T(1)*PR(1,1)
      TPO(2) = T(2)*PR(2,2)      
      TPO(3) = T(3)*PR(3,3)
      
C    NORME DU SECOND MEMBRE PROJETE    
      TNO = SQRT(TPO(1)**2 + TPO(2)**2 + TPO(3)**2) 
       
C -- RESOLUTION DU PROBLEME 1D SUR LA NORME

C    DETERMINATION DU REGIME DE COMPORTEMENT
      IF (RESI) THEN
      
C      SURFACE LIBRE (SOUS CONTRAINTE)   
        IF (TNO .LT. R*KA) THEN
          REGIME = 3

C      ADHERENCE (INITIALE OU COURANTE)          
        ELSE IF (TNO .LE. R*KA + SK) THEN
          REGIME = 0
          
C      ENDOMMAGEMENT                    
        ELSE IF (TNO .LT. R*DC) THEN
          REGIME = 1
          
C      SURFACE LIBRE FINALE (RUPTURE)
        ELSE 
          REGIME = 2
        ENDIF
        
      ELSE
        REGIME = REGM
      ENDIF


C    CALCUL DE L'ECOULEMENT 1D SELON LE REGIME DE COMPORTEMENT
      IF (REGIME.EQ.3) THEN
        LBD    = 1/R
      ELSE IF (REGIME.EQ.0) THEN
        IF (TNO.GT.0) THEN
          LBD = KA/TNO
        ELSE
          LBD = 0
        ENDIF
      ELSE IF (REGIME.EQ.1) THEN
        LBD    = (DC + (TNO-R*DC)/(R-H)) / TNO
      ELSE 
        LBD    = 1/R
      ENDIF

      
C -- CONSTRUCTION DE LA SOLUTION 3D

      IF (RESI) THEN
        CALL R8INIR(6, 0.D0, DE,1)
        DE(1) = LBD*TPO(1)
        DE(2) = LBD*TPO(2)
        DE(3) = LBD*TPO(3)
      ENDIF

      
C -- ACTUALISATION DES VARIABLES INTERNES 
C   V1 :  PLUS GRANDE NORME DU SAUT (SEUIL EN SAUT) 
C   V2 :  REGIME DE LA LOI
C         0 : ADHERENCE INITIALE OU COURANTE 
C         1 : DISSIPATION
C         2 : SURFACE LIBRE FINALE (RUPTURE)   
C         3 : SURFACE LIBRE (SOUS CONTRAINTE)  
C   V3 :  INDICATEUR D'ENDOMMAGEMENT  
C         0 : SAIN 
C         1 : ENDOMMAGE
C         2 : CASSE   
C   V4 :  POURCENTAGE D'ENERGIE DISSIPEE
C   V5 :  VALEUR DE L'ENERGIE DISSIPEE (V4*GC)
C   V6 :  ENERGIE RESIDUELLE COURANTE 
C        (NULLE POUR CE TYPE D'IRREVERSIBILITE)
C   V7 A V9 : VALEURS DE DELTA

      IF (RESI) THEN
      
        KAP = MIN( MAX(KA,LBD*TNO) , DC )
        GAP = KAP/DC * (2 - H*KAP/SC)
        GAP = MAX(0.D0,GAP)
        GAP = MIN(1.D0,GAP)

        VIP(1) = KAP
        VIP(2) = REGIME        

        IF (KAP.EQ.0.D0) THEN
          VIP(3) = 0.D0
        ELSEIF (KAP.EQ.DC) THEN
          VIP(3) = 2.D0
        ELSE
          VIP(3) = 1.D0
        ENDIF

        VIP(4) = GAP
        VIP(5) = GC*VIP(4)
        VIP(6) = 0.D0       
        VIP(7) = DE(1)
        VIP(8) = DE(2)
        VIP(9) = DE(3)
                
      ENDIF


C -- MATRICE TANGENTE

      IF (RIGI) THEN      

C      AJUSTEMENT POUR PRENDRE EN COMPTE *_MECA_ELAS
        IF (ELAS) THEN
          IF (REGIME.EQ.1) REGIME = 0
        ENDIF
        
C      CALCUL DU COEFFICIENT 1D DE LA MATRICE TANGENTE
        IF (REGIME.EQ.3) THEN
          D      = 0
        ELSE IF (REGIME.EQ.0) THEN
          IF (TNO.GT.0) THEN
            D   = -LBD/TNO**2
          ELSE
            D   = 0
          ENDIF
        ELSE IF (REGIME.EQ.1) THEN
          D      = (1/(R-H) - LBD)/TNO**2
        ELSE 
          D      = 0
        ENDIF

C      MATRICE TANGENTE 3D
        CALL R8INIR(36, 0.D0, DDEDT,1)
        DO 100 I = 1,3
          DO 110 J = 1,3
            DDEDT(I,J) = D * TPO(I)*TPO(J) + LBD*PR(I,J)
 110      CONTINUE
 100    CONTINUE
 
      ENDIF

      END
