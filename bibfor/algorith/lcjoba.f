      SUBROUTINE LCJOBA (NDIM, TYPMOD, IMATE, CRIT, SUM, DSU,
     &                    VIM,OPTION, SIG, VIP,  DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/03/2005   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*8        TYPMOD(2)
      CHARACTER*16       OPTION
      INTEGER            NDIM, IMATE
      REAL*8             SUM(2), DSU(2), VIM(6)
      REAL*8             SIG(2), VIP(6), DSIDEP(2,2),CRIT(3)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDOMMAGEABLE DE LA LIAISON ACIER-BETON :
C     COUPLAGE ENTRE L'EFFORT NORMAL ET LE CISAILLEMENT
C     COMBINABLE AVEC ELAS
C
C IN : 
C     NDIM    : DIMENSION DE L'ESPACE
C     TYPMOD  : TYPE DE MODELISATION
C     IMATE   : NATURE DU MATERIAU
C     SUM     : SAUT DE DEFORMATION EN T-
C     DSU     : INCREMENT DE SAUT DE DEFORMATION
C     VIM     : VARIABLES INTERNES EN T-
C     OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_TANG ->     DSIDEP
C                 FULL_MECA      -> SIG DSIDEP VIP
C                 RAPH_MECA      -> SIG        VIP
C OUT :
C     SIG     : CONTRAINTE SIG(1)= = SIG_N, SIG(2)=SIG_T
C     VIP     : VARIABLES INTERNES
C       1   -> VALEUR DE L'ENDOMMAGEMENT DIRECTION NORMALE
C       2   -> VALEUR DE L'ENDOMMAGEMENT DIRECTION TANGENTIELLE
C       3   -> VALEUR DE L'ECROUISSAGE ISOTROPE EN REGION 1
C       4   -> VALEUR DE L'ECROUISSAGE ISOTROPE EN REGION 2
C       5   -> DEFORMATION PAR FROTTEMENT DES FISSURES
C       6   -> VALEUR DE L'ECROUISSAGE CINEMATIQUE 
C              PAR FROTTEMENT DES FISSURES
C     DSIDEP  : MATRICE TANGENTE
C
C ON A BESOIN DE 
C   HPEN   = PENETRATION 
C   GAMD0  = DEFORMATION TANGENTIELLE SEUIL ELASTIQUE REGION 1
C   AD1    = CONSTANTE A D'ENDOMMAGEMENT TANGENTIEL REGION 1
C   BD1    = CONSTANTE B D'ENDOMMAGEMENT TANGENTIEL REGION 1
C   GAMD2  = DEFORMATION TANG SEUIL COALESCENCE DES FISSURES REGION 2
C   AD2    = CONSTANTE A D'ENDOMMAGEMENT TANGENTIEL REGION 2
C   BD2    = CONSTANTE B D'ENDOMMAGEMENT TANGENTIEL REGION 2
C   VIFRO  = COEF INTERNE DE FROTTEMENT DES FISSURES
C   FA     = COEF ECROUISSAGE CINEMATIQUE EN FROTTEMENT DES FISSURES
C   FC     = PARAMETRE DE CONTROL DU CONFINEMENT
C   EPSTR0 = DEFORMATION NORMALE SEUIL OUVERTURE DES FISSURES
C   ADN    = CONSTANTE A D'ENDOMMAGEMENT NORMALE
C   BDN    = CONSTANTE B D'ENDOMMAGEMENT NORMALE
C ----------------------------------------------------------------------
      LOGICAL     RIGI, RESI, CONV, TRAC, ADHER
      CHARACTER*2 CODRET(14)
      CHARACTER*8 NOMRES(14)
      INTEGER     I,J,K,L,ITER,ITEMAX
      REAL*8      SU(2),EPS(2),E,GTT,HPEN
      REAL*8      BDN, ADN, EPSTR0
      REAL*8      BD1, AD1, GAMD0,BD2, AD2, GAMD2
      REAL*8      FC,FA,VIFROT,SIGNO,I1
      REAL*8      Y0T,YIT,Y2T,FD1,FD2
      REAL*8      D0T,DF0T,Z0,ZF0,D2T,DF2T,Z2,ZF2,DFT
      REAL*8      GAMFRO,X0,TAOFRO,FINI
      REAL*8      D0N,DFN,Y0N,YIN,CONFI,EPSCO
      REAL*8      XMUL,FX,DFDS,DFDX,DPHIDS,DPHIDX,LAMDAP
      REAL*8      VALRES(14)

C ======================================================================
C                            INITIALISATION
C ======================================================================

C -- OPTION ET MODELISATION
      RIGI   = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI   = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      ITEMAX   = NINT(CRIT(1))
      ADHER  = .TRUE.
      EPSCO  = 1.0D0
      D0N    = 0.0D0
      D0T    = 0.0D0
      DF0T   = 0.0D0
      DF2T   = 1.0D0
      DFN    = 0.0D0
      DFT    = 0.0D0

C    LECTURE DES CARACTERISTIQUES ELASTIQUES
      NOMRES(1) = 'E'
      CALL RCVALA ( IMATE,' ','ELAS',1,' ',0.D0,1,
     &              NOMRES,VALRES,CODRET, 'FM')
      E     = VALRES(1)

C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
       NOMRES(1)  = 'HPEN'
       NOMRES(2)  = 'GTT'
       NOMRES(3)  = 'GAMD0'
       NOMRES(4)  = 'AD1'
       NOMRES(5)  = 'BD1'
       NOMRES(6)  = 'GAMD2'
       NOMRES(7)  = 'AD2'
       NOMRES(8)  = 'BD2'
       NOMRES(9)  = 'VIFROT'
       NOMRES(10) = 'FA'
       NOMRES(11) = 'FC'
       NOMRES(12) = 'EPSTR0'
       NOMRES(13) = 'ADN'
       NOMRES(14) = 'BDN'

       CALL RCVALA(IMATE,' ','JOINT_BA',1,' ',0.D0,14,
     &            NOMRES,VALRES,CODRET,'FM')
      HPEN   = VALRES(1)
      GTT    = VALRES(2)
      GAMD0  = VALRES(3)
      AD1    = VALRES(4)
      BD1    = VALRES(5)
      GAMD2  = VALRES(6)
      AD2    = VALRES(7)
      BD2    = VALRES(8)
      VIFROT = VALRES(9)
      FA     = VALRES(10)
      FC     = VALRES(11)
      EPSTR0 = VALRES(12)
      ADN    = VALRES(13)
      BDN    = VALRES(14)


C   INITIALISATION DES VARIABLES INTERNES

        D0N      = VIM(1)
        D0T      = VIM(2)
        Z0       = VIM(3)
        Z2       = VIM(4)
        GAMFRO   = VIM(5)
        X0       = VIM(6)

C ======================================================================
C       CALCUL DES GRANDEURS UTILES QUELQUE SOIT OPTION
C ======================================================================

C    1 - CALCUL DES DEFORMATIONS MECANIQUES
C--------------------------------------------------------

C CALCUL DU SAUT EN T+

      CALL DCOPY(2,SUM,1,SU,1)
      IF (RESI) CALL DAXPY(2,1.D0,DSU,1,SU,1)

C  - TRANSFORMATION DES SAUTS EN DEFORMATIONS

C      DEFORMATIONS : EPS(1) = EPS_N , EPS(2) = EPS_T
      CALL R8INIR(2, 0.D0, EPS,1)

      EPS(1) = SU(1)/HPEN
      EPS(2) = SU(2)/HPEN

C    DETERMINATION DE L'OUVERTURE OU FERMETURE DE L'ELEMENT
      IF (EPS(1).GT.0.D0) THEN
          TRAC  = .TRUE.
        IF (EPS(1).GT.EPSTR0) ADHER = .FALSE.
      ELSE
          TRAC  = .FALSE.
      ENDIF

C ======================================================================
C       CALCUL DES CONTRAINTES ET VARIABLES INTERNES 
C           (OPTION FULL_MECA ET RAPH_MECA - (RESI) )
C ====================================================================
      IF (RESI) THEN          

C    MATRICE DE COMPORTEMENT
        CALL R8INIR(4,0.D0,DSIDEP,1)
        DSIDEP(1,1)=E                   
        DSIDEP(2,2)=GTT                

C    CALCUL DU CONFINEMENT 
        SIGNO = DSIDEP(1,1)*EPS(1)
        I1 = SIGNO/3
        CONFI = FC*I1
        IF(CONFI .GT. 0.D0) CONFI=0.D0

C   2 -     CALCUL DE L'ENDOMMAGEMENT DANS LA DIRECTION 
C           NORMALE
C----------------------------------------------------------------

C    SEUIL D'ADHERENCE NORMALE PARFAITE 
        IF(TRAC)THEN
          Y0N = 0.5D0*DSIDEP(1,1)*(EPSTR0**2)
        ELSE
          Y0N = 0.5D0*DSIDEP(1,1)*(EPSCO**2)
        ENDIF
        
        YIN = 0.5D0*DSIDEP(1,1)*(EPS(1)**2)

C    ENDOMMAGEMENT DANS LA DIRECTION NORMALE

        IF (.NOT.ADHER) THEN

          DFN = 1.D0 - 1.D0/(1.D0+ADN*((YIN-Y0N)**BDN))

          DFT = MAX(D0T,DFN)

          GO TO 100
C...........................................................
C      REMARQUE: ENDOMMAGEMENT DE L'ADHERENCE NORMALE ET 
C                DISPARITION DE LA LIAISON DANS LA DIRECTION
C                TANGENTIELLE
C...........................................................

        ENDIF 

C   3 -     CALCUL DE L'ENDOMMAGEMENT DANS LA DIRECTION 
C           TANGENTIELLE
C----------------------------------------------------------------

C      SEUILS D'ENDOMMAGEMENT DE LA LIAISON DANS LA 
C      DIRECTION TANGENTIELLE 
        
        Y0T = 0.5D0*DSIDEP(2,2)*(GAMD0**2)
        YIT = 0.5D0*DSIDEP(2,2)*(EPS(2)**2)
        Y2T = 0.5D0*DSIDEP(2,2)*(GAMD2**2)

C      CRITERES D'ENDOMMAGEMENT

        FD1 = YIT - (Y0T + Z0)
        FD2 = YIT - (Y2T + Z2)

       IF (FD1.GT.0.D0) THEN

C         ENDOMMAGEMENT EN REGION 1

          DF0T= (SQRT(Y0T/YIT))*
     &         EXP(AD1*((SQRT(2.D0/DSIDEP(2,2))*
     &                  (SQRT(YIT)-SQRT(Y0T)))**BD1))
          ZF0  = YIT - Y0T

          IF (FD2.GT.0.D0) THEN

C         ENDOMMAGEMENT EN REGION 2

            DF2T = ABS(1.D0/(1.D0+AD2*((YIT-Y2T)**BD2)))
            ZF2  = YIT - Y2T
          ELSE
            DF2T = 1.D0
            ZF2  = Z2
          ENDIF          
          DFT  = 1.D0 - DF0T*DF2T
          DFN = D0N
        ELSE

C         PAS DE PROGRESSION DE L'ENDOMMAGEMENT

          DFT = D0T
          ZF0  = Z0
          ZF2  = Z2
          DFN = D0N
        ENDIF
 

C   4 -     CALCUL DE LA CONTRAINTE PAR FROTTEMENT DES FISSURES
C----------------------------------------------------------------

        IF (DFT .GT. 0.D0) THEN

C         CALCUL DE LA CONTRAINTE PAR FROTTEMENT DES FISSURES


           TAOFRO = DSIDEP(2,2)*DFT*(EPS(2)-GAMFRO)
           FINI = ABS(TAOFRO-X0) + CONFI       

           IF (FINI.GT.0.D0) THEN
              CONV = .FALSE.
              DO 40 K= 1,ITEMAX 
                TAOFRO = DSIDEP(2,2)*DFT*(EPS(2)-GAMFRO)
                IF ((TAOFRO-X0).GE.0.D0) THEN
                   XMUL = +1.D0
                ELSE
                   XMUL = -1.D0
                ENDIF

                FX     = ABS(TAOFRO-X0) + CONFI
                DFDS   = XMUL
                DFDX   = -1.D0*XMUL
                DPHIDS = XMUL
                DPHIDX = -1.D0*XMUL+FA*X0

C --------EVALUATION DU MULTIPLICATEUR PLASTIQUE             

                LAMDAP = FX/(DFDS*DSIDEP(2,2)*DFT*DPHIDS+
     &                       DFDX*VIFROT*DPHIDX)
                GAMFRO  = GAMFRO + LAMDAP*DPHIDS
                X0      = X0 - VIFROT*LAMDAP*DPHIDX
                TAOFRO = TAOFRO - LAMDAP*DSIDEP(2,2)*DFT*DPHIDS
                FX      = ABS(TAOFRO-X0) + CONFI

C --------EVALUATION DE LA CONVERGENCE
                CONV    = ((ABS(FX/FINI) .LE. 0.D0) .OR.
     &                    (LAMDAP .LE. CRIT(3)))
               IF(CONV) GO TO 100 
40            CONTINUE
           
              IF (.NOT. CONV) THEN
                 CALL UTMESS('F','LCJOBA','PAS DE CONVERGENCE DANS LE
     &                       CALCUL IMPLICITE, FROTTEMENT DES FISSURES')
              ENDIF
           ENDIF
        ENDIF

100     CONTINUE

C   5 -  CALCUL DES CONTRAINTES REELLES
C----------------------------------------------------------------	

        CALL R8INIR(2, 0.D0, SIG,1)

        SIG(1)=DSIDEP(1,1)*(1.D0-DFN)*EPS(1)
        SIG(2)=DSIDEP(2,2)*(1.D0-DFT)*EPS(2)
        TAOFRO=DSIDEP(2,2)*DFT*(EPS(2)-GAMFRO)
        IF(TRAC .AND. (.NOT.ADHER)) TAOFRO=0.D0
        SIG(2)=SIG(2)+TAOFRO
      
      END IF   

C    6 -   MISE A JOUR DES VARIABLES INTERNES
C ------------------------------------------------------------
          
        VIP(1) = DFN
        VIP(2) = DFT
        VIP(3) = ZF0
        VIP(4) = ZF2
        VIP(5) = GAMFRO
        VIP(6) = X0

C ======================================================================
C     CALCUL  DE LA MATRICE TANGENTE DSIDEP
C         OPTION RIGI_MECA_TANG ET FULL_MECA  (RIGI)
C    (PAR SIMPLICITE ON NE CALCULE QUE LA MATRICE SECANTE)
C ======================================================================
      IF (RIGI)  THEN 

C   1 -  CONTRIBUTION ELASTIQUE
C ------------------------------------------------------------     	  
              
        CALL R8INIR(4, 0.D0, DSIDEP,1)

        DSIDEP(1,1)=E*(1.D0-DFN)   
        DSIDEP(2,2)=GTT*(1.D0-DFT) 

      ENDIF

C   2 -  TRANSFORMATION DES DIMENSIONS POUR UTILISER DANS 
C        L ELEMENT JOINT
C ------------------------------------------------------------     	  

      IF (RIGI)  THEN 
        DSIDEP(1,1)= DSIDEP(1,1)/HPEN   
        DSIDEP(2,2)= DSIDEP(2,2)/HPEN
      ENDIF
 
      END
