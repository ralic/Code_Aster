      SUBROUTINE NM3DCO(NDIM,OPTION,IMATE,TM,TP,E,SIGM,
     &             EPSM,DEPS,VIM,DEFAM,DEFAP,SIGP,VIP,DSIDEP,
     &                                  CORRM,CORRP)
C MODIF MODELISA  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
C TOLE CRP_20
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION

C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------

      IMPLICIT NONE
C ----------------------------------------------------------------------
C          LOI DE L'ACIER SOUMIS A LA CORROSION 3D


C IN  T        : TEMPERATURE PLUS
C IN  TM       : TEMPERATURE MOINS
C IN  E        : MODULE D EG
C IN  ET       : PENTE D ECROUISSAGE
C IN  ALPH     : COEF DILAT THERMIQUE
C IN  SY       : LIMITE D ELASTICITE INITIALE
C IN  CORRM   : CORROSION A L'INSTANT MOINS
C IN  CORRP   : CORROSION A L'INSTANT PLUS
C IN  SIGM    : CONTRAINTE AU TEMPS MOINS
C               UTILISE UNIQUEMENT POUR EVALUER DSDEM
C IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION TOTALE MOINS
C IN  INCDFFP   : DEFORMATION  PLASTIQUE MOINS
C IN  ECUM      : DEFORMATION  PLASTIQUE CUMULEE MOINS

C OUT SIGP     : CONTRAINTES PLUS
C OUT EPSP    : DEFORMATION  PLASTIQUE PLUS
C OUT P       : DEFORMATION  PLASTIQUE CUMULEE PLUS
C OUT DSIDEP    : DSIG/DEPS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      REAL*8 TP,TM,EM,EP,ET,ALPHAM,ALPHAP,TREF
      REAL*8 SIGM(6),DEPS(6),VIM(*),VIP(*),RESU,CORRM,CORRP
      REAL*8 SIGP(6),DSIDEP(6,6),RBID,RESI,EPSM(6)
      CHARACTER*16 OPTION
      INTEGER IMATE,IRET
C     ----------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C     DECLARATION DES VARIABLES 


      INTEGER ITER,ITEMAX,NBPAR,NDIM,NDIMSI,I,J,K,L,M

      REAL*8  TC,ECUMC,ECUMD,VAR1,VAR2,VAR3,RV,NU,LIMIT,ECUMM,
     &                RINI,J2,CRIT0,ECUM,DCOEF,TERME1,TERME5,YOUNG,
     &                  KCOEF,MCOEF,TER11,DELTAP,DP,CRIT,CRITEN,
     &                  COEFDC,CRIT2,CRIT0D,DEFE,DEFC,TER11A,TREPS1,
     &                  TREPS2

      REAL*8  DEFAP(6),TERME2(6),TERME4(6),VALPAR,SIGFI(6),
     &        SIGD(6),DEFAM(6),IDENT(6,6),ECOEF(6,6),TERMEF(6)

      REAL*8  SIG0(6),DSIGT(6),SIGF(6),DEFP(6),TREPS,DEFT(6),
     &                DEFPM(6),DEFPC(6),COEF1,COEF2,E,PLAS

      CHARACTER*2  FB2,CODRES
      REAL*8       CMAT(7),KCI,DRDP,CP,DDDP,CP1,HP,
     &             LAMDA,DEUMU,BCOEF,CCOEF,ACOEF
      CHARACTER*8  NOMPAR,NOMRES,NOMECL(2)
      LOGICAL      DCONV,PCONV

      FB2 = 'FM'                        
      NDIMSI=2*NDIM
      NBPAR = 1
      NOMPAR = 'TEMP'
      
      ECUMM = VIM(1)
      DCOEF  = VIM(2)
      PLAS = VIM(3)
      
C --- CARACTERISTIQUES ECROUISSAGE LINEAIRE


      VALPAR = TP
      CALL RCVALA(IMATE,' ','CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'D_CORR',
     &              COEFDC,CODRES,FB2)
      CALL RCVALA(IMATE,' ','CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'ECRO_K',
     &              KCOEF,CODRES,FB2)
      CALL RCVALA(IMATE,' ','CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'ECRO_M',
     &              MCOEF,CODRES,FB2)
      CALL RCVALA(IMATE,' ','CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'SY',
     &              LIMIT,CODRES,FB2)
      CALL RCVALA(IMATE,' ','ELAS',NBPAR,NOMPAR,VALPAR,1,'NU',
     &              NU,CODRES,FB2)
       CALL RCVALA(IMATE,' ','ELAS',NBPAR,NOMPAR,VALPAR,1,'E',
     &              YOUNG,CODRES,FB2)  
     
        TC = CORRM       
C --- PARAMETRES DE CONVERGENCE
      CALL GETVR8('CONVERGENCE','RESI_INTE_RELA',1,1,1,RESI,IRET)
      CALL GETVIS('CONVERGENCE','ITER_INTE_MAXI',1,1,1,ITEMAX,IRET)
  

C     CALCUL DE LA DEFORMATION CRITIQUE

      DEFE = LIMIT/YOUNG

      IF (TC .LE. 15.D0)  THEN
        DEFC = 2.345D-01-(1.11D-02*TC)
      ELSE 
        DEFC = 5.1D-02-(6.D-04*TC)
      END IF
C       END IF
             
      DEFPC(1) = DEFC
      DEFPC(2) = -1.D0*((-0.5D0 +((DEFE/DEFC)*(0.5D0-NU)))*DEFC)
      DEFPC(3) = -1.D0*((-0.5D0+((DEFE/DEFC)*(0.5D0-NU)))*DEFC)
       
      DO 50 I=4,NDIMSI
        DEFPC(I) = 0.D0
 50   CONTINUE
          
C     CALCUL DE LA DEFORMATION PLAST EQUIV CRITIQUE  
         
      ECUMC = DEFPC(1) * DEFPC(1)
      ECUMC = ECUMC + DEFPC(2) * DEFPC(2)
      ECUMC = ECUMC + DEFPC(3) * DEFPC(3)
      ECUMC = ECUMC + 2.D0 * DEFPC(4) * DEFPC(4)
      ECUMC = ECUMC + 2.D0 * DEFPC(5) * DEFPC(5)
      ECUMC = ECUMC + 2.D0 * DEFPC(6) * DEFPC(6)
      ECUMC = (2.D0 / 3.D0) * ECUMC
      ECUMC = ECUMC ** 0.5D0 


      
C     CALCUL DE DEFORMATION PLASTIQUE EQUIV DE DEBUT D'ENDOMMAGMENT
      
      ECUMD = 0.8D0*ECUMC

C     CALCUL DE TRIAXIALITE
      
      VAR1 = 1.D0+NU 
      VAR2 = 1.D0-(2.D0*NU)
      VAR3 = ((1.D0/3.D0)**2.D0)
      RV = (((2.D0/3.D0)*VAR1)+(3.D0*VAR2*VAR3))

C     PARAMETRES COEF2-LAMBDA ET COEF1-2MU

      COEF1 = (YOUNG / (1.D0 + NU))
      COEF2 = (NU * YOUNG) / ( (1.D0 + NU) * (1.D0 - (2.D0 * NU)) )
C     DEFORMATION PLASTIQUE A L'INSTANT M 
      DO 55 I =1,NDIMSI
        DEFAP(I) = DEFAM(I)
 55   CONTINUE 
      DO 60 I =1,NDIMSI
        DEFT(I) =EPSM(I)
 60   CONTINUE  
      DO 65 I=1,NDIMSI
        SIGP(I) = SIGM(I)
 65   CONTINUE 
      DO 70 I=1,NDIMSI
        SIGFI(I)=SIGP(I)
 70   CONTINUE     
      IF ( OPTION(1:9).EQ.'FULL_MECA' .OR. 
     &      OPTION(1:9).EQ.'RAPH_MECA' ) THEN      
C     DEFORMATION TOTALE A L'INSTANT P                
        DO  75 I = 1,NDIMSI
          DEFT(I)=DEFT(I)+DEPS(I)
 75     CONTINUE 

      ELSE
                  
C     DEFORMATION TOTALE A L'INSTANT M      
 
        DO 80 I =1,NDIMSI
          DEFT(I) =EPSM(I)
 80     CONTINUE 
      ENDIF
      
C     DEFORMATION PLASTIQUE EQUIV A L'INSTANT M
          
      ECUM = ECUMM
      

      DCONV=.FALSE.

      ITER = 0

C     ELASTICITE

      IF (.NOT. DCONV) THEN         
      
        ITER = ITER + 1

        
C     CALCUL DES CONTRAINTES ELASTIQUES

        TREPS = DEPS(1)+DEPS(2)+DEPS(3)   
        DO 85 I=1,NDIMSI
          SIGP(I) = SIGP(I)+COEF1*DEPS(I)
 85    CONTINUE          
        DO 90 I=1,3
          SIGP(I) = SIGP(I)+COEF2*TREPS
 90    CONTINUE   

        DP = 0.D0
C      CALCUL DE J2(SIG)

        J2 = SIGP(1)** 2
        J2 = J2 + (SIGP(2)** 2)
        J2 = J2 + (SIGP(3)** 2)
        J2 = J2 - ((1.D0 / 3.D0) * 
     &           ((SIGP(1) + SIGP(2) + SIGP(3)) ** 2)) 
        J2 = J2 + ( 2.D0 * (SIGP(4)** 2))
        J2 = J2 + ( 2.D0 * (SIGP(5)** 2))
        J2 = J2 + ( 2.D0 * (SIGP(6)** 2))
        J2 = (( 3.D0 / 2.D0 ) ** 0.5D0) * ( J2 ** 0.5D0 )        
C      CALCUL D'ECROUISSAGE            
        RINI = KCOEF*(ECUM**(1.D0/MCOEF))
                                          
C       SURFACE SEUIL                    
        CRIT0 = ( (J2/(1.D0-DCOEF)) - RINI - LIMIT )    
        CRIT=CRIT0                        
        IF ( OPTION(1:9).EQ.'FULL_MECA' .OR. 
     &      OPTION(1:9).EQ.'RAPH_MECA' ) THEN

          IF (CRIT0.LT.0.D0 ) THEN
          
              PLAS=0.D0
              VIP(3)=PLAS         
              DCONV = .TRUE.
              DP = 0.D0
          ELSE   
              PLAS=1.D0
              VIP(3)=PLAS
              PCONV = .FALSE.        

C     PLASTICITE

              IF (.NOT. PCONV)  THEN 
         
C     TERME1 : F(SIG,R)   

                J2 = ( SIGP(1)** 2)
                J2 = J2 + (SIGP(2)** 2)
                J2 = J2 + (SIGP(3)** 2)
                J2 = J2 - ((1.D0 / 3.D0) * 
     &             ((SIGP(1) + SIGP(2) + SIGP(3)) ** 2)) 
                J2 = J2 + ( 2.D0 * (SIGP(4)** 2))
                J2 = J2 + ( 2.D0 * (SIGP(5)** 2))
                J2 = J2 + ( 2.D0 * (SIGP(6)** 2))
                J2 = (( 3.D0 / 2.D0 ) ** 0.5D0) * ( J2 ** 0.5D0 ) 
                   
                TERME1 = ( (J2/(1.D0-DCOEF)) - RINI - LIMIT )

C     TERME2(*) : DF(SIG,X,R) / DSIG 

                TERME2(1) = ( 1.D0 / (J2*(1.D0-DCOEF))) * ( SIGP(1) -
     &                       ( 0.5D0 * SIGP(2)) -
     &                       ( 0.5D0 * SIGP(3) ) )
C
                TERME2(2) = ( 1.D0 / (J2*(1.D0-DCOEF))) * ( SIGP(2) -
     &                       ( 0.5D0 * SIGP(1))-
     &                       ( 0.5D0 * SIGP(3)) )
C 
                TERME2(3) = ( 1.D0 / (J2*(1.D0-DCOEF)) ) * (SIGP(3) -
     &                       ( 0.5D0 * SIGP(1)) -
     &                       ( 0.5D0 * SIGP(2)) )
C                
                TERME2(4) = ( 1.D0 / (J2*(1.D0-DCOEF)) ) *
     &                       ( 1.5D0 * SIGP(4) )
C 
                TERME2(5) = ( 1.D0 / (J2*(1.D0-DCOEF)) ) * 
     &                       ( 1.5D0 * SIGP(5) )
C
                TERME2(6) = ( 1.D0 / (J2*(1.D0-DCOEF)) ) * 
     &                            ( 1.5D0 * SIGP(6) )

C     TERME3(*) : DF(SIG,X,R) / DSIG = DF(SIG,X,R) / DSIG 

C     TERME4(*) : KE * TERME2 

                TERME4(1) = (COEF1 * TERME2(1)) + (COEF2 * 
     &          (TERME2(1) + TERME2(2) + TERME2(3)))
C
                TERME4(2) = (COEF1 * TERME2(2)) + (COEF2 * 
     &          (TERME2(1) + TERME2(2) + TERME2(3)))
C
                TERME4(3) = (COEF1 * TERME2(3)) + (COEF2 * 
     &          (TERME2(1) + TERME2(2) + TERME2(3)))
C
                TERME4(4) = COEF1 * TERME2(4)
C
                TERME4(5) = COEF1 * TERME2(5)
C
                TERME4(6) = COEF1 * TERME2(6)
C
C     TERME5 = TERME2 : TERME4 

C
                TERME5 = TERME2(1) * TERME4(1)
                TERME5 = TERME5 + (TERME2(2) * TERME4(2))
                TERME5 = TERME5 + (TERME2(3) * TERME4(3))
                TERME5 = TERME5 + ( 2.D0 * TERME2(4) * TERME4(4) )
                TERME5 = TERME5 + ( 2.D0 * TERME2(5) * TERME4(5) )
                TERME5 = TERME5 + ( 2.D0 * TERME2(6) * TERME4(6) )
 
C     TER11 : DF/DR*COEFFIC      

                TER11 = (LIMIT/KCOEF)

                TER11 = (((J2)/(KCOEF*(1.D0-DCOEF))) - TER11)

                TER11 = (TER11**(1.D0-MCOEF))

                TER11 = (KCOEF/(MCOEF))*TER11

                TER11 = (-1.D0) * TER11  


C     DETERMINATION DE DELTAP 

                DELTAP = (TERME1 / ( TERME5 - TER11))
                                                                    
C      CALCUL  DE TOUTES LES VARIABLES INTERNES : 
 
 
                DO 95 I = 1,NDIMSI
                  SIGP(I) = SIGP(I) - (DELTAP * TERME4(I))
 95             CONTINUE   
                  
C     DETERMINATION DE LA DEFORMATION PLASTIQUE ET P                 
                  DO 100 I = 1,NDIMSI
                  DEFAP(I) = DEFAP(I) + (DELTAP * TERME2(I))         
 100              CONTINUE         
                  DP = DEFAP(1) * DEFAP(1)
                  DP = DP + DEFAP(2) * DEFAP(2)
                  DP = DP + DEFAP(3) * DEFAP(3)
                  DP = DP + 2.D0 * DEFAP(4) * DEFAP(4)
                  DP = DP + 2.D0 * DEFAP(5) * DEFAP(5)
                  DP = DP + 2.D0 * DEFAP(6) * DEFAP(6)
                  DP = (2.D0 / 3.D0) * DP
                  DP = DP ** 0.5D0 

                  ECUM = ECUM + DP
      
C     CALCUL DE J2(SIG)

                 J2 = ( SIGP(1) ** 2)
                 J2 = J2 + (SIGP(2)** 2)
                 J2 = J2 + (SIGP(3)** 2)
                 J2 = J2 - ((1.D0 / 3.D0) * 
     &             ((SIGP(1) + SIGP(2) + SIGP(3)) ** 2)) 
                 J2 = J2 + ( 2.D0 * (SIGP(4)** 2))
                 J2 = J2 + ( 2.D0 * (SIGP(5)** 2))
                 J2 = J2 + ( 2.D0 * (SIGP(6)** 2))
                 J2 = (( 3.D0 / 2.D0 ) ** 0.5D0) * ( J2 ** 0.5D0 )  

C     DETERMINATION DE L'ECROUISSAGE

                 TER11A = (LIMIT/KCOEF)
                 TER11A = (((J2)/(KCOEF*(1.D0-DCOEF))) - TER11A)
                 TER11A = (TER11A**(1.D0-MCOEF))
                 TER11A = (KCOEF/(MCOEF*(1.D0-DCOEF)))*TER11A
C 
                 RINI = RINI +(TER11A*DELTAP/(1.D0-DCOEF))
C     DEFORMATION PLASTIQUE

                 DEFP(1) = DEFAP(1)
                 DEFP(2) = DEFAP(2)
                 DEFP(3) = DEFAP(3)
                 DEFP(4) = DEFAP(4)
                 DEFP(5) = DEFAP(5)
                 DEFP(6) = DEFAP(6)             
C     SURFACE SEUIL

                CRIT = ((J2/(1.D0-DCOEF))- RINI - LIMIT )

                PCONV = ((ABS(CRIT/CRIT0) .LE. RESI)
     &               .OR. (DELTAP .LE. RESI))
            END IF

          END IF
        
C     CRITERE D'ENDOMMAGEMENT
         
          CRITEN = ECUM - ECUMD

           IF (CRITEN.LE.0.D0) THEN   
        
               DCONV = .TRUE.
          
           ELSE
       
C     COEFFICIENT D'ENDOMMAGEMENT
 
             DCOEF = (COEFDC*((RV*ECUM)-ECUMD))/(ECUMC-ECUMD)
 
             CRIT2 = ((J2/(1.D0-DCOEF))- RINI - LIMIT )

             IF (ITER .EQ. 1) THEN
         
                 CRIT0D = CRIT2

             ELSE       
                 CRITEN = CRIT2

                 DCONV = ((ABS(CRITEN/CRIT0D) .LE. RESI)
     &             .OR. (ITER .LE. ITEMAX))     
              IF (ITER . EQ. ITEMAX) THEN
          
                  PRINT*, 'ATTENTION PAS DE CONVERGENCE'

              END IF
            END IF
          END IF

          IF (DCOEF .GT. 0.99D0) THEN
         
              DCONV = .TRUE.
              DCOEF = 0.99D0

              DO 105 I = 1,NDIMSI
                SIGP(I) = 0.D0
 105          CONTINUE  

          END IF

          VIP(1) = ECUM
          VIP(2) = DCOEF                            
          DO 110 I = 1,3
            SIGP(I) = SIGP(I)
            DEFP(I) = DEFP(I)
            DEFAP(I)=DEFAP(I)
 110      CONTINUE  

C     CHANGEMENT DE REPERE

          DO 115 I = 4, NDIMSI
            SIGP(I) = SIGP(I)
            DEFP(I) = DEFP(I)
            DEFAP(I)=DEFAP(I) 
 115      CONTINUE  
       
       END IF   
      END IF

C     CALCUL DE CP DE LA MATRICE TANGENTE

      IF ((OPTION(1:14).EQ.'RIGI_MECA_TANG').OR.
     &   (OPTION(1:9).EQ.'FULL_MECA')) THEN
     
C     CALCUL MATRICE ELASTIQUE
        DO 150 K=1,6
        DO 150 L=1,6
          DSIDEP(K,L) = 0.D0
 150    CONTINUE

        DO 160 K=1,6
          DSIDEP(K,K) = COEF1
 160    CONTINUE

        DO 170 K=1,3
        DO 170 L=1,3
          DSIDEP(K,L) = DSIDEP(K,L) + COEF2
 170    CONTINUE
C     PLASTICITE                   
        IF (PLAS.GE.0.5D0) THEN
            KCI = 1.D0
        IF (OPTION(1:14).EQ.'RIGI_MECA_TANG') THEN
          
C     CORRECTION A LA MATRICE TANGENTE ELASTIQUE

           RBID = SIGFI(1) + SIGFI(2) + SIGFI(3)
            
           DO 175 K=1,3
             SIGD(K) = SIGFI(K)- RBID * (1.D0/3.D0)
175       CONTINUE

           DO 176 K =4,NDIMSI
            SIGD(K)= SIGFI(K)           
 176    CONTINUE
         ELSE
C     CORRECTION A LA MATRICE TANGENTE ELASTIQUE

           RBID = SIGP(1) + SIGP(2) + SIGP(3)

          
           DO 177 K=1,3
             SIGD(K) = SIGP(K)- RBID*(1.D0 / 3.D0) 
177       CONTINUE
          
           DO 178 K =4,NDIMSI
            SIGD(K)= SIGP(K)             
178        CONTINUE
         END IF   

C     ECROUISSAGE DRDP       
         DRDP = (ECUM**((1.D0/MCOEF)-1.D0))

         DRDP = (KCOEF/(MCOEF))*DRDP
            
         IF (VIM(2).LE.0.D0) THEN 
        
C     PARAMETRES DE LA MATRICE PLASTIQUE SANS DCOEF
                  
            HP = (1.D0+((3.D0/2.D0)*COEF1*KCI*DP)/((RINI+LIMIT)))
                  
            LAMDA = COEF2+((COEF1/3.D0)*(1.D0-(1.D0/HP)))
          
            DEUMU = COEF1/HP

            BCOEF = (1.D0- (((DRDP*DP)/(RINI+LIMIT)
     &                       )))
            BCOEF = KCI*((9.D0*(COEF1**2))/(4.D0*HP))*BCOEF

            CCOEF = DRDP+((3.D0/2.D0)*COEF1)
         
C     CALCUL MATRICE ELASTIQUE FICTIVE

            DO 200 K=1,NDIMSI
            DO 200 M=1,NDIMSI
              DSIDEP(K,M) = 0.D0
 200        CONTINUE

            DO 210 K=1,NDIMSI
              DSIDEP(K,K) = DEUMU
 210        CONTINUE

            DO 220 K=1,3
            DO 220 M=1,3
              DSIDEP(K,M) = DSIDEP(K,M) + LAMDA
 220        CONTINUE
         
            DO 230 K=1,NDIMSI
            DO 230 M=1,NDIMSI
              DSIDEP(K,M) = (DSIDEP(K,M) -((BCOEF/CCOEF)*
     &        ((SIGD(K)/(RINI+LIMIT))
     &            *(SIGD(M)/(RINI+LIMIT)))))
 230        CONTINUE    
          
        ELSE

C     PARAMETRES DE LA MATRICE PLASTIQUE SANS DCOEF
                  
            HP = (1.D0+((3.D0/2.D0)*COEF1*KCI*DP)/
     &                      ((1.D0-DCOEF)*(RINI+LIMIT)))
                  
            LAMDA = COEF2+((COEF1/3.D0)*(1.D0-(1.D0/HP)))
          
            DEUMU = COEF1/HP

C     CALCUL ACOEF
            
            ACOEF = ((COEFDC*RV)/(ECUMC-ECUMD))

C     CALCUL BCOEF
            BCOEF = (1.D0- (DP*(((1.D0-DCOEF)*DRDP) -
     &           (RINI*ACOEF))/((1.D0-DCOEF)*(RINI+LIMIT))))
     
            BCOEF = KCI*((9.D0*(COEF1**2))/(4.D0*HP))*BCOEF

            CCOEF = (((1.D0-DCOEF)*DRDP)+((3.D0/2.D0)*COEF1)
     &                             -(RINI*ACOEF))
         
C     CALCUL MATRICE ELASTIQUE FICTIVE
            DO 240 K=1,NDIMSI
            DO 240 M=1,NDIMSI
              DSIDEP(K,M) = 0.D0
 240        CONTINUE

            DO 250 K=1,NDIMSI
              DSIDEP(K,K) = DEUMU
 250        CONTINUE

            DO 260 K=1,3
            DO 260 M=1,3
              DSIDEP(K,M) = DSIDEP(K,M) + LAMDA
 260        CONTINUE
         
            DO 270 K=1,NDIMSI
            DO 270 M=1,NDIMSI
              DSIDEP(K,M) = (DSIDEP(K,M) -((BCOEF/CCOEF)*
     &        ((SIGD(K)/((1.D0-DCOEF)*(RINI+LIMIT)))
     &            *(SIGD(M)/((1.D0-DCOEF)*(RINI+LIMIT))))))
 270        CONTINUE    

         END IF          
        END IF
       END IF
       END
