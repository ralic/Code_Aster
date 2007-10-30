      SUBROUTINE LKCOMP (NDIM,MOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,DEPS,SIGM,VINM,
     &                   OPTION,SIGP,VINP,DSIDE,RETCOM)
C
      IMPLICIT  NONE
      INTEGER            RETCOM, IMATE
      CHARACTER*8        MOD(*)
      CHARACTER*16       COMPOR(*),OPTION
      REAL*8             CRIT(*),INSTAM,INSTAP,TM,TP,TREF
      REAL*8             DEPS(6)
      REAL*8             SIGM(6),VINM(7)
      REAL*8             SIGP(6),VINP(7)
      REAL*8             DSIDE(6,6)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2007   AUTEUR ELGHARIB J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =================================================================
C --- MODELE LETK : LAIGLE ET KLEINE (CIH)  MODELE VISCOPLASTIQUE--
C =================================================================
C --- BUT : ROUTINE PRINCIPALE---------------------------
C =================================================================
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  MOD     : TYPE DE MODELISATION
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT 
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
C IN  INSTAP  : INSTANT DU CALCUL
C IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
C IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C IN  VINM    : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
C OUT VINP    : VARIABLES INTERNES A L'INSTANT ACTUEL
C OUT DSIDE   : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
C OUT RETCOM  : CODE RETOUR POUR LE REDECOUPAGE DU PAS DE TEMPS
C               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
C=======================================================================
C=======================================================================
C --- ATTENTION : CHANGEMENT DE SIGNES DES CHAMPS DE CONTRAINTES ET DES
C ----DEFORMATIONS - DANS CE MODELE CONVENTION MECANIQUE DES SOLS A L 
C ----OPPPOSE DE CELLES DE LA MECANIQUE DES MILIEUX CONTINUS - EN  
C ----COMPRESSION LA CONTRAINTE EST POSITIVE ET EN CONTRACTANCE :
C ----DEFORMATION VOLUMIQUE POSITIVE
C=======================================================================
C TOLE CRP_20
C TOLE CRP_21
C=======================================================================
      INTEGER          NBMAT, NDT, NDIM, NDI,NR,NVI,VAL,VARV,I,K,MATR
      INTEGER          DUM, IISNAN, INDAL
      REAL*8           MUN, UN, ZERO, DEUX, TROIS,  BIDON
C      REAL*8           LGLEPS
      PARAMETER       (NBMAT  = 90 )
      REAL*8           MATERD(NBMAT,2), MATERF(NBMAT,2)
      REAL*8           DT, ALPHA, COEF
      REAL*8           SIGML(6),SIGPL(6),SIGPN(6),DEPML(6),DEPSTH(6)
      REAL*8           TRACE 
      REAL*8           I1ML, SML(6), SIIM
      REAL*8           I1PL, IPL, SPL(6), SIIP
      REAL*8           IEL , I1EL, SEL1(6), SIGEL1(6), I2EL, SEL2(6)
      REAL*8           DVML, DEVML(6) 
      REAL*8           DVML1, DEVML1(6) 
      REAL*8           SEL(6), SIGEL(6), SIGI(6)
      REAL*8           PARAVI(3), VARVI(4)
      REAL*8           PARAEP(3),VARPL(4)
      REAL*8           RCOS3T, COS3T, BPRIME
      REAL*8           H0E, H0C,HTHETA, SEUILV,SEUILP
      REAL*8           DHDS(6),DS2HDS(6),DFDSV(6),DFDSP(6)
      REAL*8           GV(6), DEPSV(6), DGAMV, DXIVM, XIPIC
      REAL*8           GP(6), DEPSP(6), DGAMP, XIVM, DXIP,DXIV
      REAL*8           SEUIVM,UCRIVM,UCRIP, UCRIV,IRREV(6)
      REAL*8           DSIG(6),DERPAR(3), VECD(6),UCRIPL, SEUIPL
      REAL*8           DEDEP(6), ETA, DE(6,6),KK,MU
      REAL*8           KRON(6),  VINTR
      CHARACTER*3      MATCST
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       (MUN   = -1.D0 )
      PARAMETER       (UN    =  1.D0 )
      PARAMETER       (ZERO  =  0.D0 )
      PARAMETER       (DEUX  =  2.D0 )
      PARAMETER       (TROIS =  3.D0 )
C      PARAMETER       (LGLEPS =  1.0D-8 )
C =================================================================
      COMMON /TDIM/   NDT , NDI
C =================================================================
      DATA             KRON /UN , UN , UN , ZERO ,ZERO ,ZERO/
      
      DT = INSTAP - INSTAM

      CALL R8INIR(6,0.D0,DEPSP,1)
      CALL R8INIR(6,0.D0,DEPSV,1)
      DGAMP = ZERO
      DGAMV= ZERO
      DXIP = ZERO
      DXIV = ZERO
C =================================================================
C --- RECUPERATION DES PARAMETRES DU MODELE -----------------------
C --- LES COEFFICIENTS MATERIAU N EVOLUENT PAS AVEC LE TEMPS-------
C =================================================================

      MATCST = 'OUI'      
      CALL LKLMAT ( MOD, IMATE, NBMAT, TM, MATERD,
     &              MATERF, MATCST, NDT, NDI, NVI, INDAL)

C      SIGC   = MATERD(3,2)
      XIPIC  = MATERD(19,2)
      XIVM   = MATERD(21,2)
C =================================================================
C --- CONVENTIONS DE SIGNE DU MODELE LAIGLE VISCOPLASTIQUE --------
C =================================================================

      DO 10 I = 1, NDT
         SIGML(I) = MUN *  SIGM(I)
         DEPML(I) = MUN *  DEPS(I)
  10  CONTINUE
C =================================================================
C --- DEFINITION DES INVARIANTS ET DU DEVIATEUR -------------------
C =================================================================

      I1ML = TRACE(NDI,SIGML)

      CALL LCDEVI(SIGML,SML)

      CALL LCPRSC(SML, SML, SIIM)

      SIIM = SQRT(SIIM)

C =================================================================
C --- DEFINITION DES DEFORMATIONS VOLUMIQUES ET DEVIATORIQUES -----
C =================================================================

      ALPHA = MATERD(3,1)

         IF ((IISNAN(TP).EQ.0).AND.(IISNAN(TM).GT.0)) THEN
           IF ((IISNAN(TREF).GT.0).AND.(INDAL .EQ. 0)) THEN 
             CALL U2MESS('F','CALCULEL_31')
           ELSE
             COEF = ALPHA*(TP-TREF) - ALPHA*(TM-TREF)
           ENDIF
         ELSE
             COEF = ZERO
         ENDIF

      DVML = 0.D0

      DO 110 K=1,NDT
        DEPSTH(K) = DEPML(K)
 110  CONTINUE
      DO 111 K=1,3
        DEPSTH(K) = DEPSTH(K) - COEF
        DVML = DVML + DEPSTH(K)
 111  CONTINUE
      DO 115 K=1,NDT
        DEVML(K)   = DEPSTH(K) - DVML/3.D0 * KRON(K)
 115  CONTINUE

C =================================================================
C --- PREDICTION ELASTIQUE ----------------------------------------
C =================================================================
      CALL LKELAS ( NDI, NDT, MOD , NBMAT, MATERD, 
     &              DEPML, SIGML, VINM, DE,KK,MU)

      IEL = I1ML + TROIS*KK*DVML

      DO 20 I = 1, NDT  
         SEL(I) = SML(I) + DEUX* MU *DEVML(I)
  20  CONTINUE

      DO 30 I = 1, NDT
         SIGEL(I) = SEL(I) + IEL/TROIS*KRON(I)
  30  CONTINUE 


      IF ( OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA'     ) THEN
C =================================================================
C --- CRITERE VISQUEUX --------------------------------------------
C =================================================================
C =================================================================
C --- CALCUL DE fv(SIGE, XIVM) ---CRITERE VISQUEUX-----------------
C =================================================================
           CALL LKCRIV(XIVM,IEL,SEL, VINM, NBMAT,MATERD,UCRIVM,SEUIVM)

           IF (UCRIVM.LT.ZERO)  CALL U2MESS('F','COMPOR1_27')
           
C --- VERIFICATION DU SEUIL VISQUEUX MAX ---------------------------
C --- CES 4 TESTS SONT NECESSAIRE POUR PRENDRE EN COMPTE LES SITUATIONS 
C --- AVANT ET APRES LE PIC-----------------------------------------

C---- VINTR : TROISIEME VARIABLE INTERNE QUI VAUT XIVMAX AU DELA DU -
C------------VISCOPLASTIQUE MAX-------------------------------------
C---- VARV : EN DESSOUS DU CRITERE VISQUEUX MAX : CONTRACTANCE: VARV=0
C---- VARV : AU DESSUS DU CRITERE VISQUEUX MAX  : DILATANCE:    VARV=1

C---- VAL  : INDICATEUR POUR LES LOIS DE DILALANCE
C----      : EN DESSOUS DU PIC : VAL = 0
C----      : AU DESSUS DU PIC  : VAL = 1

           IF ((SEUIVM .LT. ZERO).AND.(VINM(1).LT.XIPIC)) THEN 
            VINTR = VINM(3)
            VARV = 0
           ELSEIF ((SEUIVM .GT. ZERO).AND.(VINM(1).LT.XIPIC)) THEN
            VINTR = XIVM
            VARV = 1       
           ELSEIF ((SEUIVM .GT. ZERO).AND.(VINM(1).GT.XIPIC)) THEN
            VINTR = XIVM
            VARV = 1
           ELSEIF ((SEUIVM .LT. ZERO).AND.(VINM(1).GT.XIPIC)) THEN
            VINTR = XIVM
            VARV = 0        
           ENDIF
           
           CALL LKCRIV(VINTR,IEL,SEL,VINM,NBMAT,MATERD,UCRIV,SEUILV)

C =================================================================
C --- VERIFICATION SUR L'AXE HYDROSTATIQUE ------------------------
C =================================================================

           IF (UCRIV .LT. ZERO) THEN 

              CALL LKVARP(VARV,DGAMV, VINM, NBMAT,  MATERD, PARAEP)
              
              CALL LKVACP(NBMAT, MATERD, PARAEP, VARPL)     
              
              CALL LKVARV(VINTR,NBMAT, MATERD, PARAVI)
              
              CALL LKVACV(NBMAT, MATERD, PARAVI, VARVI)     
              
           IF ((-(VARVI(3)/VARVI(2))).LT.(-(VARPL(3)/VARPL(2)))) THEN
                RETCOM = 1
                GOTO 1000
                
                  ELSE
                CALL U2MESS('F','COMPOR1_28')
                 
           ENDIF
         
           ENDIF  
           
C =================================================================
C --- PAS DE VISCOSITE  -------------------------------------------
C =================================================================
           IF (SEUILV.LT.ZERO) THEN
           VAL  = 0
           DGAMV = ZERO
           DXIV  = ZERO
           DVML1 = ZERO
           
           DO 31 I = 1,NDT
              DEPSV(I)  = ZERO 
              DEVML1(I) = ZERO
 31        CONTINUE

C---- XIV A T + DT ------------------------------------------------

           VINP(3) = VINM(3) 

C---- GAMMAV A T + DT ---------------------------------------------
                 
           VINP(4) = VINM(4)
           
C --  INDICATEUR DE VISCOSITE
           VINP(6) = 0.D0 

            

           ELSE
C =================================================================
C --- VISCOSITE  --------------------------------------------------
C =================================================================
               VAL = 0
                        
C -------------CALCUL DE d GAMMAV -----------CRITERE VISQUEUX------
               CALL LKDGDE(VAL,VINTR,DT,IEL,SEL,I1ML,SML,VINM,
     &                     NBMAT,MATERD,DEPSV,DGAMV)

               DVML1 = TRACE(NDI,DEPSV)
               CALL LCDEVI(DEPSV,DEVML1)
               
               
C -------------DELTA XIV 
               DXIVM = XIVM - VINM(3)
               DXIV  = MIN(DGAMV,DXIVM)
               
C---- XIV A T + DT ------------------------------------------------

               VINP(3) = VINM(3) + DXIV

C---- GAMMAV A T + DT ---------------------------------------------
                 
               VINP(4) = VINM(4) + DGAMV
               
C --  INDICATEUR DE VISCOSITE
               VINP(6) = 1.D0

          ENDIF
          
C --- MISE A JOUR DE LA PREDICTION DE LA CONTRAINTE ---------------

              I1EL = IEL - TROIS*KK*DVML1

              DO 22 I = 1, NDT  
              SEL1(I) = SEL(I) - DEUX* MU *DEVML1(I)
 22           CONTINUE

C =================================================================
C --- CRITERE ELASTOPLASTIQUE  ------------------------------------
C =================================================================
C =================================================================
C --- CALCUL DE fp(SIGE, XIPM) ---CRITERE ELASTOPLASTIQUE ---------
C =================================================================
           CALL LKCRIP(VARV,DGAMV,I1EL,SEL1,VINM,NBMAT,MATERD,UCRIP,
     &                 SEUILP)
           IF (UCRIP  .LT. ZERO) THEN
           RETCOM = 1
           GOTO 1000
           ENDIF 

C==================================================================
C--------- ELASTICITE ---------------------------------------------
C==================================================================
           IF (SEUILP.LT.ZERO) THEN
           DGAMP = ZERO          

              DO 35 I = 1,NDT
              DEPSP(I) = ZERO  
  35          CONTINUE

C---- REACTUALISATION DES CONTRAINTES -----------------------------

              DO 23 I = 1, NDT  
              SIGEL(I) = SEL1(I) + I1EL/TROIS*KRON(I)
              SIGPL(I)  = SIGEL(I)
  23          CONTINUE

C -------- DELTA XIP 
           
             IF (VARV.EQ.0) THEN 

C--------- CONTRACTANCE
C---------- ELASTICITE EN DESSOUS DU CRITERE VISQUEUX MAX
             DXIP = ZERO
             VINP(5) = 0.0D0

             ELSEIF(VARV.EQ.1) THEN

C -------- DILATANCE
C---------- ELASTICITE EN DESSUS DU CRITERE VISQUEUX MAX

             DXIP = DGAMV
             VINP(5) = 1.0D0

             ENDIF

C---- XIP A T + DT ------------------------------------------------

              VINP(1) = VINM(1) + DXIP

C---- GAMMAP A T + DT ---------------------------------------------

              VINP(2) = VINM(2)
              
C --  INDICATEUR DE PLASTICITE
              VINP(7) = 0.D0

           ELSE
C =================================================================
C -------- PLASTIFICATION -----------------------------------------
C =================================================================
           IF (VINM(1).LT.XIPIC) THEN 
            VAL  = 0
            ELSE
            VAL  = 1
           ENDIF

C ------- CALCUL DE d GAMMAP -------------CRITERE ELASTOPLASTIQUE--
            CALL LKGAMP(VAL,VARV,I1ML,SML,VINM,NBMAT,
     &           MATERD,DE,DEPML,DEPSV,DGAMV,DEPSP,DGAMP)

             
C -------- DELTA XIP 
           
             IF (VARV.EQ.0) THEN 

C--------- CONTRACTANCE
C--------- PLASTIFICATION ET EN DESSOUS DU CRITERE VISQUEUX MAX

             DXIP = DGAMP
             VINP(5) = 0.0D0

             ELSEIF(VARV.EQ.1) THEN

C -------- DILATANCE
C--------- PLASTIFICATION ET EN DESSUS DU CRITERE VISQUEUX MAX

             DXIP = DGAMP + DGAMV
             VINP(5) = 1.0D0

             ENDIF
C =================================================================
C --- REACTUALISATION DES CONTRAINTES  ----------------------------
C =================================================================
C --- DEFORMATIONS IRREVERSIBLES ----------------------------------

             CALL LCSOVE (DEPSV, DEPSP, IRREV )

             CALL LCDIVE (DEPML, IRREV, VECD)
           
             CALL LCPRMV (DE, VECD, DSIG)

             DO 40 I = 1,NDT
              SIGPL(I) = SIGML(I) + DSIG(I)
   40        CONTINUE

C =================================================================
C --- DEFINITION DES INVARIANTS ET DU DEVIATEUR -------------------
C =================================================================

C             I1PL = TRACE(NDI,SIGPL)
           
C             CALL LCDEVI(SIGPL,SPL)

C             CALL LCPRSC(SPL, SPL, SIIP)

C             SIIP = SQRT(SIIP)

C==================================================================
C--------- REACTUALISATION DES VARIABLES INTERNES VISQUEUSES ------
C==================================================================
C---- XIP A T + DT ------------------------------------------------

             VINP(1) = VINM(1) + DXIP

C---- GAMMAP A T + DT ---------------------------------------------

             VINP(2) = VINM(2) + DGAMP

C --  INDICATEUR DE PLASTICITE
           
             VINP(7) = 1.D0
C =================================================================
C --- AJUSTEMENT DES CONTRAINTES POUR ANNULER LE CRITER PLASTIQUE -
C =================================================================
C           CALL LKVARV(VINTR,NBMAT, MATERD, PARAVI)

           CALL LKVARP(VARV,DGAMV, VINP, NBMAT, MATERD, PARAEP)

C           RCOS3T = COS3T  (SPL, MATERD(1,2), LGLEPS)
        
C          CALL LKVACP(NBMAT, MATERD, PARAEP, VARPL)   

C           CALL LKHTET(NBMAT, MATERD, RCOS3T, H0E, H0C, HTHETA)

C          ETA = ((SIIP * HTHETA / SIGC / H0C)**(UN/PARAEP(1)) -
C     &      VARPL(1)*SIIP*HTHETA - VARPL(3))/
C     &      VARPL(2)/I1PL
C         write(6,*) 'ETA', ETA
C           ETA = UN
C           IPL = I1PL * ETA
        
C          DO 45 I = 1, NDT
C           SIGPN(I) = SPL(I) + IPL/TROIS*KRON(I)
C           SIGPL(I) = SIGPN(I)
C  45    CONTINUE
        
C          DUM=0 
C           CALL LKCRIP(DUM,DGAMV, IPL,SPL,VINP,NBMAT,MATERD,UCRIPL,
C     &                  SEUIPL)
C           IF (UCRIPL  .LT. ZERO) THEN
C       CALL U2MESS('F','COMPOR1_17')
C           write (6,*) 'COND 4 UCRIPL ',UCRIPL
C           RETCOM = 1
C           GOTO 1000
C           ENDIF 

           ENDIF
            
      ENDIF
            
C =================================================================
C --- TERMES DE L OPERATEUR TANGENT -------------------------------
C =================================================================
      IF (OPTION(11:14).EQ.'ELAS') THEN
          CALL LKELAS ( NDI, NDT, MOD , NBMAT, MATERD, 
     &              DEPML, SIGML, VINM, DE,KK,MU)
          CALL LCEQMA(DE, DSIDE)
      ENDIF
      IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG'.OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA'         ) THEN
       IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG' ) THEN
           IF ((VINM(7) .EQ. 0.D0).AND.(VINM(6) .EQ. 0.D0)) THEN
            MATR = 0
           ELSEIF ((VINM(7) .EQ. 0.D0).AND.(VINM(6) .NE. 0.D0)) THEN
            MATR = 3
           ELSEIF ((VINM(7) .NE. 0.D0).AND.(VINM(6) .EQ. 0.D0)) THEN
            MATR = 2
           ELSEIF ((VINM(7) .NE. 0.D0).AND.(VINM(6) .NE. 0.D0)) THEN
            MATR = 1 
           END IF
       END IF
       IF ( OPTION(1:9) .EQ. 'FULL_MECA' ) THEN
           IF ((VINP(7) .EQ. 0.D0).AND.(VINP(6) .EQ. 0.D0)) THEN
            MATR = 0
           ELSEIF ((VINP(7) .EQ. 0.D0).AND.(VINP(6) .EQ. 1.D0)) THEN
            MATR = 3
           ELSEIF ((VINP(7) .EQ. 1.D0).AND.(VINP(6) .EQ. 0.D0)) THEN
            MATR = 2
           ELSEIF ((VINP(7) .EQ. 1.D0).AND.(VINP(6) .EQ. 1.D0)) THEN
            MATR = 1
           END IF
        END IF
       CALL R8INIR(6*6,0.D0,DSIDE,1)
       IF (MATR .EQ. 0) THEN

          CALL LKELAS ( NDI, NDT, MOD , NBMAT, MATERD, 
     &              DEPML, SIGML, VINM, DE,KK,MU)

          DO 120 I = 1,NDT
          DO 130 K = 1,NDT
          DSIDE(I,K) = DE(I,K)
 130     CONTINUE
 120     CONTINUE

       ELSE
      
          CALL LKLNVI( MOD, NDT, NDI, NVI)
          CALL LKELAS( NDI, NDT, MOD , NBMAT, MATERD, 
     &              DEPML, SIGML, VINM, DE,KK,MU)
          VINTR = VINM(3)
          IF (VINM(1).LT.XIPIC) THEN 
            VAL  = 0
            ELSE
            VAL  = 1
          ENDIF
          CALL LKOPTG(MATR,VAL,VARV,DGAMV,VINTR,DT,NBMAT, 
     &                MATERD,I1ML, SML,VINM,DE,DEPSV,DSIDE)
      
       ENDIF
  
      ENDIF
C==================================================================
C--------- CONTRAINTES DE SORTIE: 
C -------- RETABLISSEMENT DES SIGNES POUR ASTER --
C==================================================================
           DO 50 I = 1,NDT
              SIGP(I) =  MUN *  SIGPL(I)
              DEPS(I) =  MUN *  DEPML(I) 
  50       CONTINUE
C =================================================================
 1000 CONTINUE
      END
