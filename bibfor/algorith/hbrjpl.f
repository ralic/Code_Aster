      SUBROUTINE HBRJPL(MOD,NBMAT,MATERF,SIGP,VIP,VIM,VP,VECP,DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2005   AUTEUR JMBHH01 J.M.PROIX 
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
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        VIM(*),VIP(*),SIGP(6),DSIDEP(6,6),MATERF(NBMAT,2)
      REAL*8        VP(3),VECP(3,3)
      CHARACTER*8   MOD(*)
C ======================================================================
C -- HOEK BROWN : CALCUL DE LA MATRICE TANGENTE COHERENTE DSIG/DEPS ----
C ======================================================================
C IN  : MOD    : TYPE DE MODELISATION ----------------------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATERF : PARAMETRES MATERIAU -----------------------------------
C --- : SIGP   : TENSEUR DES CONTRAINTES A T+ --------------------------
C --- : VIM    : VARIABLES INTERNES A T- -------------------------------
C --- : VIP    : VARIABLES INTERNES A T+ -------------------------------
C --- : VP     : VALEURS PROPRES DU DEVIATEUR ELASTIQUE ----------------
C --- : VECP   : VECTEURS PROPRES DU DEVIATEUR ELASTIQUE ---------------
C OUT : DSIDEP : DSIG/DEPS ---------------------------------------------
C ======================================================================
      INTEGER      NDT,NDI,ITMAX,II,JJ
      REAL*8       GP,ETAP,SIG3,MU,K,NEUF
      REAL*8       I1E,DG,SIGEQE,SE(6),UN,ZERO
      REAL*8       HOOKF(6,6),DEUX,TROIS,TRACE
      REAL*8       SF(6),SEQF
      REAL*8       PARAME(4),DERIVE(5)
      REAL*8       DSDDE(6,6),DSEDE(6,6),PARAM1,DDLDE(6),GRES,GRUP   
      REAL*8       VUNITE(6),BIDON(6,6),PMAT1(6,6),DETADG,PMAT6(6,6)
      REAL*8       PMAT2(6,6),PMAT3(6,6),PMAT4(6,6),PMAT5(6,6),DGDL
      REAL*8       TMP(6,6),SEB(6),PI,R8PI,PPHI1,PPHI2,PPHI0
C ======================================================================
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( ZERO   =  0.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( NEUF   =  9.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      K = MATERF(5,1)
      MU = MATERF(4,1)
      PI     = R8PI()
      PI     = PI/180.D0
      GRUP = MATERF(1,2)
      GRES = MATERF(2,2)
      PPHI1 = MATERF(9,2)
      PPHI2 = MATERF(15,2)
      PPHI0 = MATERF(16,2)                      
      CALL LCINMA(0.0D0,DSIDEP)
      CALL LCINMA(0.0D0,BIDON)      
      CALL LCINMA(0.0D0,DSDDE)
      CALL LCINMA(0.0D0,DSEDE)
      CALL LCINMA(0.0D0,PMAT1)
      CALL LCINMA(0.0D0,PMAT2)  
      CALL LCINMA(0.0D0,PMAT3)  
      CALL LCINMA(0.0D0,PMAT4)  
      CALL LCINMA(0.0D0,PMAT5)   
      CALL LCINMA(0.0D0,PMAT6)   
      CALL LCINMA(0.0D0,TMP)
      PI     = R8PI()/180.0D0        
C =====================================================================
C --- CALCUL DES PARAMETRES D ECROUISSAGE -----------------------------
C =====================================================================
      GP = VIP(1)
      DG = VIP(1)-VIM(1)
      CALL HBVAEC(GP,NBMAT,MATERF,PARAME)
      ETAP = DEUX*SIN(PARAME(4)*PI)/(TROIS+SIN(PARAME(4)*PI))
C =====================================================================
C --- CALCUL DES VALEURS PROPRES --------------------------------------
C =====================================================================
      CALL LCDEVI(SIGP,SF)
      CALL PSCAL(NDT,SF,SF,SEQF)
      SIGEQE = SQRT(TROIS*SEQF/DEUX)+TROIS*MU*DG/(ETAP+UN) 
      I1E    = TRACE(NDI,SIGP)+9.0D0*K*ETAP*DG/(ETAP+UN)
      DO 10 II=1,NDT
         SE(II) = SF(II)/(1.0D0-3.0D0*MU*DG/((ETAP+UN)*SIGEQE))
 10   CONTINUE     
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE ------------------------------------
C ======================================================================
      SIG3 = VP(3)*(UN - TROIS*MU*DG/(SIGEQE*(ETAP+UN))) +
     +      (I1E - NEUF*K*ETAP*DG/(ETAP+UN))/TROIS          
      CALL HBDERI(GP,NBMAT,MATERF,ZERO,ETAP,PARAME,DERIVE)
C ======================================================================
      IF (GP.LT.MATERF(1,2)) THEN 
         DETADG = 6.0D0*(PPHI1-PPHI0)*PI*COS(PARAME(4)*PI) /
     &                (GRUP*(TROIS+SIN(PARAME(4)*PI))**2)
      ELSE IF (GP.LT.MATERF(2,2)) THEN 
         DETADG = 6.0D0*(PPHI2-PPHI1)*PI*COS(PARAME(4)*PI) /
     &                ((GRES-GRUP)*(TROIS+SIN(PARAME(4)*PI))**2)
      ELSE
         DETADG = 0.D0
      ENDIF        
      DGDL   = ETAP+UN
C =====================================================================
C --- CALCUL DU VECTEUR UNITE -----------------------------------------
C =====================================================================
      DO 120 II = 1, NDI
         VUNITE(II) = UN
 120  CONTINUE
      DO 125 II = NDI+1,6
         VUNITE(II) = 0.0D0
 125  CONTINUE 
      DO 150 II=1,NDI
               SEB(II) = SE(II)
 150  CONTINUE             
      DO 140 II=NDI+1,NDT
               SEB(II) = SE(II) / SQRT(DEUX)
 140  CONTINUE  
      DO 145 II=NDT+1,6
               SEB(II) = 0.0D0
 145  CONTINUE      
C =====================================================================
C --- CALCUL DE DSEDE -------------------------------------------------
C =====================================================================
      DO 15 II = 1, NDI
        DO 20 JJ = 1, NDI
            DSEDE(II,JJ) = - DEUX*MU/TROIS
 20     CONTINUE
 15   CONTINUE
      DO 30 II = 1, NDT
        DSEDE(II,II) = DSEDE(II,II) + DEUX*MU
 30   CONTINUE
C =====================================================================
C --- CALCUL DE K*DIEDE -----------------------------------------------
C =====================================================================
      CALL     LCPRTE( VUNITE, VUNITE, BIDON )
      CALL     LCPRSM( K, BIDON,  PMAT1 )       
C =====================================================================
C --- CALCUL DE PARA*DSEDE --------------------------------------------
C =====================================================================
       PARAM1 = UN - TROIS*MU*DG/(SIGEQE*(ETAP+UN))
       CALL     LCPRSM( PARAM1, DSEDE,  PMAT2 )
       CALL     LCSOMA(PMAT2,PMAT1,PMAT6)             
C =====================================================================
C --- CALCUL DE SE*DSIGEQDE -------------------------------------------
C ====================================================================
      PARAM1 = NEUF*MU*MU*DG/((ETAP+UN)*SIGEQE**3)
      CALL     LCPRTE( SEB, SEB, BIDON )
      CALL     LCPRSM( PARAM1, BIDON,  PMAT3 )            
C ======================================================================
C --- CALCUL DE DDLAMBDA/DE ----------------------------------------
C=======================================================================
      CALL CALCDL(VP,I1E,SIGEQE,NBMAT,MATERF,PARAME,DERIVE,SIG3,
     &                 VECP,ETAP,DG,SEB,DETADG,DGDL,DDLDE)        
C ======================================================================
      PARAM1 = TROIS*MU/SIGEQE
      CALL     LCPRTE( SEB, DDLDE, BIDON )
      CALL     LCPRSM( PARAM1, BIDON,  PMAT4 )       
C ======================================================================
      PARAM1 = TROIS*K*(DETADG*DGDL*DG/(ETAP+UN)+ETAP)        
      CALL     LCPRTE( DDLDE, VUNITE, BIDON )
      CALL     LCPRSM( PARAM1, BIDON,  PMAT5 )                
C ======================================================================
C --- CALCUL DE DSIG/DEPS ----------------------------------------------
C ======================================================================
      DO 90    II = 1,NDT
         DO 100 JJ = 1,NDT
             DSIDEP(II,JJ) = PMAT6(II,JJ)+PMAT3(II,JJ)-PMAT4(II,JJ)
     &              -PMAT5(II,JJ)          
 100  CONTINUE
 90   CONTINUE
C ======================================================================
      END
