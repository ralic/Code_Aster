      SUBROUTINE HBMATA(SE,DG,ETAP,I1E,SIGEQE,VP,VECP,PARAME,
     &   DERIVE,SIG3,DETADG,DGDL,NBMAT,MATERF,DSIDEP) 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2005   AUTEUR JOUMANA J.EL-GHARIB 
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
      REAL*8        SE(6),DG,ETAP,I1E,DSIDEP(6,6),MATERF(NBMAT,2)
      REAL*8        VP(3),VECP(3,3),SIGEQE,PARAME(4),DERIVE(5),SIG3
      REAL*8        DETADG,DGDL
C ======================================================================
C -- HOEK BROWN : CALCUL DE LA MATRICE TANGENTE COHERENTE DSIG/DEPS ----
C ======================================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATERF : PARAMETRES MATERIAU -----------------------------------
C --- : SE     : DEVIATEUR DES CONTRAINTES ELASTIQUES ------------------
C --- : VP     : VALEURS PROPRES DU DEVIATEUR ELASTIQUE ----------------
C --- : VECP   : VECTEURS PROPRES DU DEVIATEUR ELASTIQUE ---------------
C --- : PARAME : VALEUR DES PARAMETRES DE LA LOI S*SIG, M*SIG, B -------
C --- : DERIVE : VALEUR DES DERIVEES DES PARAMETRES PAR RAPPORT A GAMMA 
C --- : SIG3   : CONTRAINTE PRINCIPALE SIG3 ----------------------------
C --- : DG     : INCREMENT DU PARAMETRE D ECROUISSAGE GAMMA ------------
C --- : DETADG : DERIVEE DE ETA PAR RAPPORT A GAMMA --------------------
C --- : DGDL   : DERIVEE  DE GAMMA PAR RAPPORT A LAMBDA ----------------
C OUT : DSIDEP : DSIG/DEPS ---------------------------------------------
C ======================================================================
      INTEGER      NDT,NDI,II,JJ
      REAL*8       UN,DEUX,TROIS,MU,K
      REAL*8       DSEDE(6,6),PARAM1,DDLDE(6),SEB(6)
      REAL*8       VUNITE(6),BIDON(6,6),PMAT1(6,6),PMAT6(6,6)
      REAL*8       PMAT2(6,6),PMAT3(6,6),PMAT4(6,6),PMAT5(6,6)
C ======================================================================
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      CALL LCINMA(0.0D0,DSIDEP)
      CALL LCINMA(0.0D0,BIDON)      
      CALL LCINMA(0.0D0,DSEDE)
      CALL LCINMA(0.0D0,PMAT1)
      CALL LCINMA(0.0D0,PMAT2)  
      CALL LCINMA(0.0D0,PMAT3)  
      CALL LCINMA(0.0D0,PMAT4)  
      CALL LCINMA(0.0D0,PMAT5)   
      CALL LCINMA(0.0D0,PMAT6)   
      MU = MATERF(4,1)
      K = MATERF(5,1)
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
      PARAM1 = 9.0D0*MU*MU*DG/((ETAP+UN)*SIGEQE**3)
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
