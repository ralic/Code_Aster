      SUBROUTINE HBDSDP(SE,DG,ETAP,SIGEQE,VP,PARAME,DERIVE,
     &   NBMAT,MATERF,SIG3,DETADG,DGDL,DSDSIP) 
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
      REAL*8        SE(6),DG,ETAP,I1E,DSDSIP(6)
      REAL*8        VP(3),VECP(3,3),SIGEQE,PARAME(4),DERIVE(5),SIG3
      REAL*8        DETADG,DGDL,MATERF(NBMAT,2)
C ======================================================================
C -- HOEK BROWN : CALCUL DE LA MATRICE DSIG/DSIP (CONT. TOTALES) -------
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
      INTEGER      NDT,NDI,II
      REAL*8       DEUX,TROIS,SEB(6),MU,K,PARAM1,DLDSIP
C ======================================================================
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- CALCUL DU VECTEUR UNITE -----------------------------------------
C =====================================================================
      DO 91 II = 1,6
         DSDSIP(II) = 0.0D0
 91   CONTINUE
      MU = MATERF(4,1)
      K = MATERF(5,1)
      DO 150 II=1,NDI
         SEB(II) = SE(II)
 150  CONTINUE             
      DO 140 II=NDI+1,NDT
         SEB(II) = SE(II) / SQRT(DEUX)
 140  CONTINUE  
      DO 145 II=NDT+1,6
         SEB(II) = 0.0D0
 145  CONTINUE      
C ======================================================================
C --- CALCUL DE DDLAMBDA/DSIP ------------------------------------------
C ======================================================================
      CALL CADLDP(VP,SIGEQE,NBMAT,MATERF,PARAME,DERIVE,SIG3,
     &           ETAP,DG,DETADG,DGDL,DLDSIP)        
C ======================================================================
      PARAM1 = -TROIS*MU*DLDSIP/SIGEQE
      CALL     LCPRSV( PARAM1, SEB,  DSDSIP )       
C ======================================================================
      PARAM1 = 1.0D0-TROIS*K*DLDSIP*(DETADG*DGDL*DG/(ETAP+1.0D0)+ETAP)
      DO 90 II = 1,NDI
         DSDSIP(II) = DSDSIP(II)+PARAM1
 90   CONTINUE
C ======================================================================
      END
