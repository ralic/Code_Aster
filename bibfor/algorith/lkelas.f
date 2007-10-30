        SUBROUTINE LKELAS ( NDI, NDT, MOD , NMAT, MATER, 
     &                      DEPS, SIGD, VIND, DE,K,MU)
        IMPLICIT NONE
        INTEGER         NDI, NDT, NMAT 
        REAL*8          MATER(NMAT,2) 
        REAL*8          SIGD(6) 
        REAL*8          VIND(9) 
        REAL*8          DEPS(6), DE(6,6), MU, K
        CHARACTER*8     MOD
C =================================================================
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
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
C =================================================================
C       MATRICE  HYPOELASTIQUE 
C       IN  NDI    :  DIMENSION DE L ESPACE
C           NDT    :  2* NDI POUR LE CALCUL TENSORIEL
C           MOD    :  MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATER :  COEFFICIENTS MATERIAU 
C           DEPS   :  INCREMENT DE DEFORMATION
C           SIGD   :  CONTRAINTE  A T
C           VIND   :  VARIABLES INTERNES A T
C       OUT DE     :  MATRICE HYPOELASTIQUE
C            K     :  MODULE DE COMPRESSIBILITE
C            G     :  MODULE DE CISAILLEMENT
C       -----------------------------------------------------------
        INTEGER         I, J
        REAL*8          MUE, KE, PA, NELAS
        REAL*8          INVAR1, TRACE, DSIG(6)
        REAL*8          DEUX, TROIS  
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
        PARAMETER       ( DEUX   =  2.0D0  )
        PARAMETER       ( TROIS  =  3.0D0  )
C =================================================================
C --- CALCUL DU PREMIER INVARIANT DES CONTRAINTES A LINSTANT T ----
C =================================================================
        INVAR1 = TRACE (NDI, SIGD)
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
        MUE = MATER(4,1)
        KE  = MATER(5,1)

        PA    = MATER(1,2)
        NELAS = MATER(2,2)

C =================================================================
C --- CALCUL DES PARAMETRES AU TEMPS T + DT -----------------------
C =================================================================
        K = KE * (INVAR1/TROIS/PA)**NELAS
        MU = MUE * (INVAR1/TROIS/PA)**NELAS
C =================================================================
C --- DEFINITION DE LA MATRICE HYPOLELASTIQUE ---------------------
C =================================================================
        CALL R8INIR(6*6,0.D0,DE,1)     
        DO 10 I = 1,3
        DO 20 J = 1,3
        DE(I,J) = K - DEUX*MU/TROIS
   20   CONTINUE
   10   CONTINUE
        
        DO 30 I = 1,NDT
        DE(I,I) = DE(I,I) + DEUX*MU
   30   CONTINUE
                 
C =================================================================
       END
