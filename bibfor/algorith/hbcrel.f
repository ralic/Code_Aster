      SUBROUTINE HBCREL(VP,GAMMA,DG,NBMAT,MATERF,SIGEQE,I1E,ETAP,PARAME,
     &   SEUIL)
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
C =====================================================================
      IMPLICIT      NONE
      INTEGER        NBMAT
      REAL*8        VP(3),GAMMA,MATERF(NBMAT,2),SIGEQE,I1E,SEUIL,DG,ETAP
      REAL*8        PARAME(4)
C ======================================================================
C --- HOEK BROWN : CALCUL DU CRITERE PLASTIQUE F(SIGE,DG) --------------
C ======================================================================
C IN  VP     VALEURS PROPRES DU DEVIATEUR DE SIGMA ---------------------
C IN  GAMMA  VALEUR DE LA VARIABLE D ECROUISSAGE GAMMA_PLUS ------------
C IN  DG     INCREMENT DE GAMMA ----------------------------------------
C IN  NBMAT  NOMBRE DE DONNEES MATERIAU -------------------------------
C IN  MATERF DONNEES MATERIAU ------------------------------------------
C IN  ETAP   VALEUR DE ETA A GAMMA_PLUS --------------------------------
C IN  PARAME VALEUR DES PARAMETRES D ECROUISSAGE -----------------------
C OUT SEUIL  VALEUR DU CRITERE PLASTIQUE -------------------------------
C ======================================================================
      REAL*8       DIFSIG,SIG3,MU,K,SIGBD,UN,TROIS,NEUF
      REAL*8       AUX1,AUX2
C ======================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( NEUF   =  9.0D0  )
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU --------------------------------
C ======================================================================
      MU    = MATERF(4,1)
      K     = MATERF(5,1)
      SIGBD = MATERF(14,2)
C ======================================================================
C --- CALCUL DES VALEURS PROPRES DE SIG --------------------------------
C ======================================================================
      DIFSIG  = (VP(3)-VP(1))*(UN - TROIS*MU*DG/(SIGEQE*(ETAP+UN)))
      SIG3    = VP(3)*(UN - TROIS*MU*DG/(SIGEQE*(ETAP+UN))) +
     +     (I1E - NEUF*K*ETAP*DG/(ETAP+UN))/TROIS
C ======================================================================
C --- CALCUL DU SEUIL --------------------------------------------------
C ======================================================================
      AUX1 = -SIG3*PARAME(2)+PARAME(1)
      AUX2 = PARAME(3)*(UN+SIG3/SIGBD)
      SEUIL = (DIFSIG - AUX2)**2 - AUX1
 10   CONTINUE
C ======================================================================
      END
