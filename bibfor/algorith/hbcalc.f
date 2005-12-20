      SUBROUTINE HBCALC(FMOINS,GAMMA,DG,NBMAT,MATERF,I1E,SIGEQE,VP,ETAP,
     &     VH,VG,PARAME,DERIVE,INCRG)
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
      REAL*8        FMOINS,INCRG,GAMMA,DG,I1E,SIGEQE,DERIVE(5)
      REAL*8        VP(3),MATERF(NBMAT,2),ETAP,VH,VG,PARAME(4)
C ======================================================================
C --- LOI DE HOEK BROWN : CALCUL DE L INCREMENT de GAMMA (VAR ECROUIS.) 
C ======================================================================
C IN   FMOINS  CRITERE PLASTIQUE A L ITERATION PRECEDENTE --------------
C IN   GAMMA   VARIABLE D ECROUISSAGE GAMMA A L ITERATION PRECEDENTE ---
C IN   NBMAT   NOMBRE DE DONNEES MATERIAU ------------------------------
C IN   MATERF  DONNEES MATERIAU ----------------------------------------
C IN   VP      VALEURS PROPRES Du DEVIATEUR ELASTIQUE SE ---------------
C IN   I1E     PREMIER INVARIANT ELASTIQUE -----------------------------
C IN   DG      VALEUR DE DELTA GAMMA A L ITERATION PRECEDENTE ----------
C IN   ETAP    VALEUR DE ETA A L ITERATION PRECEDENTE ------------------
C IN   VH, VG  VALEUR DES FONCTIONS H ET G A L ITERATION PRECEDENTE ----
C IN   PARAME  VALEUR DES PARAMETRES DE LA LOI A L ITERATION PRECEDENTE
C IN   DERIVE  VALEUR DES DERIVEES DES PARAMETRES A L ITER PRECEDENTE --
C OUT  INCRG   VALEUR DE L INCREMENT POUR DGAMMA A L ITERATION COURANTE
C ======================================================================
      REAL*8       DFDGA,DERH,DERG,AUX1,AUX2,AUX3,AUX4
      REAL*8       VM,DS,DM     
      REAL*8       A1,A2,A3,A4,SIGBD,MU,UN,DEUX,TROIS,EPS
C ======================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( EPS    =  1.0D-12 )
C ======================================================================
C --- INITIALISATION DES PARAMETRES ------------------------------------
C ======================================================================
      SIGBD  = MATERF(14,2)
      MU     = MATERF(4,1)
      A1     = VP(3)+I1E/TROIS
      A2     = VP(3)-VP(1)
      A3     = UN/SIGBD
      A4     = TROIS*MU/SIGEQE
      DERH    = DERIVE(4)
      DERG    = DERIVE(5)
      VM      = PARAME(2)      
      DS      = DERIVE(1)
      DM      = DERIVE(2)
      AUX1    = -A2*A4*(DERH*DG+VH) 
      AUX2    = -DERIVE(3)*(UN+A3*(A1-VG*DG))+PARAME(3)*A3*(DERG*DG+VG)
      AUX3    = A2*(UN-A4*VH*DG)-PARAME(3)*(UN+A3*(A1-VG*DG))
      AUX4    = DS
     &     -DM*(A1-VG*DG)
     &     +VM*(DERG*DG+VG)
C ======================================================================
      DFDGA = DEUX*AUX3*(AUX1 + AUX2) - AUX4
      IF (ABS(DFDGA).LT.EPS) THEN
         CALL UTMESS('F','HBCALC','DERIVEE DE F NULLE')
      ENDIF
      INCRG = -FMOINS/DFDGA
C ======================================================================
      END
