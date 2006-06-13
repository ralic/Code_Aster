      SUBROUTINE DRUDRS(PARAME, Q, H0, SIGC, DUDSIG)
C
      IMPLICIT      NONE
      REAL*8        Q(6), PARAME(5), H0, SIGC, DUDSIG(6)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/06/2003   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DE DU/DSIG ------------------------------------------
C ======================================================================
C IN  : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
C --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
C --- : PARAME : PARAMETRES D'ECROUISSAGES -----------------------------
C --- : Q      : DG/DSIG -----------------------------------------------
C --- : H0     : H0 = (1-GAMCJS)**(1/6) --------------------------------
C --- : SIGC   : PARAMETRE MATERIAU ------------------------------------
C OUT : DUDSIG : DUDSIG = - M(GAMP)*K(GAMP)*Q/(RAC(6)*SIGC*H0) ---------
C ------------ :          - K(GAMP)*M(GAMP)*I/(3*SIGC) -----------------
C ======================================================================
      INTEGER II, NDT, NDI
      REAL*8  MGAMP, KGAMP, FACT1, FACT2, MUN, TROIS, SIX
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( SIX    =  6.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DES VARIABLES D'ECROUISSAGES ------------------------
C ======================================================================
      KGAMP = PARAME(3)
      MGAMP = PARAME(4)
C ======================================================================
C --- CALCUL INTERMEDIAIRE ---------------------------------------------
C ======================================================================
      FACT1 = MUN*MGAMP*KGAMP/(SQRT(SIX)*SIGC*H0)
      FACT2 = MUN*MGAMP*KGAMP/(TROIS*SIGC)
C ======================================================================
C --- CALCUL FINAL -----------------------------------------------------
C ======================================================================
      DO 10 II=1,NDT
         DUDSIG(II) = FACT1*Q(II)
 10   CONTINUE
      DO 20 II=1,NDI
         DUDSIG(II) = DUDSIG(II) + FACT2
 20   CONTINUE
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
