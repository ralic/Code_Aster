      SUBROUTINE DRFDRS(N, Q, PARAME, H0, SIGC, RGDEV, U, DUDS, DFDS)
C
      IMPLICIT    NONE
      INTEGER     N
      REAL*8      Q(*), PARAME(5), H0, SIGC, RGDEV, U, DUDS(*), DFDS(*)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/03/2002   AUTEUR CIBHHBC R.FERNANDES 
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
C IN  : N      : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
C --- : Q      : DG/DSIG -----------------------------------------------
C --- : PARAME : PARAMETRES D'ECROUISSAGE ------------------------------
C --- : H0     : H0 = (1-GAMCJS)**(1/6) --------------------------------
C --- : SIGC   : PARAMETRE DU MODELE -----------------------------------
C --- : RGDEV  : G(S) --------------------------------------------------
C --- : U      : CRITERE PLASTIQUE -------------------------------------
C --- : DUDS   : DU/DSIG -----------------------------------------------
C OUT : DFDS   : DF/DSIG = (1/A)*((1/(SIGC*H0))**(1/A))*G**((1-A)/A)*Q -
C ------------ :         - DU/DSIG  ------------------------------------
C ======================================================================
      INTEGER II
      REAL*8  AGAMP, FACT1, FACT2, FACT3, FACT4, UN
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- RECUPERATION DES VARIABLES D'ECROUISSAGES ------------------------
C ======================================================================
      UN = 1.0D0
      AGAMP = PARAME(2)
C ======================================================================
C --- VARIABLE INTERMEDIAIRE -------------------------------------------
C ======================================================================
      FACT2 = UN/AGAMP
      FACT3 = (UN/(SIGC*H0))**FACT2
      FACT3 = FACT3*FACT2
      FACT2 = (UN-AGAMP)/AGAMP
      FACT4 = RGDEV**FACT2
      FACT1 = FACT3*FACT4
C ======================================================================
C --- CALCUL FINAL -----------------------------------------------------
C ======================================================================
      DO 10 II=1,N
         DFDS(II) = FACT1*Q(II)-DUDS(II)
 10   CONTINUE
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
