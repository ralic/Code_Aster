      FUNCTION COSPHI (COEFB, GAMCJS, TYPE)
C
      IMPLICIT     NONE
      REAL*8       COEFB, GAMCJS, COSPHI
      CHARACTER*3  TYPE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C --- BUT : CALCUL DE COS(PHI) POUR LA PROJECTION AU SOMMET DU DOMAINE -
C ======================================================================
C IN  : COEFB  : COEFFICIENT BPRIME OBTENU LORS DU CALCUL DE LA NORMALE
C --- : GAMCJS : DONNEE MATERIAU ---------------------------------------
C --- : TYPE   : TYPE DE L'ENCADREMENT ---------------------------------
C ------------ : 'MIN' : MINORANT --------------------------------------
C ------------ : 'MAX' : MAJORANT --------------------------------------
C OUT : COSPHI = 3/( (COEFB**2+3) * ------------------------------------
C ------------ :      SQRT( (3/(COEFB**2+3))**2 - 1/2 ------------------
C ------------ :          + 1/(2*(1+LAMBDA*GAMCJS)) --------------------
C ------------ :          + GAMCJS**2/(2*(1+LAMBDA*GAMCJS))**2 )) ------
C ------------ : AVEC LAMBDA =  1 SI TYPE = 'MAX' ----------------------
C ------------ :      LAMBDA = -1 SI TYPE = 'MIN' ----------------------
C ======================================================================
      REAL*8 UN, TROIS, QUATRE, NEUF, EPSTOL, R8PREM
      REAL*8 FACT1, FACT3, FACT4, RACINE
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( QUATRE =  4.0D0  )
      PARAMETER       ( NEUF   =  9.0D0  )
C ----------------------------------------------------------------------
      EPSTOL = R8PREM()
      IF (TYPE.EQ.'MAX') THEN
         COSPHI =  UN
      ELSE IF (TYPE.EQ.'MIN') THEN
C ======================================================================
C --- CALCUL DE FACT1 = 1/(2*(1+LAMBDA*GAMCJS)) ------------------------
C ======================================================================
         IF ((UN-GAMCJS*GAMCJS).LT.EPSTOL) THEN
            CALL U2MESS('F','ALGELINE_4')
         ENDIF
         FACT1 = (GAMCJS*GAMCJS)/(QUATRE*(UN-GAMCJS*GAMCJS))
C ======================================================================
C --- CALCUL DE FACT4 = (3/(COEFB**2+3))**2 - 1/2 ----------------------
C ======================================================================
         FACT3 = COEFB*COEFB + TROIS
         FACT4 = NEUF/(FACT3*FACT3)
C ======================================================================
C --- CALCUL FINAL DE COSPHI -------------------------------------------
C ======================================================================
         RACINE = SQRT(FACT4 + FACT1)
         COSPHI = TROIS/(FACT3*RACINE)
      ELSE
         CALL U2MESS('F','ALGELINE_29')
      ENDIF
C ======================================================================
      END
