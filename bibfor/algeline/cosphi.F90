function cosphi(coefb, gamcjs, type)
!
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
    real(kind=8) :: coefb, gamcjs, cosphi
    character(len=3) :: type
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DE COS(PHI) POUR LA PROJECTION AU SOMMET DU DOMAINE -
! ======================================================================
! IN  : COEFB  : COEFFICIENT BPRIME OBTENU LORS DU CALCUL DE LA NORMALE
! --- : GAMCJS : DONNEE MATERIAU ---------------------------------------
! --- : TYPE   : TYPE DE L'ENCADREMENT ---------------------------------
! ------------ : 'MIN' : MINORANT --------------------------------------
! ------------ : 'MAX' : MAJORANT --------------------------------------
! OUT : COSPHI = 3/( (COEFB**2+3) * ------------------------------------
! ------------ :      SQRT( (3/(COEFB**2+3))**2 - 1/2 ------------------
! ------------ :          + 1/(2*(1+LAMBDA*GAMCJS)) --------------------
! ------------ :          + GAMCJS**2/(2*(1+LAMBDA*GAMCJS))**2 )) ------
! ------------ : AVEC LAMBDA =  1 SI TYPE = 'MAX' ----------------------
! ------------ :      LAMBDA = -1 SI TYPE = 'MIN' ----------------------
! ======================================================================
    real(kind=8) :: un, trois, quatre, neuf, epstol
    real(kind=8) :: fact1, fact3, fact4, racine
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( trois  =  3.0d0  )
    parameter       ( quatre =  4.0d0  )
    parameter       ( neuf   =  9.0d0  )
! ----------------------------------------------------------------------
    epstol = r8prem()
    if (type .eq. 'MAX') then
        cosphi = un
    else if (type.eq.'MIN') then
! ======================================================================
! --- CALCUL DE FACT1 = 1/(2*(1+LAMBDA*GAMCJS)) ------------------------
! ======================================================================
        if ((un-gamcjs*gamcjs) .lt. epstol) then
            call utmess('F', 'ALGELINE_4')
        endif
        fact1 = (gamcjs*gamcjs)/(quatre*(un-gamcjs*gamcjs))
! ======================================================================
! --- CALCUL DE FACT4 = (3/(COEFB**2+3))**2 - 1/2 ----------------------
! ======================================================================
        fact3 = coefb*coefb + trois
        fact4 = neuf/(fact3*fact3)
! ======================================================================
! --- CALCUL FINAL DE COSPHI -------------------------------------------
! ======================================================================
        racine = sqrt(fact4 + fact1)
        cosphi = trois/(fact3*racine)
    else
        ASSERT(.false.)
    endif
! ======================================================================
end function
