subroutine solrei(gamp, s, i1n, parame, nbmat,&
                  mater, q, vecn, codret)
!
    implicit   none
#include "asterfort/bprime.h"
#include "asterfort/calcn.h"
#include "asterfort/calcq.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbmat, codret
    real(kind=8) :: s(6), i1n, parame(5), mater(nbmat, 2), q(6), vecn(6), gamp
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
! --- BUT : CALCUL DE Q ET DE N ----------------------------------------
! ======================================================================
! IN  : NDT    : NOMBRE DE COMPOSANTES TOTAL DU TENSEUR ----------------
! --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
! --- : S      : TENSEUR DU DEVIATEUR DES CONTRAINTES ------------------
! --- : I1N    : PREMIER INVARIANT DES CONTRAINTES --------------------
! --- : PARAME : VARIABLES D'ECROUISSAGES ------------------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! OUT : Q      : DERIVEE Q = DG/DSIG -----------------------------------
! --- : VECN   : VECTEUR N POUR PROJECTION SUR LE DOMAINE --------------
! ======================================================================
    integer :: ndt, ndi
    real(kind=8) :: zero, un, epsult, gamult, gamcjs, pref, epssig
    real(kind=8) :: b
! ======================================================================
! --- INITIALISATION DE PARAMETRE --------------------------------------
! ======================================================================
    parameter       ( zero     =  0.0d0   )
    parameter       ( un       =  1.0d0   )
    parameter       ( epsult   =  1.0d-03 )
    parameter       ( epssig   =  1.0d-8  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DE PARAMETRES MATERIAU ------------------------------
! ======================================================================
    gamult = mater( 1,2)
    gamcjs = mater(12,2)
    pref = mater(15,2)
! ======================================================================
! --- CALCUL DE Q ------------------------------------------------------
! ======================================================================
    call calcq(s, gamcjs, pref, epssig, q,&
               codret)
    if (codret .ne. 0) goto 100
! ======================================================================
! --- CALCUL DE N ------------------------------------------------------
! ======================================================================
! --- CAS OU GAMP > GAMULT(1-EPS) --------------------------------------
! ======================================================================
    if (gamp .gt. (gamult*(un-epsult))) then
        b = zero
    else
! ======================================================================
! --- CAS OU GAMP <= GAMULT(1-EPS) -------------------------------------
! ======================================================================
        b = bprime(nbmat, mater, parame, i1n, s, epssig)
    endif
    call calcn(s, b, vecn)
! ======================================================================
100  continue
    call jedema()
! ======================================================================
end subroutine
