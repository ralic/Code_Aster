function prjsom(nbmat, mater, invare, invars, b,&
                siie, type)
!
    implicit     none
#include "asterfort/assert.h"
#include "asterfort/cosphi.h"
    logical :: prjsom
    integer :: nbmat
    real(kind=8) :: invare, invars, mater(nbmat, 2), b, siie
    character(len=9) :: type
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
! --- BUT : TESTER S'IL DOIT Y AVOIR PROJECTION ------------------------
! --------- AU SOMMET DU DOMAINE DE REVERSIBILITE ----------------------
! ======================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! --- : INVARE : PREMIER INVARIANT DU TENSEUR DE CONTRAINTES ELASTIQUES-
! --- : INVARS : PREMIER INVARIANT DU TENSEUR DE CONTRAINTES AU SOMMET -
! --- : B      : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE --------
! ------------ : DU MATERIAU -------------------------------------------
! --- : SIIE   : NORME DU DEVIATEUR ------------------------------------
! --- : TYPE   : 'SUPERIEUR' OU 'INFERIEUR' POUR LE CALCUL DE COSPHI ---
! OUT : PRJSOM : TRUE SI LA PROJECTION AU SOMMET EST RETENUE -----------
! --- :        : FALSE SI PROJECTION AU SOMMET DU DOMAINE --------------
! ======================================================================
    real(kind=8) :: mun, zero, deux, trois
    real(kind=8) :: mu, k, gamcjs, costyp, test1, test2
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
    parameter       ( zero   =  0.0d0  )
    parameter       ( deux   =  2.0d0  )
    parameter       ( trois  =  3.0d0  )
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    mu = mater ( 4,1)
    k = mater ( 5,1)
    gamcjs = mater (12,2)
! ======================================================================
! --- CALCUL A PRIORI DE LA PROJECTION AU SOMMET -----------------------
! ======================================================================
    test1 = invare-invars
    if (type .eq. 'SUPERIEUR') then
        if (b .lt. zero) then
            costyp = cosphi(b, gamcjs, 'MAX')
        else
            costyp = cosphi(b, gamcjs, 'MIN')
        endif
        test2 = mun*trois*k*b*siie*costyp/(deux*mu)
        if (test1 .lt. test2) then
            prjsom = .false.
        else
            prjsom = .true.
        endif
    else if (type.eq.'INFERIEUR') then
        if (b .lt. zero) then
            costyp = cosphi(b, gamcjs, 'MIN')
        else
            costyp = cosphi(b, gamcjs, 'MAX')
        endif
        test2 = mun*trois*k*b*siie*costyp/(deux*mu)
        if (test1 .gt. test2) then
            prjsom = .false.
        else
            prjsom = .true.
        endif
    else
        call assert(.false.)
    endif
! ======================================================================
end function
