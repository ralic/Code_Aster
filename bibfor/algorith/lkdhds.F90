subroutine lkdhds(nbmat, mater, invar, s, dhds,&
                  retcom)
!
    implicit      none
#include "asterc/r8miem.h"
#include "asterfort/cjst.h"
#include "asterfort/cos3t.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lkhlod.h"
#include "asterfort/u2mess.h"
    integer :: nbmat, retcom
    real(kind=8) :: mater(nbmat, 2), invar, s(6), dhds(6)
! =================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
! =================================================================
! --- BUT : CALCUL DES DERICEES dh(THETA)/ds ----------------------
! =================================================================
! IN  :  NBMAT :  NOMBRE DE PARAMETRES MATERIAU -------------------
! --- :  MATER :  COEFFICIENTS MATERIAU A T+DT --------------------
! -----------  :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES --------
! -----------  :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES --------
! --- :  INVAR : INVARINAT DES CONTRAINTES ------------------------
! --- :  S     : DEVIATEUR DES CONTRAINTES ------------------------
! OUT : DHDS: dh(theta)/ds ----------------------------------------
!     : RETCOM : CODE RETOUR POUR REDECOUPAGE ---------------------
! =================================================================
    integer :: ndt, ndi, ii
    real(kind=8) :: gamcjs, pref
    real(kind=8) :: t(6)
    real(kind=8) :: sii, rcos3t, rhlode, h5
    real(kind=8) :: deux, cinq, six, lgleps, ptit
    real(kind=8) :: fact1, fact2
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( deux    =  2.0d0   )
    parameter       ( cinq    =  5.0d0   )
    parameter       ( six     =  6.0d0   )
    parameter       ( lgleps  =  1.0d-8  )
! -----------------------------------------------------------------
    common /tdim/   ndt  , ndi
! -----------------------------------------------------------------
! =================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
! =================================================================
    gamcjs = mater(5,2)
    pref = mater(1,2)
! =================================================================
! --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
! =================================================================
    retcom = 0
    ptit = r8miem()
    call lcprsc(s, s, sii)
    sii = sqrt (sii)
    if (sii .lt. ptit) then
        call u2mess('A', 'COMPOR1_29')
        retcom = 1
        goto 1000
    endif
! =================================================================
! --- CALCUL DE h(THETA) ------------------------------------------
! =================================================================
    rcos3t = cos3t (s, pref, lgleps)
    rhlode = lkhlod (gamcjs, rcos3t)
    h5 = (rhlode)**cinq
!
    call cjst(s, t)
!
! =================================================================
! --- VARIABLES INTERMEDIAIRES-------------------------------------
! =================================================================
    fact1 = gamcjs*rcos3t/deux/h5/sii**2
    fact2 = gamcjs*sqrt(54.d0)/six/h5/sii**3
! =================================================================
! --- CALCUL FINAL ------------------------------------------------
! =================================================================
    do 10 ii = 1, ndt
        dhds(ii) = fact1*s(ii)-fact2*t(ii)
10  end do
! =================================================================
1000  continue
end subroutine
