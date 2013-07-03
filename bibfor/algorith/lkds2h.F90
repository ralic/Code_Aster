subroutine lkds2h(nbmat, mater, invar, s, dhds,&
                  ds2hds, retcom)
!
    implicit      none
#include "asterc/r8miem.h"
#include "asterfort/cos3t.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lctrma.h"
#include "asterfort/lkhtet.h"
#include "asterfort/r8inir.h"
#include "asterfort/u2mess.h"
    integer :: nbmat, retcom
    real(kind=8) :: invar, mater(nbmat, 2), s(6), dhds(6), ds2hds(6)
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
! --- BUT : CALCUL DES DERICEES d(sII*h(THETA))/dsig --------------
! =================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE -------------------
! --- : MATER  : PARAMETRES DU MODELE -----------------------------
! --- : INVAR :  INVARIANT DES CONTRAINTES ------------------------
! --- : S     :  DEVIATEUR DES CONTRAINTES ------------------------
!     : DHDS   : dh(THETA)/ds--------------------------------------
! OUT : DS2HDS: d(sII*h(THETA))/dsig-------------------------------
! =================================================================
    integer :: ndt, ndi, i, k
    real(kind=8) :: h0ext, pref, h0e, h0c, htheta
    real(kind=8) :: kron(6), iden6(6, 6)
    real(kind=8) :: a(6), b(6, 6), bt(6, 6)
    real(kind=8) :: sii, rcos3t
    real(kind=8) :: zero, un, trois, lgleps, ptit
    real(kind=8) :: fact1
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( zero    =  0.0d0   )
    parameter       ( un      =  1.0d0   )
    parameter       ( trois   =  3.0d0   )
    parameter       ( lgleps  =  1.0d-8  )
! -----------------------------------------------------------------
    common /tdim/   ndt  , ndi
! -----------------------------------------------------------------
    data    kron    /un     ,un     ,un     ,zero   ,zero  ,zero/
    data    iden6   /un     , zero  , zero  , zero  ,zero  ,zero,&
     &                 zero   , un    , zero  , zero  ,zero  ,zero,&
     &                 zero   , zero  , un    , zero  ,zero  ,zero,&
     &                 zero   , zero  , zero  , un    ,zero  ,zero,&
     &                 zero   , zero  , zero  , zero  ,un    ,zero,&
     &                 zero   , zero  , zero  , zero  ,zero  ,un/
! =================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
! =================================================================
    h0ext = mater(4,2)
    pref = mater(1,2)
! =================================================================
! --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
! =================================================================
    retcom = 0
    ptit = r8miem()
    call lcprsc(s, s, sii)
    sii = sqrt (sii)
    if (sii .lt. ptit) then
        call u2mess('A', 'COMPOR1_30')
        retcom = 1
        goto 1000
    endif
!
! =================================================================
! --- CALCUL DE h(THETA), H0E ET H0C, -----------------------------
! =================================================================
    rcos3t = cos3t (s, pref, lgleps)
    call lkhtet(nbmat, mater, rcos3t, h0e, h0c,&
                htheta)
!
    fact1 = (h0c - h0ext)/(h0c - h0e)
! =================================================================
! --- CALCUL DU PREMIER TERME
! =================================================================
!
    call r8inir(6, 0.d0, a, 1)
    do 10 i = 1, ndt
        a(i) = fact1*dhds(i)*sii + htheta*s(i)/sii
10  end do
!
! =================================================================
! --- CALCUL DU SECOND  TERME
! =================================================================
    call r8inir(6*6, 0.d0, b, 1)
    do 20 i = 1, ndt
        do 30 k = 1, ndt
            b(i,k) = iden6(i,k) - kron(i)*kron(k)/trois
30      end do
20  end do
!
! =================================================================
! --- RESULTAT FINAL
! =================================================================
    call r8inir(6, 0.d0, ds2hds, 1)
!
    call lctrma(b, bt)
    call lcprmv(bt, a, ds2hds)
!
! =================================================================
1000  continue
end subroutine
