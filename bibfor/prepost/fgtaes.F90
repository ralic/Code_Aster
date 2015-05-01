subroutine fgtaes(nommat, nomnap, nbcycl, epsmin, epsmax,&
                  dom)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/limend.h"
#include "asterfort/rcpare.h"
#include "asterfort/rcvale.h"
    character(len=*) :: nommat, nomnap
    real(kind=8) :: epsmin(*), epsmax(*)
    real(kind=8) :: dom(*)
    integer :: nbcycl
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!     -----------------------------------------------------------------
!     CALCUL DU DOMMAGE ELEMENTAIRE POUR TAHERI_MIXTE
!     ------------------------------------------------------------------
! IN  NOMMAT : K   : NOM DU MATERIAU
! IN  NOMNAP : K   : NOM DE LA NAPPE
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  EPSMIN : R   : DEFORMATIONS MINIMALES DES CYCLES
! IN  EPSMAX : R   : DEFORMATIONS MAXIMALES DES CYCLES
! OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
!     ------------------------------------------------------------------
!
    integer :: icodre(10), icodwo
    integer :: icodba, icodhs
    character(len=4) :: mode
    character(len=8) :: nompa1, nomp(2)
    character(len=16) :: nomre1, nomre2, nomres(10), cara
    character(len=8) :: nompar, kbid
    character(len=32) :: pheno
    real(kind=8) :: nrupt(1), delta, dnap, epmax, valp(2), val(10)
    real(kind=8) :: salt, x, re(1), slmodi, y
    aster_logical :: endur
!-----------------------------------------------------------------------
    integer :: i, ier, nbpar
    real(kind=8) :: rbid, zero
!-----------------------------------------------------------------------
    data zero /1.d-13/
!
    call jemarq()
!
    rbid = 0.d0
    epmax = 0.d0
    nomre1 = 'MANSON_COFFIN'
    nomre2 = 'WOHLER  '
    nbpar = 1
    pheno = 'FATIGUE '
    nompa1 = 'EPSI    '
!
    cara = 'WOHLER'
    call rcpare(nommat, pheno, cara, icodwo)
    if (icodwo .eq. 0) mode = 'FONC'
    cara = 'A_BASQUIN'
    call rcpare(nommat, pheno, cara, icodba)
    if (icodba .eq. 0) then
        nompar = ' '
        nbpar = 0
        nomres(2) = 'A_BASQUIN'
        nomres(3) = 'BETA_BASQUIN'
        call rcvale(nommat, 'FATIGUE', nbpar, nompar, [rbid],&
                    2, nomres(2), val(2), icodre(2), 2)
        mode = 'BASQ'
    endif
    cara = 'A0'
    call rcpare(nommat, pheno, cara, icodhs)
    if (icodhs .eq. 0) then
        nomres(4) = 'E_REFE'
        nomres(5) = 'A0'
        nomres(6) = 'A1'
        nomres(7) = 'A2'
        nomres(8) = 'A3'
        nomres(9) = 'SL'
        nbpar = 0
        nompar = ' '
        call rcvale(nommat, 'FATIGUE', nbpar, nompar, [rbid],&
                    6, nomres(4), val(4), icodre(4), 2)
        nomres(10) = 'E'
        call rcvale(nommat, 'ELAS', nbpar, nompar, [rbid],&
                    1, nomres(10), re(1), icodre(10), 2)
        mode = 'ZONE'
    endif
!
    do 10 i = 1, nbcycl
        delta = (abs(epsmax(i)-epsmin(i)))/2.d0
        if (delta .gt. epmax-zero) then
            epmax = delta
!
! --- INTERPOLATION SUR MANSON_COFFIN ---
!
            call rcvale(nommat, pheno, nbpar, nompa1, [delta],&
                        1, nomre1, nrupt(1), icodre(1), 2)
            dom(i) = 1.d0/nrupt(1)
        else
            nomp(1) = 'X'
            nomp(2) = 'EPSI'
            valp(1) = epmax
            valp(2) = delta
            call fointe('F ', nomnap, 2, nomp, valp,&
                        dnap, ier)
!
! --- INTERPOLATION SUR WOHLER ---
!
            if (mode .eq. 'FONC') then
                nbpar = 1
                nompar = 'SIGM'
                call limend(nommat, dnap, 'WOHLER', kbid, endur)
                if (endur) then
                    dom(i) = 0.d0
                else
                    call rcvale(nommat, pheno, nbpar, nompar, [dnap],&
                                1, nomre2, nrupt(1), icodre(1), 2)
                    dom(i) = 1.d0/nrupt(1)
                endif
            else if (mode.eq.'BASQ') then
                dom(i) = val(2)* dnap**val(3)
            else if (mode.eq.'ZONE') then
                slmodi = val(9)
                salt = (val(4)/re(1))*dnap
                x = log10 (salt)
                if (salt .ge. slmodi) then
                    y = val(5) + val(6)*x + val(7)*x**2 + val(8)*x**3
                    nrupt(1) = 10**y
                    dom(i) = 1.d0 / nrupt(1)
                else
                    dom(i) = 0.d0
                endif
            endif
        endif
 10 end do
!
    call jedema()
end subroutine
