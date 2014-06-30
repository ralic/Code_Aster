subroutine inithm(imate, yachai, yamec, phi0, em,&
                  cs, tbiot, t, epsv, depsv,&
                  epsvm, angmas, aniso, mdal, dalal,&
                  alphfi, cbiot, unsks, alpha0, ndim,&
                  phenom)
! ======================================================================
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
    implicit none
!
#include "asterfort/calela.h"
#include "asterfort/dilata.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/tebiot.h"
#include "asterfort/unsmfi.h"
#include "asterfort/utmess.h"
!
    integer :: nelas, ndim, iret
    parameter    ( nelas=4 )
    real(kind=8) :: elas(nelas)
    character(len=8) :: ncra1(nelas)
    integer :: icodre(nelas)
    logical(kind=1) :: yachai
    integer :: imate, yamec, i, aniso
    real(kind=8) :: phi0, em, cs, tbiot(6), epsvm, epsv, depsv
    real(kind=8) :: angmas(3), t, eps, dalal, mdal(6), young, nu
    real(kind=8) :: alphfi, rbid(6, 6), biot(4), cbiot, unsks, alpha0, k0
    character(len=16) :: phenom
!
    parameter  ( eps = 1.d-21 )
! ======================================================================
! --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES -----------
! ======================================================================
    data ncra1/'E','NU','ALPHA','RHO'/
!
! =====================================================================
! --- SI PRESENCE DE MECANIQUE OU DE CHAINAGE -------------------------
! =====================================================================
    if ((yamec.eq.1) .or. yachai) then
! =====================================================================
! --- CALCUL CAS ISOTROPE (POUR LA ROUTINE VIPORO) --------------------
! =====================================================================
        999     if (aniso.eq.0) then
!
        call rccoma(imate, 'ELAS', 0, phenom, iret)
        if (iret .eq. 0) then
            if (phenom .eq. 'ELAS_ISTR') then
                call utmess('F', 'ALGORITH17_33')
            else if (phenom.eq.'ELAS_ORTH') then
                call utmess('F', 'ALGORITH17_34')
            endif
        else
! CAS ELAS_GONF
            call rccoma(imate, 'ELAS_ISTR', 0, phenom, iret)
            if (iret .eq. 0) then
                call utmess('F', 'ALGORITH17_33')
            endif
            call rccoma(imate, 'ELAS_ORTH', 0, phenom, iret)
            if (iret .eq. 0) then
                call utmess('F', 'ALGORITH17_34')
            endif
            phenom = 'ELAS'
!
        endif
!
        call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                    [t], 3, ncra1(1), elas( 1), icodre,&
                    0)
!
        young = elas(1)
        nu = elas(2)
        alpha0 = elas(3)
        cbiot = tbiot(1)
        k0 = young / 3.d0 / (1.d0-2.d0*nu)
        unsks = (1.0d0-cbiot) / k0
!
    else if (aniso.eq.1) then
        call rccoma(imate, 'ELAS_ISTR', 0, phenom, iret)
        if (iret .eq. 1) then
            aniso=0
            goto 999
        endif
    else if (aniso.eq.2) then
        call rccoma(imate, 'ELAS_ORTH', 0, phenom, iret)
!
        if (iret .eq. 1) then
            aniso=0
            goto 999
        endif
    endif
! =====================================================================
! --- CALCUL DES GRANDEURS MECANIQUES DANS LE CAS GENERAL -------------
! =====================================================================
    call unsmfi(imate, phi0, cs, t, tbiot,&
                aniso, ndim, phenom)
!
    call dilata(imate, phi0, alphfi, t, aniso,&
                angmas, tbiot, phenom)
!
    call calela(imate, angmas, mdal, dalal, t,&
                aniso, rbid, ndim, phenom)
!
! =====================================================================
! --- SI ABSENCE DE MECANIQUE -----------------------------------------
! =====================================================================
else
    if (aniso .eq. 0) then
! =====================================================================
! --- CALCUL CAS ISOTROPE ---------------------------------------------
! =====================================================================
        alphfi = 0.0d0
        cs = em
        dalal = 0.d0
        alpha0 = 0.0d0
        unsks = em
        do 20 i = 1, 6
            mdal(i) = 0.d0
20      continue
        if (em .lt. eps) then
            cbiot =phi0
            biot(1)=phi0
            biot(2)=phi0
            biot(3)=phi0
            call tebiot(angmas, biot, tbiot, 0, ndim)
        endif
    else if (aniso.eq.1) then
! =====================================================================
! --- CALCUL CAS ISOTROPE TRANSVERSE-----------------------------------
! =====================================================================
        if (ndim .ne. 3) then
            call utmess('F', 'ALGORITH17_38')
        endif
        alphfi = 0.0d0
        cs = em
        dalal = 0.d0
        do 30 i = 1, 6
            mdal(i) = 0.d0
30      continue
        if (em .lt. eps) then
            biot(1)=phi0
            biot(2)=phi0
            biot(3)=phi0
            biot(4)=phi0
            call tebiot(angmas, biot, tbiot, 1, ndim)
        endif
    else if (aniso.eq.2) then
! =====================================================================
! --- CALCUL CAS ORTHO 2D-----------------------------------
! =====================================================================
        if (ndim .ne. 2) then
            call utmess('F', 'ALGORITH17_37')
        endif
        alphfi = 0.0d0
        cs = em
        dalal = 0.d0
        do 40 i = 1, 6
            mdal(i) = 0.d0
40      continue
        if (em .lt. eps) then
            biot(1)=phi0
            biot(2)=phi0
            biot(3)=phi0
            biot(4)=phi0
            call tebiot(angmas, biot, tbiot, 2, ndim)
        endif
    endif
endif
! =====================================================================
! --- CALCUL EPSV AU TEMPS MOINS --------------------------------------
! =====================================================================
    epsvm = epsv - depsv
! =====================================================================
end subroutine
