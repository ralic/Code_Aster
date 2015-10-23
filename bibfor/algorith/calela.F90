subroutine calela(imate, angmas, mdal, dalal, t,&
                  aniso, d, ndim, phenom)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#include "asterc/r8dgrd.h"
#include "asterfort/dpassa.h"
#include "asterfort/matini.h"
#include "asterfort/matrot.h"
#include "asterfort/rcvala.h"
#include "asterfort/utbtab.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
    integer :: i, j, ndim, irep
    integer :: aniso
    integer :: dim2, dim1, dim3, imate
    parameter   ( dim1   =  4 )
    parameter   ( dim2   =  8 )
    parameter   ( dim3   =  11 )
    integer :: icodr3(dim3)
    integer :: icodr2(dim2)
    integer :: icodr1(dim1)
    real(kind=8) :: elas3(dim3)
    real(kind=8) :: elas2(dim2)
    real(kind=8) :: elas1(dim1)
    real(kind=8) :: zero, un
    real(kind=8) :: young, nu, g
    real(kind=8) :: young1, young3, nu12, nu21, nu13, nu31, nu23, nu32
    real(kind=8) :: young2, alpha1, alpha3, g13, g12
    real(kind=8) :: d(6, 6), al(6), t, dorth(6, 6), c1, delta, repere(7)
    real(kind=8) :: mdal(6), dalal, angmas(3), work(6, 6), bid(3)
    real(kind=8) :: tetaro, phirot, passag(6, 6), pass(3, 3), tal(3, 3), talg(3, 3)
    character(len=8) :: ncra2(dim2), ncra3(dim3)
    character(len=8) :: ncra1(dim1)
    character(len=16) :: phenom
!
! =====================================================================
! --- DONNEES POUR RECUPERER LES CARACTERISTIQUES MECANIQUES ----------
! =====================================================================
    data ncra1 /'E','NU','ALPHA','RHO'/
    data ncra2 /'E_L','E_N','NU_LT','NU_LN','G_LN',&
     &            'ALPHA_L','ALPHA_N','RHO'/
    data ncra3 /'E_L','E_N','E_T','NU_LT','NU_LN','NU_TN',&
     &            'G_LT','ALPHA_L','ALPHA_T','ALPHA_N','RHO'/
! ======================================================================
! --- INITIALISATIONS --------------------------------------------------
! ======================================================================
    phirot = angmas(1)
    tetaro = angmas(2)
!
    zero = 0.0d0
    un = 1.0d0
!
    call vecini(3, zero, bid)
    call vecini(6, zero, mdal)
    call vecini(6, zero, al)
    call vecini(7, zero, repere)
    call matini(6, 6, zero, d)
    call matini(3, 3, zero, tal)
    call matini(3, 3, zero, talg)
    call matini(6, 6, zero, passag)
    call matini(6, 6, zero, dorth)
    call matini(6, 6, zero, work)
!
! matrice de passage du local au global
    call matrot(angmas, pass)
! ======================================================================
! ----   CALCUL DE LA MATRICE DE PASSAGE DU REPERE LOCAL D'ORTHOTROPIE A
! ----   REPERE GLOBAL POUR LE TENSEUR D'ELASTICITE
!
    repere(1) = 1.d0
    repere(2) = angmas(1)
    repere(3) = angmas(2)
    repere(4) = angmas(3)
!
!
    call dpassa(bid, repere, irep, passag)
! ======================================================================
! --- ON CALCUL DANS UN PREMIERS TEMPS LA MATRICE DE COMPLAISANCE ET ---
! --- LE TENSEUR DE DILATATION TERMIQUE DANS LE REPERE LOCAL ----------
! ======================================================================
! ======================================================================
! --- CALCUL DE LA MATRICE DE COMPLAISANCE DANS LE REPERE LOCAL -------
! ======================================================================
! ======================================================================
! --- CALCUL CAS ISOTROPE ----------------------------------------------
! ======================================================================
    if (aniso .eq. 0) then
        call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                    [t], 3, ncra1(1), elas1(1), icodr1,&
                    0)
        young = elas1(1)
        nu = elas1(2)
        g = young/(2.d0*(1.d0+nu))
!        alpha1 = elas1(3)
!        alpha3 = elas1(3)
        al(1)  =elas1(3)
        al(2)  =elas1(3)
        al(3)  =elas1(3)
!
        dorth(1,1) = young*(1.d0-nu)/ ((1.d0+nu)*(1.-2.d0*nu))
        dorth(2,2) = dorth(1,1)
        dorth(3,3) = dorth(1,1)
!
        dorth(1,2) = young*nu/ ((1.d0+nu)*(1.d0-2.d0*nu))
        dorth(1,3) = dorth(1,2)
        dorth(2,1) = dorth(1,2)
        dorth(2,3) = dorth(1,2)
        dorth(3,1) = dorth(1,2)
        dorth(3,2) = dorth(1,2)
!
        dorth(4,4) = g
        dorth(5,5) = dorth(4,4)
        dorth(6,6) = dorth(4,4)
! ======================================================================
! --- CALCUL CAS ISOTROPE TRANSVERSE------------------------------------
! ======================================================================
    else if (aniso.eq.1) then
        if (phenom .eq. 'ELAS') then
            call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                        [t], 3, ncra1(1), elas1(1), icodr1,&
                        0)
            young1 = elas1(1)
            young3 = elas1(1)
            nu12 = elas1(2)
            nu13 = elas1(2)
            g13 = young1/(2.d0*(1.d0+nu12))
            alpha1 = elas1(3)
            alpha3 = elas1(3)
            al(1)  =elas1(3)
            al(2)  =elas1(3)
            al(3)  =elas1(3)
!
        else if (phenom.eq.'ELAS_ISTR') then
            if (ndim .ne. 3) then
                call utmess('F', 'ALGORITH17_35')
            endif
            call rcvala(imate, ' ', 'ELAS_ISTR', 1, 'TEMP',&
                        [t], 7, ncra2(1), elas2(1), icodr2,&
                        0)
            young1 = elas2(1)
            young3 = elas2(2)
            nu12 = elas2(3)
            nu13 = elas2(4)
            g13 = elas2(5)
            tal(1,1) = elas2(6)
            tal(2,2) = elas2(6)
            tal(3,3) = elas2(7)
!            alpha3 = elas2(7)
            al(1) = talg(1,1)
            al(2) = talg(2,2)
            al(3) = talg(3,3)
            al(4) = talg(1,2)
            al(5) = talg(1,3)
            al(6) = talg(2,3)
        endif
        nu31 = nu13*young3/young1
        c1 = young1/ (1.d0+nu12)
        delta = 1. - nu12 - 2.d0*nu13*nu31
        dorth(1,1) = c1* (1.d0-nu13*nu31)/delta
        dorth(1,2) = c1* ((1.d0-nu13*nu31)/delta-1.d0)
        dorth(1,3) = young3*nu13/delta
        dorth(2,1) = dorth(1,2)
        dorth(2,2) = dorth(1,1)
        dorth(2,3) = dorth(1,3)
        dorth(3,1) = dorth(1,3)
        dorth(3,2) = dorth(2,3)
        dorth(3,3) = young3* (1.d0-nu12)/delta
        dorth(4,4) = 0.5d0*c1
        dorth(5,5) = g13
        dorth(6,6) = dorth(5,5)
! ======================================================================
! --- CALCUL CAS ORTHOTROPE 2D ------------------------------------
! ======================================================================
    else if (aniso.eq.2) then
        if (phenom .eq. 'ELAS') then
            call rcvala(imate, ' ', 'ELAS', 1, 'TEMP',&
                        [t], 3, ncra1(1), elas1(1), icodr1,&
                        0)
            young1 = elas1(1)
            young2 = elas1(1)
            young3 = elas1(1)
            nu12 = elas1(2)
            nu13 = elas1(2)
            nu23 = elas1(2)
            g12 = young1/(2.d0*(1.d0+nu12))
            al(1)  =elas1(3)
            al(2)  =elas1(3)
            al(3)  =elas1(3)
        else if (phenom.eq.'ELAS_ORTH') then
            if (ndim .ne. 2) then
                call utmess('F', 'ALGORITH17_36')
            endif
            call rcvala(imate, ' ', 'ELAS_ORTH', 1, 'TEMP',&
                        [t], 10, ncra3(1), elas3(1), icodr3,&
                        0)
            young1 = elas3(1)
            young3 = elas3(2)
            young2 = elas3(3)
            nu12 = elas3(4)
            nu13 = elas3(5)
            nu23 = elas3(6)
            g12 = elas3(7)
            tal(1,1) = elas3(8)
            tal(2,2) = elas3(9)
            tal(3,3) = elas3(10)
            call utbtab('ZERO', 3, 3, tal, pass,&
                        work, talg)
            al(1) = talg(1,1)
            al(2) = talg(2,2)
            al(3) = talg(3,3)
            al(4) = talg(1,2)
            al(5) = talg(1,3)
            al(6) = talg(2,3)
!
        endif
        nu21 = nu12*young2/young1
        nu31 = nu13*young3/young1
        nu32 = nu23*young3/young2
        delta = un-nu23*nu32-nu31*nu13-nu21*nu12-2.d0*nu23*nu31*nu12
!
        dorth(1,1) = (un - nu23*nu32)*young1/delta
        dorth(1,2) = (nu21 + nu31*nu23)*young1/delta
        dorth(1,3) = (nu31 + nu21*nu32)*young1/delta
!
        dorth(2,1) = dorth(1,2)
        dorth(2,2) = (un - nu13*nu31)*young2/delta
        dorth(2,3) = (nu32 + nu31*nu12)*young2/delta
!
        dorth(3,1) = (nu13+nu12*nu23)*young3/delta
        dorth(3,2) = (nu23+nu21*nu13)*young3/delta
        dorth(3,3) = (un - nu21*nu12)*young3/delta
!
        dorth(4,4) = g12
        dorth(5,5) = g12
        dorth(6,6) = g12
!
    endif
! ======================================================================
! --- PASSAGE DANS LE REPERE GLOBAL -------
! ======================================================================
! Calcul de la matrice de passage
!
    d = dorth
    if ((aniso.eq.1) .or. (aniso.eq.2)) then
        if (irep .eq. 1) then
            call utbtab('ZERO', 6, 6, dorth, passag,&
                        work, d)
        endif
    endif
!
! ======================================================================
! --- CALCUL DES PRODUITS TENSORIELS D:AL:AL ET DE D:AL-----------------
! ======================================================================
    dalal= 0.d0
    do 40 i = 1, 6
        do 50 j = 1, 6
            mdal(i) = mdal(i) +d(i,j)*al(j)
50      continue
40  end do
    do 60 i = 1, 6
        dalal=dalal+mdal(i)*al(i)
60  end do
end subroutine
