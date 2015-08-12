subroutine glrc_recup_mate(imate, compor, lrgm, ep, lambda, deuxmu, lamf, deumuf, gt, gc, gf, &
                           seuil, alpha, alfmc)
! person_in_charge: sebastien.fayolle at edf.fr
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
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
    aster_logical, intent(in) :: lrgm
    character(len=16), intent(in) :: compor
    integer, intent(in) :: imate
    real(kind=8), intent(in) :: ep
    real(kind=8), optional, intent(out) :: lambda, deuxmu, deumuf, lamf
    real(kind=8), optional, intent(out) :: gt, gc, gf, seuil, alpha, alfmc
! ----------------------------------------------------------------------
!
! BUT : LECTURE DES PARAMETRES MATERIAU POUR LE MODELE GLRC_DM
!
!
! IN:
!       IMATE   : ADRESSE DU MATERIAU
!       COMPOR  : COMPORTMENT
!       EP      : EPAISSEUR DE LA PLAQUE
! OUT:
!       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
!       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
!       LAMF    : PARAMETRE D ELASTICITE - FLEXION
!       DEUMUF  : PARAMETRE D ELASTICITE - FLEXION
!       GT      : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION
!       GC      : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
!       GF      : PARAMETRE GAMMA POUR LA FLEXION
!       SEUIL   : INITIAL MEMBRANE
!       ALPHA   : PARAMETRE DE SEUIL FLEXION
!       ALFMC   : PARAMETRE DE DECOUPLAGE SEUILS TRACTION-COMPRESSION
! ----------------------------------------------------------------------
!
    integer :: icodre(7)
    real(kind=8) :: valres(7), e, nu, ef, nycmax, rmesg(2)
    real(kind=8) :: nyt, nyc, myf, nuf, delas(6, 6)
    real(kind=8) :: lambda_int, deuxmu_int, deumuf_int, lamf_int
    real(kind=8) :: gt_int, gc_int, gf_int, seuil_int, alpha_int, alfmc_int
    character(len=16) :: nomres(7)
!
    if ((.not.( compor(1:7) .eq. 'GLRC_DM'))) then
        call utmess('F', 'ELEMENTS4_65', sk=compor)
    endif
!
    call r8inir(6*6, 0.0d0, delas, 1)
    call r8inir(7, 0.0d0, valres, 1)
!
!    LECTURE DES CARACTERISTIQUES DU MATERIAU
    nomres(1) = 'E_M'
    nomres(2) = 'NU_M'
!
    call rcvala(imate, ' ', 'ELAS_GLRC', 0, ' ',&
                    [0.d0], 2, nomres, valres, icodre,1)
!
    e = valres(1)
    nu = valres(2)
    lambda_int = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)*ep
    deuxmu_int = e/(1.d0+nu)*ep
!
    nomres(1) = 'E_F'
    nomres(2) = 'NU_F'
!
    call rcvala(imate, ' ', 'ELAS_GLRC', 0, ' ',&
                [0.d0], 2, nomres, valres, icodre,&
                0)
!
    if (icodre(1) .eq. 0) then
        ef = valres(1)
    else
        ef = e
    endif
!
    if (icodre(2) .eq. 0) then
        nuf = valres(2)
    else
        nuf = nu
    endif
!
    lamf_int = ef*nuf/(1.d0-nuf*nuf) *ep**3/12.0d0
    deumuf_int = ef/(1.d0+nuf) *ep**3/12.0d0
!
!    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
    nomres(1) = 'GAMMA_T'
    nomres(2) = 'GAMMA_C'
    nomres(3) = 'GAMMA_F'
    nomres(4) = 'NYT'
    nomres(5) = 'NYC'
    nomres(6) = 'MYF'
    nomres(7) = 'ALPHA_C'
    call rcvala(imate, ' ', 'GLRC_DM', 0, ' ',&
                [0.d0], 7, nomres, valres, icodre,&
                0)
!
    gt_int = valres(1)
    gf_int = valres(3)
    nyt = valres(4)
    myf = valres(6)
    alfmc_int = valres(7)
!
    if (icodre(2) .eq. 0 .and. icodre(5) .eq. 0) then
! - ON EST DANS LE CAS DE DEFI_GLRC
        gc_int = valres(2)
        nyc = valres(5)
    else if (icodre(2).eq.0) then
! - ON EST DANS LE CAS DE DEFI_MATERIAU
        gc_int = valres(2)
!
        if (gc_int .eq. 1.d0 .and. gt_int .eq. 1.d0) then
            call utmess('F', 'ALGORITH6_1')
        endif
!
        nyc = (1.d0-nu)*(1.d0+2.d0*nu)*(1.d0-gt_int)+nu**2*(1.d0-gc_int)
        nyc = nyc/((1.d0-nu)*(1.d0+2.d0*nu)*(1.d0-gc_int)+nu**2*(1.d0-gt_int))
        nyc = -sqrt(nyc*nyt**2)
    else if (icodre(5).eq.0) then
! - ON EST DANS LE CAS DE DEFI_MATERIAU
        nyc = valres(5)
        nycmax = nyt*sqrt((1.d0-nu)*(1.d0+2.d0*nu))/nu
!
        if (valres(5) .gt. nycmax) then
            rmesg(1) = nyc
            rmesg(2) = nycmax
            call utmess('F', 'ALGORITH6_2', nr=2, valr=rmesg)
        endif
!
        gc_int = (1.d0-gt_int)*(nyt**2*(1.d0-nu)*(1.d0+2.d0*nu)-nyc**2*nu**2)
        gc_int = gc_int/(nyc**2*(1.d0-nu)*(1.d0+2.d0*nu)-nyt**2*nu**2)
        gc_int = 1.d0 - gc_int
    else
        gc_int = 1.d0
    endif
!
    if (gc_int .eq. 1.d0 .and. gt_int .eq. 1.d0) then
        call utmess('F', 'ALGORITH6_1')
    endif
!
    if (icodre(7) .eq. 0 .and. gc_int .ne. 1.d0) then
        alfmc_int = valres(7)
    else
        if (gc_int .eq. 1.d0) then
            alfmc_int = 1.d0
        else
            alfmc_int=(1.d0-gc_int)*(nyc**2*(1.d0-nu)*(1.d0+2.d0*nu)/nyt**2-nu**2)&
                    /((1.d0-gt_int)*((1.d0-nu)*(1.d0+2.d0*nu)-(nu*nyc/nyt)**2))
        endif
    endif
!
!    CALCUL DU SEUIL (k0 DANS R7.01.32) ET DE ALPHA
    if (lrgm) then
        alpha_int = 1.d0
        alfmc_int = 1.d0
        seuil_int = 0.d0
    else
        seuil_int = lambda_int*(1.0d0 - gt_int)*(1.0d0-2.0d0*nu)**2 &
                  + deuxmu_int*( 1.0d0 - gt_int + (1.0d0 - gc_int)*nu**2/alfmc_int)
!
        seuil_int = seuil_int/(2.0d0*(lambda_int*(1.0d0-2.0d0*nu) + deuxmu_int))**2
        seuil_int = seuil_int*nyt**2
!
        if (seuil_int .ne. 0.d0) then
            alpha_int = lamf_int*(1.0d0-nuf)**2 + deumuf_int
            alpha_int = alpha_int/(2.0d0*(lamf_int*(1.0d0-nuf) + deumuf_int)**2)
            alpha_int = alpha_int*(1.0d0 - gf_int)*myf**2/seuil_int
        else
            call utmess('F', 'ALGORITH6_3')
        endif
    endif
!
    if (present(lambda)) then
        lambda = lambda_int
    endif
    if (present(deuxmu)) then
        deuxmu = deuxmu_int
    endif
    if (present(deumuf)) then
        deumuf = deumuf_int
    endif
    if (present(lamf)) then
        lamf = lamf_int
    endif
    if (present(gt)) then
        gt = gt_int
    endif
    if (present(gc)) then
        gc = gc_int
    endif
    if (present(gf)) then
        gf = gf_int
    endif
    if (present(seuil)) then
        seuil = seuil_int
    endif
    if (present(alpha)) then
        alpha = alpha_int
    endif
    if (present(alfmc)) then
        alfmc = alfmc_int
    endif
!
end subroutine
