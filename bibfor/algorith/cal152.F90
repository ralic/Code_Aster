subroutine cal152(option, max, may, maz, model,&
                  phib24, iphi1, iphi2, imade, modmec,&
                  chamno, num, vrai, i, j,&
                  mij, cij, kij)
    implicit none
!---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!---------------------------------------------------------------------
! AUTEUR : G.ROUSSEAU
! OPERATEUR CALCULANT LA MASSE AJOUTEE, L'AMORTISSEMENT
!  ET LA RIGIDITE AJOUTEE EN THEORIE POTENTIELLE : CALC_MATR_AJOU
!     SUR BASE MODALE DE LA STRUCTURE DANS LE VIDE
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/calamr.h"
#include "asterfort/calmaj.h"
#include "asterfort/utmess.h"
    logical(kind=1) :: vrai
    integer :: i, j
    integer :: imade
    integer :: iphi1, iphi2
    real(kind=8) :: mij, cij, kij, kij1, cij1, cij2
    real(kind=8) :: valr(2)
    character(len=2) :: model
    character(len=8) :: modmec
    character(len=9) :: option
    character(len=14) :: num
    character(len=19) :: max, may, maz, chamno
    character(len=24) :: phib24
! -----------------------------------------------------------------
    if (option .eq. 'MASS_AJOU') then
        call calmaj(option, max, may, maz, model,&
                    zk24(iphi1+j-1)(1:19), modmec, chamno, num, vrai,&
                    i, j, mij)
    endif
!
    if (option .eq. 'AMOR_AJOU') then
        call calmaj(option, max, may, maz, model,&
                    zk24(iphi2+j-1)(1:19), modmec, chamno, num, vrai,&
                    i, j, cij1)
        call calamr(phib24, zk24(iphi1+j-1)(1:19), zk24(imade+i-1), num, j,&
                    cij2)
        cij=cij1+cij2
        valr (1) = cij1
        valr (2) = cij2
        call utmess('I', 'ALGORITH14_80', nr=2, valr=valr)
    endif
!
    if (option .eq. 'RIGI_AJOU') then
        call calamr(phib24, zk24(iphi2+j-1)(1:19), zk24(imade+i-1), num, j,&
                    kij1)
        kij=kij1
    endif
end subroutine
