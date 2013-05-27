subroutine projmp(np1, np2, nbm, nbnl, phii,&
                  accg, vitg, depg, acc, vit,&
                  dep)
!
! ********************************************************************
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ********************************************************************
! DESCRIPTION : RECOMBINAISON EN BASE PHYSIQUE AUX NOEUDS DE CHOC
! ------------
!
! ****************** DECLARATION DES VARIABLES ***********************
!
    implicit none
!
! ARGUMENTS
! ---------
    include 'asterfort/projmg.h'
    integer :: np1, np2
    integer :: nbm, nbnl
    real(kind=8) :: accg(*), vitg(*), depg(*)
    real(kind=8) :: acc(3, *), vit(3, *), dep(3, *)
    real(kind=8) :: phii(np2, np1, 3)
!
!
! VARIABLES LOCALES
! -----------------
    integer :: ic
    real(kind=8) :: aglo(3), xglo(3), vglo(3)
!
! ****************** DEBUT DU CODE EXECUTABLE ************************
!
! 1. BOUCLE SUR LES NON-LINEARITES.
!    ------------------------------
!
    do 10 ic = 1, nbnl
!
!       (ACCELERATIONS)
        call projmg(np1, np2, ic, nbm, phii,&
                    accg, aglo)
        acc(1,ic) = aglo(1)
        acc(2,ic) = aglo(2)
        acc(3,ic) = aglo(3)
!       (VITESSES)
        call projmg(np1, np2, ic, nbm, phii,&
                    vitg, vglo)
        vit(1,ic) = vglo(1)
        vit(2,ic) = vglo(2)
        vit(3,ic) = vglo(3)
!       (DEPLACEMENTS)
        call projmg(np1, np2, ic, nbm, phii,&
                    depg, xglo)
        dep(1,ic) = xglo(1)
        dep(2,ic) = xglo(2)
        dep(3,ic) = xglo(3)
!
10  end do
!
end subroutine
