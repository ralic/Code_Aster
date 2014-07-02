!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine vpsorn(lmasse, ldynfa, nbeq, nbvect, nfreq,&
                      tolsor, vect, resid, workd, workl,&
                      lonwl, selec, dsor, fshift, vaux,&
                      workv, ddlexc, ddllag, neqact, maxitr,&
                      ifm, niv, priram, alpha, omecor,&
                      nconv, flage, solveu)
        integer :: lonwl
        integer :: nfreq
        integer :: nbvect
        integer :: nbeq
        integer :: lmasse
        integer :: ldynfa
        real(kind=8) :: tolsor
        real(kind=8) :: vect(nbeq, nbvect)
        real(kind=8) :: resid(nbeq)
        real(kind=8) :: workd(3*nbeq)
        real(kind=8) :: workl(lonwl)
        aster_logical :: selec(nbvect)
        real(kind=8) :: dsor(nfreq+1, 2)
        real(kind=8) :: fshift
        real(kind=8) :: vaux(nbeq)
        real(kind=8) :: workv(3*nbvect)
        integer :: ddlexc(nbeq)
        integer :: ddllag(nbeq)
        integer :: neqact
        integer :: maxitr
        integer :: ifm
        integer :: niv
        integer :: priram(8)
        real(kind=8) :: alpha
        real(kind=8) :: omecor
        integer :: nconv
        aster_logical :: flage
        character(len=19) :: solveu
    end subroutine vpsorn
end interface
