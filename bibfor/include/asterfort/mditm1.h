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
    subroutine mditm1(nbm, nbmcd, nbmp, nbnl, indic,&
                      nbf, impr, itrans, epst, icoupl,&
                      tpfl, veci1, locfl0, dt0, tfexm,&
                      ts, iarch, nexcit, tabexc, numexc,&
                      masgi, amori, pulsi, vecr3, phii,&
                      parcho, noecho, intitu, vecr5, vecr1,&
                      vecr2, vgap, vecr4, nbchoc, depg0,&
                      vitg0, xsi0, nbsauv)
        integer :: nbnl
        integer :: nbm
        integer :: nbmcd
        integer :: nbmp
        integer :: indic
        integer :: nbf
        integer :: impr
        integer :: itrans
        real(kind=8) :: epst
        integer :: icoupl
        character(len=8) :: tpfl
        integer :: veci1(*)
        aster_logical :: locfl0(*)
        real(kind=8) :: dt0
        real(kind=8) :: tfexm
        real(kind=8) :: ts
        integer :: iarch
        integer :: nexcit
        character(len=8) :: tabexc(*)
        integer :: numexc(*)
        real(kind=8) :: masgi(*)
        real(kind=8) :: amori(*)
        real(kind=8) :: pulsi(*)
        real(kind=8) :: vecr3(*)
        real(kind=8) :: phii(nbnl, nbm, *)
        real(kind=8) :: parcho(nbnl, *)
        character(len=8) :: noecho(nbnl, *)
        character(len=8) :: intitu(*)
        real(kind=8) :: vecr5(*)
        real(kind=8) :: vecr1(*)
        real(kind=8) :: vecr2(*)
        real(kind=8) :: vgap
        real(kind=8) :: vecr4(*)
        integer :: nbchoc
        real(kind=8) :: depg0(*)
        real(kind=8) :: vitg0(*)
        real(kind=8) :: xsi0(*)
        integer :: nbsauv
    end subroutine mditm1
end interface
