! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine irecri(nomcon, form, ifi, titre, lgmsh,&
                      nbcham, cham, partie, nbpara, para,&
                      nbordr, ordr, lresu, motfac, iocc,&
                      cecr, tycha, lcor, nbnot, numnoe,&
                      nbmat, nummai, nbcmp, nomcmp, lsup,&
                      borsup, linf, borinf, lmax, lmin,&
                      formr, nive, versio)
        character(len=*) :: nomcon
        character(len=*) :: form
        integer :: ifi
        character(len=*) :: titre
        aster_logical :: lgmsh
        integer :: nbcham
        character(len=*) :: cham(*)
        character(len=*) :: partie
        integer :: nbpara
        character(len=*) :: para(*)
        integer :: nbordr
        integer :: ordr(*)
        aster_logical :: lresu
        character(len=*) :: motfac
        integer :: iocc
        character(len=*) :: cecr
        character(len=8) :: tycha
        aster_logical :: lcor
        integer :: nbnot
        integer :: numnoe(*)
        integer :: nbmat
        integer :: nummai(*)
        integer :: nbcmp
        character(len=*) :: nomcmp(*)
        aster_logical :: lsup
        real(kind=8) :: borsup
        aster_logical :: linf
        real(kind=8) :: borinf
        aster_logical :: lmax
        aster_logical :: lmin
        character(len=*) :: formr
        integer :: nive
        integer :: versio
    end subroutine irecri
end interface
