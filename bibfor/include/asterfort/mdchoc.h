!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine mdchoc(nbnli, nbchoc, nbflam, nbsism, nbrfis,&
                      nbpal, logcho, dplmod, parcho, paincho, noecho,&
                      intitu, ps1del, ps2del, numddl, nbmode,&
                      pulsat, masgen, lamor, amogen, bmodal,&
                      neq, nexcit, info, monmot, ier)
        integer :: nexcit
        integer :: neq
        integer :: nbmode
        integer :: nbnli
        integer :: nbchoc
        integer :: nbflam
        integer :: nbsism(3)
        integer :: nbrfis
        integer :: nbpal
        integer :: logcho(nbnli, *)
        real(kind=8)        :: dplmod(nbnli, nbmode, *)
        real(kind=8)        :: parcho(nbnli, *)
        integer             :: paincho(nbnli, *)
        character(len=8)    :: noecho(nbnli, *)
        character(len=8)    :: intitu(*)
        real(kind=8)        :: ps1del(neq, nexcit)
        real(kind=8)        :: ps2del(nbnli, nexcit, *)
        character(len=14)   :: numddl
        real(kind=8)        :: pulsat(*)
        real(kind=8)        :: masgen(*)
        aster_logical       :: lamor
        real(kind=8)        :: amogen(*)
        real(kind=8)        :: bmodal(neq, *)
        integer             :: info
        character(len=8)    :: monmot
        integer             :: ier
    end subroutine mdchoc
end interface
