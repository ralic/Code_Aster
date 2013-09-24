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
interface 
    subroutine cremnl(reprise, baseno, numrep, nbordr0, nbordr, nbpt, neq,&
                      nbhar, imat, numedd, parcho, nbchoc, vk8, modrep)
        integer :: nbhar
        integer :: numrep
        integer :: neq
        logical :: reprise
        character(len=8) :: baseno
        integer :: nbordr0
        integer :: nbordr
        integer :: nbpt
        integer :: imat(2)
        character(len=24) :: numedd
        character(len=14) :: parcho
        integer :: nbchoc
        character(len=8) :: vk8
        character(len=8) :: modrep
    end subroutine cremnl
end interface 
