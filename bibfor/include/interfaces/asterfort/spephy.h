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
    subroutine spephy(ioptch, intphy, intmod, nomu, table,&
                      freq, cham, specmr, specmi, disc,&
                      nnoe, nomcmp, nuor, nbmr, nbn,&
                      imod1, nbpf, nbm, ivitef)
        integer :: nbm
        integer :: nbpf
        integer :: nbn
        integer :: nbmr
        integer :: ioptch
        logical :: intphy
        logical :: intmod
        character(len=8) :: nomu
        character(len=8) :: table
        real(kind=8) :: freq(2, nbm, *)
        real(kind=8) :: cham(nbn, nbmr)
        real(kind=8) :: specmr(nbpf, *)
        real(kind=8) :: specmi(nbpf, *)
        real(kind=8) :: disc(*)
        character(len=8) :: nnoe(nbn)
        character(len=8) :: nomcmp
        integer :: nuor(nbmr)
        integer :: imod1
        integer :: ivitef
    end subroutine spephy
end interface
