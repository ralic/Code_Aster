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
interface
    subroutine xassha_frac(nddls, nddlm, nnop, nnops,&
                           lact, elrefp, elrefc, elc, contac,&
                           dimuel, nface, npgf, nbspg, nptf,&
                           jcohes, jptint, igeom, jbasec,&
                           nlact, cface, fpg, ncompv,&
                           compor, jmate, ndim, idepm, idepd, jcoheo, incoca,&
                           pla, rela, algocr, jheavn, ncompn,&
                           ifiss, nfiss, nfh, jheafa, ncomph,&
                           pos)
                           
        integer :: nddls
        integer :: nddlm
        integer :: nnop
        integer :: nnops
        integer :: lact(16)
        character(len=8) :: elrefp
        character(len=8) :: elrefc
        character(len=8) :: elc
        integer :: contac
        integer :: dimuel
        integer :: nface
        integer :: npgf
        integer :: nbspg
        integer :: nptf
        integer :: jcohes
        integer :: jptint
        integer :: igeom
        integer :: jbasec
        integer :: nlact(2)
        integer :: cface(30,6)
        character(len=8) :: fpg
        integer :: ncompv
        character(len=16) :: compor(*)
        integer :: jmate
        integer :: ndim
        integer :: idepm
        integer :: idepd
        integer :: jcoheo
        integer :: incoca
        integer :: pla(27)
        real(kind=8) :: rela
        integer :: algocr
        integer :: jheavn
        integer :: ncompn
        integer :: ifiss
        integer :: nfiss
        integer :: nfh
        integer :: jheafa
        integer :: ncomph
        integer :: pos(16)
    end subroutine xassha_frac
end interface
