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
    subroutine lrmmf1(fid, nomamd, nbrfam, carafa, nbnoeu,&
                      famnoe, nmatyp, jfamma, jnumty, tabaux,&
                      nomgro, numgro, nument, infmed, nivinf,&
                      ifm, vecgrm, nbcgrm)
        integer :: nbnoeu
        integer :: nbrfam
        integer :: fid
        character(len=*) :: nomamd
        integer :: carafa(3, nbrfam)
        integer :: famnoe(nbnoeu)
        integer :: nmatyp(69)
        integer :: jfamma(69)
        integer :: jnumty(69)
        integer :: tabaux(*)
        character(len=*) :: nomgro
        character(len=*) :: numgro
        character(len=*) :: nument
        integer :: infmed
        integer :: nivinf
        integer :: ifm
        character(len=*) :: vecgrm
        integer :: nbcgrm
    end subroutine lrmmf1
end interface
