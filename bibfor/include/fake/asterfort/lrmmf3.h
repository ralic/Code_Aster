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
    subroutine lrmmf3(fid, nomamd, rangfa, carafa, nbnoeu,&
                      famnoe, nmatyp, jfamma, jnumty, vaatfa,&
                      nogrfa, tabaux, nomgro, numgro, nument,&
                      infmed, nivinf, ifm, vecgrm, nbcgrm,&
                      nbgrlo)
        integer :: nbnoeu
        integer :: fid
        character(*) :: nomamd
        integer :: rangfa
        integer :: carafa(3, *)
        integer :: famnoe(nbnoeu)
        integer :: nmatyp(69)
        integer :: jfamma(69)
        integer :: jnumty(69)
        integer :: vaatfa(*)
        character(*) :: nogrfa(*)
        integer :: tabaux(*)
        character(*) :: nomgro
        character(*) :: numgro
        character(*) :: nument
        integer :: infmed
        integer :: nivinf
        integer :: ifm
        character(*) :: vecgrm
        integer :: nbcgrm
        integer :: nbgrlo
    end subroutine lrmmf3
end interface
