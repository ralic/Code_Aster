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
    subroutine voiuti(numa, codvoi, nvoima, nscoma, iarepe,&
                      iaddvo, iadvoi, nbvois, livois, tyvois,&
                      nbnovo, nbsoco, lisoco)
        integer :: nscoma
        integer :: nvoima
        integer :: numa
        character(*) :: codvoi
        integer :: iarepe
        integer :: iaddvo
        integer :: iadvoi
        integer :: nbvois
        integer :: livois(1:nvoima)
        integer :: tyvois(1:nvoima)
        integer :: nbnovo(1:nvoima)
        integer :: nbsoco(1:nvoima)
        integer :: lisoco(1:nvoima, 1:nscoma, 1:2)
    end subroutine voiuti
end interface
