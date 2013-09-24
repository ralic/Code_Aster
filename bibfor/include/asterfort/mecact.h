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
    subroutine mecact(base, nomcar, moclez, nomco, nomgdz,&
                      ncmp, nomcmp,  si, sr, sc, sk,&
                            lnomcmp, vi, vr, vc, vk         )
        integer, intent(in) :: ncmp
        character(len=*), intent(in) :: base
        character(len=*), intent(in) :: nomcar
        character(len=*), intent(in) :: moclez
        character(len=*), intent(in) :: nomco
        character(len=*), intent(in) :: nomgdz
        character(len=*), intent(in), optional :: nomcmp, lnomcmp(ncmp)
        integer, intent(in), optional :: si, vi(ncmp)
        real(kind=8), intent(in), optional :: sr, vr(ncmp)
        complex(kind=8), intent(in), optional :: sc, vc(ncmp)
        character(len=*), intent(in), optional :: sk, vk(ncmp)
    end subroutine mecact
end interface
