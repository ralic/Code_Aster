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
    subroutine cglect(resu, modele, ndim, option, cas,&
                      typfis, nomfis, fonoeu, chfond, basfon,&
                      taillr, conf, lnoff, liss, ndeg, typdis)
        character(len=8) :: resu
        character(len=8) :: modele
        integer :: ndim
        character(len=16) :: option
        character(len=16) :: cas
        character(len=8) :: typfis
        character(len=8) :: nomfis
        character(len=24) :: fonoeu
        character(len=24) :: chfond
        character(len=24) :: basfon
        character(len=24) :: taillr
        character(len=8) :: conf
        integer :: lnoff
        character(len=24) :: liss
        integer :: ndeg
        character(len=16) :: typdis
    end subroutine cglect
end interface
