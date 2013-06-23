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
    subroutine paqnoe(nomsd, nomu, nommai, nommet, nomcri,&
                      nomfor, grdvie, forvie, fordef, typcha,&
                      proaxe, instic, inscri, prec)
        character(len=8) :: nomsd
        character(len=8) :: nomu
        character(len=8) :: nommai
        character(len=16) :: nommet
        character(len=16) :: nomcri
        character(len=16) :: nomfor
        character(len=8) :: grdvie
        character(len=16) :: forvie
        logical :: fordef
        character(len=16) :: typcha
        character(len=16) :: proaxe
        real(kind=8) :: instic
        character(len=8) :: inscri
        real(kind=8) :: prec
    end subroutine paqnoe
end interface
