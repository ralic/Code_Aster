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
    subroutine mdarch(typcal, isto1, ipas, disc, nbmode,&
                      iorsto, discst, dt, depger, vitger,&
                      accger, depstr, vitstr, accstr, passto,&
                      nbsym, nomsym, depgec, vitgec, accgec,&
                      depstc, vitstc, accstc)
        character(len=4) , intent(in)  :: typcal
        integer          , intent(in)  :: isto1
        integer          , intent(in)  :: ipas
        real(kind=8)     , intent(in)  :: disc
        integer          , intent(in)  :: nbmode
        integer                        :: iorsto(*)
        real(kind=8)                   :: discst(*)
        real(kind=8)     , optional, intent(in)  :: dt
        real(kind=8)     , optional, intent(in)  :: depger(nbmode)
        real(kind=8)     , optional, intent(in)  :: vitger(nbmode)
        real(kind=8)     , optional, intent(in)  :: accger(nbmode)
        real(kind=8)     , optional              :: depstr(*)
        real(kind=8)     , optional              :: vitstr(*)
        real(kind=8)     , optional              :: accstr(*)
        real(kind=8)     , optional              :: passto(*)
        integer          , optional, intent(in)  :: nbsym
        character(len=4) , optional, intent(in)  :: nomsym(*)
        complex(kind=8)  , optional, intent(in)  :: depgec(nbmode)
        complex(kind=8)  , optional, intent(in)  :: vitgec(nbmode)
        complex(kind=8)  , optional, intent(in)  :: accgec(nbmode)
        complex(kind=8)  , optional              :: depstc(*)
        complex(kind=8)  , optional              :: vitstc(*)
        complex(kind=8)  , optional              :: accstc(*)   
    end subroutine mdarch
end interface
