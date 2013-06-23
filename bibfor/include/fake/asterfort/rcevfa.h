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
    subroutine rcevfa(nommat, para, sm, cnoc, csno,&
                      csne, cspo, cspe, kemixt, cspto,&
                      cspte, cspmo, cspme, cfao, cfae)
        character(len=8) :: nommat
        real(kind=8) :: para(3)
        real(kind=8) :: sm
        character(len=24) :: cnoc
        character(len=24) :: csno
        character(len=24) :: csne
        character(len=24) :: cspo
        character(len=24) :: cspe
        logical :: kemixt
        character(len=24) :: cspto
        character(len=24) :: cspte
        character(len=24) :: cspmo
        character(len=24) :: cspme
        character(len=24) :: cfao
        character(len=24) :: cfae
    end subroutine rcevfa
end interface
