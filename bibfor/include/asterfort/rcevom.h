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
    subroutine rcevom(csigm, cinst, cnoc, sm, lfatig,&
                      lpmpb, lsn, csno, csne, flexio,&
                      csneo, csnee, cfao, cfae, cspo,&
                      cspe, cresu, kinti, it, jt,&
                      lrocht, symax, cpres, kemixt, cspto,&
                      cspte, cspmo, cspme)
        character(len=24) :: csigm
        character(len=24) :: cinst
        character(len=24) :: cnoc
        real(kind=8) :: sm
        logical(kind=1) :: lfatig
        logical(kind=1) :: lpmpb
        logical(kind=1) :: lsn
        character(len=24) :: csno
        character(len=24) :: csne
        logical(kind=1) :: flexio
        character(len=24) :: csneo
        character(len=24) :: csnee
        character(len=24) :: cfao
        character(len=24) :: cfae
        character(len=24) :: cspo
        character(len=24) :: cspe
        character(len=24) :: cresu
        character(len=16) :: kinti
        integer :: it
        integer :: jt
        logical(kind=1) :: lrocht
        real(kind=8) :: symax
        character(len=24) :: cpres
        logical(kind=1) :: kemixt
        character(len=24) :: cspto
        character(len=24) :: cspte
        character(len=24) :: cspmo
        character(len=24) :: cspme
    end subroutine rcevom
end interface
