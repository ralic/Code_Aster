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
    subroutine mdallo(nomres, basemo, masgen, riggen, amogen,&
                      nbmode, dt, nbsauv, nbchoc, noecho,&
                      intitu, nbrede, fonred, nbrevi, fonrev,&
                      jdepl, jvite, jacce, jptem, jordr,&
                      jdisc, jfcho, jdcho, jvcho, jadcho,&
                      jredc, jredd, jrevc, jrevv, method,&
                      nbsym, nomsym, typcal, sauve)
        integer :: nbrevi
        integer :: nbrede
        integer :: nbchoc
        character(len=8) :: nomres
        character(len=*) :: basemo
        character(len=*) :: masgen
        character(len=*) :: riggen
        character(len=*) :: amogen
        integer :: nbmode
        real(kind=8) :: dt
        integer :: nbsauv
        character(len=8) :: noecho(nbchoc, *)
        character(len=8) :: intitu(*)
        character(len=8) :: fonred(nbrede, *)
        character(len=8) :: fonrev(nbrevi, *)
        integer :: jdepl
        integer :: jvite
        integer :: jacce
        integer :: jptem
        integer :: jordr
        integer :: jdisc
        integer :: jfcho
        integer :: jdcho
        integer :: jvcho
        integer :: jadcho
        integer :: jredc
        integer :: jredd
        integer :: jrevc
        integer :: jrevv
        character(len=16) :: method
        integer :: nbsym
        character(len=4) :: nomsym(*)
        character(len=4) :: typcal
        character(len=4) :: sauve
    end subroutine mdallo
end interface
