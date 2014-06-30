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
subroutine mdallo(nomres, typcal, nbsauv, base, nbmodes,&
                  rigi, mass, amor, jordr, jdisc,&
                  nbsym, nomsym, jdepl, jvite, jacce,&
                  method, dt, jptem, nbchoc, noecho,&
                  intitu, jfcho, jdcho, jvcho, jadcho,&
                  nbrede, fonred, jredc, jredd, nbrevi,&
                  fonrev, jrevc, jrevv, sauve, checkarg)
        character(len=8) , intent(in) :: nomres
        character(len=4) , intent(in) :: typcal
        integer          , intent(in) :: nbsauv
        character(len=*) , optional, intent(in)  :: base
        integer          , optional, intent(in)  :: nbmodes
        character(len=*) , optional, intent(in)  :: rigi, mass, amor
        integer          , optional, intent(out) :: jordr, jdisc
        integer          , optional, intent(in)  :: nbsym
        character(len=4) , optional, intent(in)  :: nomsym(*)
        integer          , optional, intent(out) :: jdepl, jvite, jacce
        character(len=*) , optional, intent(in)  :: method
        real(kind=8)     , optional, intent(in)  :: dt
        integer          , optional, intent(out) :: jptem
        integer          , optional, intent(in)  :: nbchoc
        character(len=8) , optional, intent(in)  :: noecho(*), intitu(*)
        integer          , optional, intent(out) :: jfcho, jdcho, jvcho, jadcho
        integer          , optional, intent(in)  :: nbrede
        character(len=8) , optional, intent(in)  :: fonred(*)
        integer          , optional, intent(out) :: jredc, jredd
        integer          , optional, intent(in)  :: nbrevi
        character(len=8) , optional, intent(in)  :: fonrev(*)
        integer          , optional, intent(out) :: jrevc, jrevv
        character(len=4) , optional, intent(in)  :: sauve
        logical(kind=1)          , optional, intent(in)  :: checkarg
    end subroutine mdallo
end interface
