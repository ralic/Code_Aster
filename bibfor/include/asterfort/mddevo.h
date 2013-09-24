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
    subroutine mddevo(nbpas, dt, nbmode, pulsat, pulsa2,&
                      masgen, amogen, basemo, tinit, iparch,&
                      nbsauv, nbchoc, logcho, dplmod, parcho,&
                      noecho, nbrede, dplred, fonred,&
                      nbrevi, dplrev, fonrev, depsto, vitsto,&
                      accsto, iorsto, temsto, fchost, dchost,&
                      vchost, ichost, iredst, dredst, coefm,&
                      liad, inumor, idescf, nofdep, nofvit,&
                      nofacc, nomfon, psidel, monmot, nomres,&
                      nbexci, passto, irevst, drevst)
        integer :: nbchoc
        integer :: nbmode
        integer :: nbpas
        real(kind=8) :: dt
        real(kind=8) :: pulsat(*)
        real(kind=8) :: pulsa2(*)
        real(kind=8) :: masgen(*)
        real(kind=8) :: amogen(*)
        character(len=8) :: basemo
        real(kind=8) :: tinit
        integer :: iparch(*)
        integer :: nbsauv
        integer :: logcho(*)
        real(kind=8) :: dplmod(nbchoc, nbmode, *)
        real(kind=8) :: parcho(*)
        character(len=8) :: noecho(*)
        integer :: nbrede
        real(kind=8) :: dplred(*)
        character(len=8) :: fonred(*)
        integer :: nbrevi
        real(kind=8) :: dplrev(*)
        character(len=8) :: fonrev(*)
        real(kind=8) :: depsto(*)
        real(kind=8) :: vitsto(*)
        real(kind=8) :: accsto(*)
        integer :: iorsto(*)
        real(kind=8) :: temsto(*)
        real(kind=8) :: fchost(*)
        real(kind=8) :: dchost(*)
        real(kind=8) :: vchost(*)
        integer :: ichost(*)
        integer :: iredst(*)
        real(kind=8) :: dredst(*)
        real(kind=8) :: coefm(*)
        integer :: liad(*)
        integer :: inumor(*)
        integer :: idescf(*)
        character(len=8) :: nofdep(*)
        character(len=8) :: nofvit(*)
        character(len=8) :: nofacc(*)
        character(len=8) :: nomfon(*)
        real(kind=8) :: psidel(*)
        character(len=8) :: monmot
        character(len=8) :: nomres
        integer :: nbexci
        real(kind=8) :: passto(*)
        integer :: irevst(*)
        real(kind=8) :: drevst(*)
    end subroutine mddevo
end interface
