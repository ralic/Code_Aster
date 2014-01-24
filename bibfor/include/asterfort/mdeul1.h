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
    subroutine mdeul1(nbpas, dt, neqgen, pulsat, pulsa2,&
                      masgen, descmm, riggen, descmr, rgygen,&
                      lamor, amogen, descma, gyogen, foncv,&
                      fonca, typbas, basemo, tinit, iparch,&
                      nbsauv, nbchoc, logcho, dplmod, parcho,&
                      noecho, nbrede, dplred, fonred, nbrevi,&
                      dplrev, fonrev, depsto, vitsto, accsto,&
                      iorsto, temsto, fchost, dchost, vchost,&
                      ichost, iredst, dredst, irevst, drevst,&
                      coefm, liad, inumor, idescf, nofdep,&
                      nofvit, nofacc, nomfon, psidel, monmot,&
                      nbrfis, fk, dfk, angini, foncp,&
                      nbpal, dtsto, vrotat, prdeff, nomres,&
                      nbexci, passto, intitu )
        integer :: nbchoc
        integer :: neqgen
        integer :: nbpas
        real(kind=8) :: dt
        real(kind=8) :: pulsat(*)
        real(kind=8) :: pulsa2(*)
        real(kind=8) :: masgen(*)
        integer :: descmm
        real(kind=8) :: riggen(*)
        integer :: descmr
        real(kind=8) :: rgygen(*)
        logical :: lamor
        real(kind=8) :: amogen(*)
        integer :: descma
        real(kind=8) :: gyogen(*)
        character(len=8) :: foncv
        character(len=8) :: fonca
        character(len=16) :: typbas
        character(len=8) :: basemo
        real(kind=8) :: tinit
        integer :: iparch(*)
        integer :: nbsauv
        integer :: logcho(nbchoc, *)
        real(kind=8) :: dplmod(nbchoc, neqgen, *)
        real(kind=8) :: parcho(*)
        character(len=8) :: noecho(nbchoc, *)
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
        integer :: irevst(*)
        real(kind=8) :: drevst(*)
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
        integer :: nbrfis
        character(len=8) :: fk(2)
        character(len=8) :: dfk(2)
        real(kind=8) :: angini
        character(len=8) :: foncp
        integer :: nbpal
        real(kind=8) :: dtsto
        real(kind=8) :: vrotat
        logical :: prdeff
        character(len=8) :: nomres
        integer :: nbexci
        real(kind=8) :: passto(*)
        character(len=8) :: intitu(*)
    end subroutine mdeul1
end interface
