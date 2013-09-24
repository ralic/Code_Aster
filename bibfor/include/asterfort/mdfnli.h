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
    subroutine mdfnli(nbmode, depgen, vitgen, accgen, fexgen,&
                      masgen, phicar, pulsa2, amogen, nbchoc,&
                      logcho, dplmod, parcho, noecho, saucho,&
                      nbrede, dplred, fonred, saured, saredi,&
                      nbrevi, dplrev, fonrev, saurev, sarevi,&
                      temps , nofdep, nofvit, nofacc, nbexci, psidel,&
                      monmot, nbrfis, fk    , dfk   , angini,&
                      foncp , numpas, nbpal , dt    , dtsto ,&
                      vrotat, typal , finpal, cnpal , prdeff,&
                      conv  , fsauv)
        integer :: nbexci
        integer :: nbchoc
        integer :: nbmode
        real(kind=8) :: depgen(*)
        real(kind=8) :: vitgen(*)
        real(kind=8) :: accgen(*)
        real(kind=8) :: fexgen(*)
        real(kind=8) :: masgen(*)
        real(kind=8) :: phicar(*)
        real(kind=8) :: pulsa2(*)
        real(kind=8) :: amogen(*)
        integer :: logcho(*)
        real(kind=8) :: dplmod(nbchoc, nbmode, *)
        real(kind=8) :: parcho(*)
        character(len=8) :: noecho(*)
        real(kind=8) :: saucho(*)
        integer :: nbrede
        real(kind=8) :: dplred(*)
        character(len=8) :: fonred(*)
        real(kind=8) :: saured(*)
        integer :: saredi(*)
        integer :: nbrevi
        real(kind=8) :: dplrev(*)
        character(len=8) :: fonrev(*)
        real(kind=8) :: saurev(*)
        integer :: sarevi(*)
        real(kind=8) :: temps
        character(len=8) :: nofdep(nbexci)
        character(len=8) :: nofvit(nbexci)
        character(len=8) :: nofacc(nbexci)
        real(kind=8) :: psidel(nbchoc, nbexci, *)
        character(len=8) :: monmot
        integer :: nbrfis
        character(len=8) :: fk(2)
        character(len=8) :: dfk(2)
        real(kind=8) :: angini
        character(len=8) :: foncp
        integer :: numpas
        integer :: nbpal
        real(kind=8) :: dt
        real(kind=8) :: dtsto
        real(kind=8) :: vrotat
        character(len=6) :: typal(20)
        character(len=3) :: finpal(20)
        character(len=8) :: cnpal(20)
        logical :: prdeff
        real(kind=8) :: conv
        real(kind=8) :: fsauv(20, 3)
    end subroutine mdfnli
end interface
