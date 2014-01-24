subroutine mdarnl(isto1, ipas, t, dt, nbmode,&
                  depgen, vitgen, accgen, isto2, nbchoc,&
                  saucho, nbscho, isto3, nbrede, saured,&
                  saredi, isto4, nbrevi, saurev, sarevi,&
                  depsto, vitsto, accsto, passto, iorsto,&
                  temsto, fchost, dchost, vchost, ichost,&
                  vint, iredst, dredst, irevst, drevst)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=W1504
!
    implicit none
!
#include "asterfort/mdarch.h"
#include "asterfort/mdtr74grd.h"
!
! --------------------------------------------------------------------------------------------------
!
!     ARCHIVAGE DES DES CHAMPS GENERALISES OBLIGATOIRES ET OPTIONELS
!     POUR LA SD_DYNA_GENE DANS LE CADRE D'UN CALCUL TRANSITOIRE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iorsto(*), iredst(*), saredi(*), irevst(*), sarevi(*)
    integer :: ichost(*)
    real(kind=8) :: depgen(*), vitgen(*), accgen(*), depsto(*), vitsto(*)
    integer :: nbchoc
    real(kind=8) :: saucho(nbchoc, *), saured(*), dredst(*), saurev(*)
    real(kind=8) :: drevst(*), passto(*), accsto(*), temsto(*), fchost(*)
    real(kind=8) :: dchost(*), vchost(*)
    real(kind=8) :: vint(*)
    integer :: ic, ind, ipas, ird, irv, isto1, isto2, ndec, nbscho, ii
    integer :: isto3, isto4, nbmode, nbrede, nbrevi, nbvari, dimschor
    character(len=4) :: typcal, kbid
    real(kind=8) :: dt, t
    complex(kind=8) :: cbid
!-----------------------------------------------------------------------
    typcal = 'TRAN'
!
!   enregistrement des données obligatoires
    cbid=dcmplx(0.d0,0.d0)
    call mdarch(isto1, ipas, t, dt, nbmode,&
                typcal, 0, [kbid], depgen, vitgen,&
                accgen, depsto, vitsto, accsto, [cbid],&
                [cbid], [cbid], [cbid], [cbid], [cbid],&
                passto, iorsto, temsto)
!
!   enregistrement des données optionelles
    if (nbchoc .ne. 0) then
        ndec = nbchoc*isto1
!
        dimschor= mdtr74grd('SCHOR')
        nbvari = mdtr74grd('MAXVINT')
        ind = nbchoc*isto1*nbvari
        do ic = 1, nbchoc
            isto2 = isto2 + 1
            fchost(isto2) = saucho(ic,1)
            dchost(isto2) = saucho(ic,4)
            vchost(isto2) = saucho(ic,7)
            dchost(nbscho+isto2) = saucho(ic,10)
            isto2 = isto2 + 1
            fchost(isto2) = saucho(ic,2)
            dchost(isto2) = saucho(ic,5)
            vchost(isto2) = saucho(ic,8)
            dchost(nbscho+isto2) = saucho(ic,11)
            isto2 = isto2 + 1
            fchost(isto2) = saucho(ic,3)
            dchost(isto2) = saucho(ic,6)
            vchost(isto2) = saucho(ic,9)
            dchost(nbscho+isto2) = saucho(ic,12)
            ichost(ndec+ic) = nint(saucho(ic,13))
!           variables internes
!           V(nbchoc,Vint,nbsauv)
            do ii= 1, nbvari
                vint(ic+(ii-1)*nbchoc+ind) = saucho(ic,dimschor+ii)
            enddo
        enddo
    endif
!
!   cas des relations effort-déplacement
    if (nbrede .ne. 0) then
        do ird = 1, nbrede
            isto3 = isto3 + 1
            iredst(isto3) = saredi(ird)
            dredst(isto3) = saured(ird)
        enddo
    endif
!
!   cas des relations effort-vitesse
    if (nbrevi .ne. 0) then
        do irv = 1, nbrevi
            isto4 = isto4 + 1
            irevst(isto4) = sarevi(irv)
            drevst(isto4) = saurev(irv)
        enddo
    endif
!
end subroutine
