subroutine rc32fu(nbsigr, nocc, nom, situ, sigr, fuij, comb, lieu,&
                  ug, factus, factus2, ugenv, fatiguenv)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/infniv.h"
#include "asterfort/rc32f0.h"
#include "asterfort/rc32f2.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexnom.h"
#include "asterfort/codent.h"
#include "asterfort/rc32env.h"
    integer :: nbsigr, nocc(*), situ(*), sigr(*)
    real(kind=8) :: fuij(*), ug, factus(*), ugenv, comb(*)
    character(len=4) :: lieu
    character(len=24) :: factus2(*), nom(*)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!
!     CALCUL DU FACTEUR D'USAGE
!
!     ------------------------------------------------------------------
    integer :: isk, isl, k, l, nk, nl, n0, i1, ifm, niv, icompt, ndim
    real(kind=8) :: fuijm, ukl, fenkl, kekl
    aster_logical :: trouve, fatiguenv
    integer :: ioc1, ioc2, numg1, numg2
    integer, pointer :: situ_group(:) => null()
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
    if (niv .ge. 2) then
        write(ifm,*) 'MATRICE FACTEUR D USAGE INITIALE'
        write(ifm,1012) ( situ(l),l=1,nbsigr )
        write(ifm,1010) ( nocc(l),l=1,nbsigr )
        do 100 k = 1, nbsigr
            i1 = nbsigr*(k-1)
            write(ifm,1000) situ(k), nocc(k), (fuij(i1+l),l=1,nbsigr)
100     continue
    endif
!
    icompt = 0
    ug = 0.d0
    ugenv = 0.d0
!
 10 continue
    fuijm = 0.d0
    trouve = .false.
!
! --- RECHERCHE DU SALT MAXI
!
    call rc32f0(nbsigr, nocc, fuij, fuijm, trouve,&
                isk, isl, nk, nl)
!
    if (.not. trouve) goto 9999
!
    n0 = min ( nk , nl )
    ukl = dble( n0 ) * fuijm
    ndim = nbsigr*nbsigr
    kekl = comb(5*ndim+nbsigr*(isk-1)+isl)
!
    call jeveuo('&&RC32SI.SIT_GROUP', 'L', vi=situ_group)
    ioc1 = sigr(isk)
    ioc2 = sigr(isl)
    numg1 = situ_group(1+2*ioc1-2)
    numg2 = situ_group(1+2*ioc2-2)
!
    if (icompt .le. 49) then
        if (fatiguenv) then
            call rc32env(situ(isk), situ(isl), kekl, lieu, fenkl)
        else
            fenkl = 1
        endif 
!
        icompt = icompt + 1
        factus(23*(icompt-1)+1) = 1
        factus(23*(icompt-1)+2) = situ(isk)
        factus(23*(icompt-1)+3) = nk
        factus(23*(icompt-1)+4) = numg1
        factus(23*(icompt-1)+5) = situ(isl)
        factus(23*(icompt-1)+6) = nl
        factus(23*(icompt-1)+7) = numg2
        factus(23*(icompt-1)+8) = n0
!
        factus(23*(icompt-1)+9) = comb(nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+10) = comb(1*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+11) = comb(2*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+12) = comb(3*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+13) = comb(4*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+14) = comb(6*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+15) = comb(7*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+16) = comb(8*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+17) = comb(9*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+18) = comb(10*ndim+nbsigr*(isk-1)+isl)
        factus(23*(icompt-1)+19) = comb(11*ndim+nbsigr*(isk-1)+isl)
!
        factus(23*(icompt-1)+20) = fuijm
        factus(23*(icompt-1)+21) = ukl
        factus(23*(icompt-1)+22) = fenkl
        factus(23*(icompt-1)+23) = fenkl*ukl
!
        factus2(2*(icompt-1)+1) = nom(isk)
        factus2(2*(icompt-1)+2) = nom(isl)
    endif
!
    if (niv .ge. 2) then
        write(ifm,1040)'=> FU MAXI = ', fuijm, situ(isk), situ(isl)
        write(ifm,1030)'          N0 = ', n0
        write(ifm,1020)'         UKL = ', ukl
    endif
!
! --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT SUIVANT
!     LE NOMBRE D'OCCURENCE EGAL A ZERO
!
    call rc32f2(nbsigr, nocc, fuij, isk, isl,&
                nk, nl, n0)
!
    if (niv .ge. 2) then
        write(ifm,*) 'MATRICE FACTEUR D USAGE MODIFIEE'
        write(ifm,1012) ( situ(l),l=1,nbsigr )
        write(ifm,1010) ( nocc(l),l=1,nbsigr )
        do 110 k = 1, nbsigr
            i1 = nbsigr*(k-1)
            write(ifm,1000) situ(k), nocc(k), (fuij(i1+l),l=1,nbsigr)
110     continue
    endif
!
    ug = ug + ukl
    ugenv = ugenv+fenkl*ukl
    goto 10
!
9999 continue
!
    1000 format(1p,i7,i9,'|',40(e9.2,'|'))
    1010 format(1p,7x,'NB_OCCUR ','|',40(i9,'|'))
    1012 format(1p,7x,'SITUATION','|',40(i9,'|'))
    1040 format(1p,a15,e12.5,', LIGNE:',i4,', COLONNE:',i4)
    1030 format(1p,a15,i12)
    1020 format(1p,a15,e12.5)
!
end subroutine
