subroutine rc32fu(nbsigr, nocc, situ, fuij, ke, lieu,&
                  ug, factus, ugenv, fatiguenv)
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
#include "asterfort/rcZ2env.h"
    integer :: nbsigr, nocc(*), situ(*)
    real(kind=8) :: fuij(*), ug, factus(*), ugenv, ke(*)
    character(len=4) :: lieu
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: isk, isl, k, l, nk, nl, n0, i1, ifm, niv, icompt
    real(kind=8) :: fuijm, ukl, fenkl, kekl
    aster_logical :: trouve, fatiguenv
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
    kekl = ke(nbsigr*(isk-1)+isl)
!
    if (icompt .le. 49) then
        icompt = icompt + 1
        factus(6*(icompt-1)+1) = 1
        factus(6*(icompt-1)+2) = situ(isk)
        factus(6*(icompt-1)+3) = situ(isl)
        factus(6*(icompt-1)+4) = ukl
        if (fatiguenv) then
            call rcZ2env(situ(isk), situ(isl), kekl, lieu, fenkl)
        else
            fenkl = 1
        endif 
        factus(6*(icompt-1)+5) = fenkl
        factus(6*(icompt-1)+6) = fenkl*ukl
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
