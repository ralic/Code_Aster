subroutine rc32fp(nbsigr, nocc, situ, sigr, fuij,&
                  ke, lieu, ug, factus, ugenv, fatiguenv)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/infniv.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rc32f0.h"
#include "asterfort/rc32f1.h"
#include "asterfort/rc32f2.h"
#include "asterfort/rc32f3.h"
#include "asterfort/rc32f4.h"
#include "asterfort/rc32f5.h"
#include "asterfort/rc32f6.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexnom.h"
#include "asterfort/codent.h"
#include "asterfort/rcZ2env.h"
    integer :: nbsigr, nocc(*), situ(*), sigr(*)
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
!     ROUTINE IDENTIQUE A RC32FU
!
!     CALCUL DU FACTEUR D'USAGE POUR LES SITUATIONS DE PASSAGE
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: isk, isl, k, l, nk, nl, n0, i1, nsitup, ifm, niv, icompt
    integer :: nbsg1, nbsg2, nbsg3, nbp12, nbp23, nbp13
    real(kind=8) :: fumax, ukl, fenkl, kekl
    aster_logical :: trouve, yapass, fatiguenv
    character(len=3) :: typass
    integer, pointer :: passage_sit(:) => null()
    character(len=8) :: knume1, knume2
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
    call jeveuo('&&RC32SI.PASSAGE_SIT', 'L', vi=passage_sit)
    call jelira('&&RC32SI.PASSAGE_1_2', 'LONUTI', nbp12)
    call jelira('&&RC32SI.PASSAGE_2_3', 'LONUTI', nbp23)
    call jelira('&&RC32SI.PASSAGE_1_3', 'LONUTI', nbp13)
    nbsg1 = passage_sit(1)
    nbsg2 = passage_sit(2)
    nbsg3 = passage_sit(3)
!
! --- MISE A ZERO DES LIGNES ET COLONNES DE LA MATRICE SALT
!     S'IL N'EXISTE PAS DE SITUATIONS DE PASSAGE
!
    call rc32f5(nbp12, nbp23, nbp13, nbsigr, nbsg1,&
                nbsg2, nbsg3, fuij)
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
    fumax = 0.d0
    trouve = .false.
!
! --- RECHERCHE DU SALT MAXI
!
    call rc32f0(nbsigr, nocc, fuij, fumax, trouve,&
                isk, isl, nk, nl)
!
    if (.not. trouve) goto 9999
!
! --- DETERMINATION DU N0
!     RECHERCHE DES CHEMINS DE PASSAGE
!
    call rc32f1(nbsigr, nocc, fuij, isk, isl,&
                nk, nl, n0, nbp12, nbp23,&
                nbp13, sigr, yapass, typass, nsitup)
!
    ukl = dble( n0 ) * fumax
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
        if (yapass) then
            write(ifm,1040)'=> FU MAXI = ',fumax,situ(isk),situ(isl),&
            typass,situ(nsitup)
        else
            write(ifm,1042)'=> FU MAXI = ',fumax,situ(isk),situ(isl)
        endif
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
! --- IDEM POUR LE CHEMIN DE PASSAGE
!
    if (yapass) then
        nocc(nsitup) = max(0,nocc(nsitup)-n0)
        if (nocc(nsitup) .eq. 0) then
            if (typass .eq. '1_2') then
                nbp12 = nbp12 - 1
            else if (typass .eq. '1_3') then
                nbp13 = nbp13 - 1
            else if (typass .eq. '2_3') then
                nbp23 = nbp23 - 1
            endif
        endif
        call rc32f3(nbsigr, nocc, fuij, nsitup)
        call rc32f4(typass, nbp12, nbp23, nbp13, nbsigr,&
                    nbsg1, nbsg2, nbsg3, fuij)
    endif
!
! --- ON VERIFIE SI LA COMBINAISON A ANNULEE DES CHEMINS DE PASSAGE
    call rc32f6(nbp12, nbp23, nbp13, nbsigr, nbsg1,&
                nbsg2, nbsg3, sigr, nocc, fuij)
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
    ugenv = ugenv + fenkl*ukl
    goto 10
!
9999 continue
!
    1000 format(1p,i7,i9,'|',40(e9.2,'|'))
    1010 format(1p,7x,'NB_OCCUR ','|',40(i9,'|'))
    1012 format(1p,7x,'SITUATION','|',40(i9,'|'))
    1040 format(1p,a15,e12.5,', LIGNE:',i4,', COLONNE:',i4,&
     &       ', PASSAGE: ',a3,', SITUATION DE PASSAGE: ',i4)
    1042 format(1p,a15,e12.5,', LIGNE:',i4,', COLONNE:',i4)
    1030 format(1p,a15,i12)
    1020 format(1p,a15,e12.5)
!
end subroutine
