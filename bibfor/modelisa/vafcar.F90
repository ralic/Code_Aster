subroutine vafcar(tpgz, imclf, nmobjz, nutyel, ntyele, car, ncar,&
                  ivr, kioc, ier)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!       VERIFICATION DE LA BONNE AFFECTATION DES DONNEES :
!         CARAC POUTRE      >  ELEMENT POUTRE
!         CARAC DISCRET     >  ELEMENT DISCRET DE TYPE L OU N
!         CARAC COQUE       >  ELEMENT COQUE
!         CARAC ORIENTATION >  ELEMENT DISCRET OU POUTRE
!         CARAC DEFI_ARC    >  ELEMENT POUTRE COURBE
!         CARAC CABLE       >  ELEMENT CABLE
!         CARAC BARRE       >  ELEMENT BARRE
!         CARAC MASSIF      >  ELEMENT THERMIQUE
!         CARAC GRILLE      >  ELEMENT GRILLE
!         CARAC MEMBRANE    >  ELEMENT MEMBRANE
!
! --------------------------------------------------------------------------------------------------
!
    use cara_elem_parameter_module
    implicit none
    integer :: ntyele(*), ivr(*), ncar, nutyel, ier, imclf
    character(len=6) :: kioc
    character(len=*) :: tpgz, nmobjz, car(*)
!
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: ii, ll0, ll1
    integer :: npd, npf
    character(len=8) :: tpg, nomobj, carz
    character(len=17) :: tpge
    character(len=24) :: valk(4)
! --------------------------------------------------------------------------------------------------
    tpg = tpgz
    nomobj = nmobjz
!
!   Vérification de l'affectation de la maille par un élément
    tpge = tpg//' '//nomobj
    if (nutyel .eq. 0) then
        if ( ivr(1).eq.1 ) then
            valk(1) = kioc
            valk(2) = ACE_MCLEF(imclf)
            valk(3) = tpge
            call utmess('A', 'MODELISA7_63', nk=3, valk=valk)
            ier = ier + 1
        endif
        goto 999
    endif
!
!   Vérification du bon type de l'élément
    if      ( imclf .eq. ACE_POUTRE ) then
        npd = 1
        npf = ACE_NB_POUTRE
    else if ( (imclf.eq.ACE_DISCRET) .or. (imclf.eq.ACE_DISCRET_2D) .or. &
              (imclf.eq.ACE_RIGI_PARASOL) .or. &
              (imclf.eq.ACE_MASS_AJOU) .or. &
              (imclf.eq.ACE_MASS_REP) ) then
        npd = ACE_NB_POUTRE + 1
        npf = ACE_NB_POUTRE + ACE_NB_DISCRET
    else if ( imclf.eq.ACE_ORIENTATION ) then
        npd = 1
        npf = ACE_NB_POUTRE + ACE_NB_DISCRET
    else if ( imclf.eq.ACE_COQUE) then
        npd = ACE_NB_POUTRE + ACE_NB_DISCRET + 1
        npf = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE
    else if ( imclf.eq.ACE_CABLE ) then
        npd = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + 1
        npf = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE
    else if ( imclf.eq.ACE_BARRE ) then
        npd = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + 1
        npf = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + ACE_NB_BARRE
    else if ( imclf.eq.ACE_DEFI_ARC) then
        npd = 4
        npf = 4
    else if ( imclf.eq.ACE_MASSIF ) then
        npd = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + ACE_NB_BARRE + 1
        npf = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + ACE_NB_BARRE + &
              ACE_NB_MASSIF
    else if ( imclf.eq.ACE_GRILLE ) then
        npd = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + ACE_NB_BARRE + &
              ACE_NB_MASSIF + 1
        npf = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + ACE_NB_BARRE + &
              ACE_NB_MASSIF + ACE_NB_GRILLE
    else if ( imclf.eq.ACE_MEMBRANE ) then
        npd = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + ACE_NB_BARRE + &
              ACE_NB_MASSIF + ACE_NB_GRILLE + 1
        npf = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + ACE_NB_BARRE + &
              ACE_NB_MASSIF + ACE_NB_GRILLE + ACE_NB_MEMBRANE
    else
        valk(1) = ACE_MCLEF(imclf)
        call utmess('A', 'MODELISA9_11', sk=valk(1))
    endif
!
    do ii = npd, npf
        if (nutyel .eq. ntyele(ii)) goto 20
    enddo
    if (ivr(1).eq.1) then
        valk(1) = kioc
        valk(2) = ACE_MCLEF(imclf)
        valk(3) = tpge
        call utmess('A', 'MODELISA7_64', nk=3, valk=valk)
        ier = ier + 1
    endif
    goto 999
20  continue
!
! --- CAS PARTICULIER DES ELEMENTS DISCRETS
    if ( (imclf.eq.ACE_DISCRET) .or. (imclf.eq.ACE_DISCRET_2D) ) then
        do ii = 1, ncar
            carz = car(ii)
            if (carz(3:4) .eq. 'T_') then
                ll0 = 1
                ll1 = 5
            else if (carz(3:4).eq.'TR') then
                ll0 = 3
                ll1 = 7
            endif
            if ( ( (carz(5:5).eq.'N' .or. carz(6:6).eq.'N' .or. &
                    carz(7:7).eq.'N' .or. carz(8:8).eq.'N') &
                    .and. (nutyel.ne.ntyele(ACE_NB_POUTRE+ll0) .and. &
                           nutyel.ne.ntyele(ACE_NB_POUTRE+ll1)) ) .or. &
                 ( (carz(5:5).eq.'L' .or. carz(6:6).eq.'L' .or. carz(7:7).eq.'L' .or. &
                    carz(8:8).eq.'L') &
                    .and. (nutyel.ne.ntyele(ACE_NB_POUTRE+1+ll0).and. &
                           nutyel.ne.ntyele(ACE_NB_POUTRE+1+ll1)) ) .or.&
                 ( (carz(5:5).eq.'D' .or. carz(6:6).eq.'D') &
                    .and. (nutyel.ne.ntyele(ACE_NB_POUTRE+ll0) .and. &
                           nutyel.ne.ntyele(ACE_NB_POUTRE+ll1) .and. &
                           nutyel.ne.ntyele(ACE_NB_POUTRE+1+ll0) .and. &
                           nutyel.ne.ntyele(ACE_NB_POUTRE+1+ll1) ) ) .or.&
                 ( (carz(1:5).eq.'M_T_D' .or. carz(1:6).eq.'M_TR_D') &
                    .and. (nutyel.eq.ntyele(ACE_NB_POUTRE+1+ll1) .and. &
                           nutyel.ne.ntyele(ACE_NB_POUTRE+ll1) ) )) then
                if (ivr(1) .eq. 1 ) then
                    valk(1) = kioc
                    valk(2) = ACE_MCLEF(imclf)
                    valk(3) = tpge
                    valk(4) = carz
                    call utmess('A', 'MODELISA7_65', nk=4, valk=valk)
                    ier = ier + 1
                endif
            endif
        enddo
    endif
!
999  continue
end subroutine
