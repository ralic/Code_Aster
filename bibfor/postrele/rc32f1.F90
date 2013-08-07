subroutine rc32f1(nbsigr, nocc, saltij, isk, isl,&
                  nk, nl, n0, nbp12, nbp23,&
                  nbp13, sigr, yapass, typass, nsitup)
    implicit   none
#include "jeveux.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
    integer :: nbsigr, nocc(*), isk, isl, nk, nl, n0, nsitup, nbp12, nbp23
    integer :: nbp13, sigr(*)
    real(kind=8) :: saltij(*)
    logical :: yapass
    character(len=3) :: typass
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL DU FACTEUR D'USAGE POUR LES SITUATIONS DE PASSAGE
!     DETERMINATION DU CHEMIN DE PASSAGE
! OUT : N0     : NOMBRE D'OCCURRENCE
! OUT : YAPASS : UNE SITUATION DE PASSAGE EXISTE
! OUT : TYPASS : PASSAGE D'UNE SITUATION A UNE AUTRE
!                1_2 : PASSAGE GROUPE 1 A GROUPE 2
!                1_2 : PASSAGE GROUPE 2 A GROUPE 3
!                1_3 : PASSAGE GROUPE 1 A GROUPE 3
! OUT : NSITUP : NUMERO DU CHEMIN DE SITUATION DE PASSAGE
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: jsigr, ig1, ig2, nbsips, jnpass, i, k, nsitu, numg1, numg2
    integer :: sipass, npass, ioc1, ioc2
    real(kind=8) :: salmia, salt1, saltam
    logical :: chemin
!     ------------------------------------------------------------------
!
    call jeveuo('&&RC32SI.SITU_GROUP', 'L', jsigr)
!
    ioc1 = sigr(isk)
    ioc2 = sigr(isl)
    numg1 = zi(jsigr+2*ioc1-2)
    ig1 = zi(jsigr+2*ioc1-1)
    numg2 = zi(jsigr+2*ioc2-2)
    ig2 = zi(jsigr+2*ioc2-1)
!
    yapass = .false.
    typass = '?_?'
    nsitup = 0
!
    if (numg1 .eq. numg2) then
! ------ MEME GROUPE
        n0 = min ( nk , nl )
        goto 9999
    else if (numg1 .eq. ig2) then
! ------ MEME GROUPE
        n0 = min ( nk , nl )
        goto 9999
    else if (numg2 .eq. ig1) then
! ------ MEME GROUPE
        n0 = min ( nk , nl )
        goto 9999
    else if (ig1 .eq. ig2) then
! ------ MEME GROUPE
        n0 = min ( nk , nl )
        goto 9999
    endif
!
    if (( numg1.eq.1 .and. numg2.eq.2 ) .or. ( numg1.eq.2 .and. numg2.eq.1 )) then
        if (nbp12 .eq. 0) then
            if (( ig1.eq.1 .and. ig2.eq.3 ) .or. ( ig1.eq.3 .and. ig2.eq.1 )) then
                typass = '1_3'
                yapass = .true.
                elseif ( ( ig1.eq.2 .and. ig2.eq.3 ) .or. ( ig1.eq.3&
            .and. ig2.eq.2 ) ) then
                typass = '2_3'
                yapass = .true.
            endif
        else
            typass = '1_2'
            yapass = .true.
        endif
        elseif ( ( numg1.eq.2 .and. numg2.eq.3 ) .or. ( numg1.eq.3 .and.&
    numg2.eq.2 ) ) then
        if (nbp23 .eq. 0) then
            if (( ig1.eq.1 .and. ig2.eq.2 ) .or. ( ig1.eq.2 .and. ig2.eq.1 )) then
                typass = '1_2'
                yapass = .true.
                elseif ( ( ig1.eq.1 .and. ig2.eq.3 ) .or. ( ig1.eq.3&
            .and. ig2.eq.1 ) ) then
                typass = '1_3'
                yapass = .true.
            endif
        else
            typass = '2_3'
            yapass = .true.
        endif
        elseif ( ( numg1.eq.1 .and. numg2.eq.3 ) .or. ( numg1.eq.3 .and.&
    numg2.eq.1 ) ) then
        if (nbp13 .eq. 0) then
            if (( ig1.eq.1 .and. ig2.eq.2 ) .or. ( ig1.eq.2 .and. ig2.eq.1 )) then
                typass = '1_2'
                yapass = .true.
                elseif ( ( ig1.eq.2 .and. ig2.eq.3 ) .or. ( ig1.eq.3&
            .and. ig2.eq.2 ) ) then
                typass = '2_3'
                yapass = .true.
            endif
        else
            typass = '1_3'
            yapass = .true.
        endif
    endif
!
! --- RECHERCHE DU CHEMIN DE PASSAGE
!
    call jelira('&&RC32SI.PASSAGE_'//typass, 'LONUTI', nbsips)
    call jeveuo('&&RC32SI.PASSAGE_'//typass, 'L', jnpass)
    chemin = .false.
    salmia = 1.d+50
    do 10 i = 1, nbsips
        sipass = zi(jnpass+i-1)
        do 12 k = 1, nbsigr
            if (sigr(k) .eq. sipass) then
                ioc1 = k
                goto 14
            endif
12      continue
        call u2mess('F', 'POSTRCCM_36')
14      continue
        npass = nocc(ioc1)
        if (npass .eq. 0) goto 10
        chemin = .true.
! --------- ON RECHERCHE LE MIN DES SALT MAX
        saltam = 0.d0
        do 16 k = 1, nbsigr
            salt1 = saltij(nbsigr*(k-1)+ioc1)
            if (salt1 .gt. saltam) then
                saltam = salt1
                nsitu = ioc1
            endif
16      continue
        if (saltam .lt. salmia) then
            salmia = saltam
            nsitup = nsitu
        endif
10  end do
    if (chemin) then
        npass = nocc(nsitup)
        n0 = min ( nk , nl, npass )
    else
        yapass = .false.
        n0 = min ( nk , nl )
    endif
!
9999  continue
!
end subroutine
