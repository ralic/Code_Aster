subroutine rvfmai(courbe, listma)
    implicit none
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: listma
    character(len=8) :: courbe
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     SAISIE DES MAILLES POUR LE POST-TRAITEMENT D' UNE COURBE
!     ------------------------------------------------------------------
! IN  COURBE : K : NOM DU CONCEPT DE TYPE COURBE
! IN  LISTMA : K : NOM DE L' OJB  S V I REALISANT LA SAISIE
!     ------------------------------------------------------------------
!     LE VECTEUR LISTMA EST DE DIMENSION NBR_MAILLE_ACTIVE ET CONTIENT
!     LES NUMEROS DES MAILLES ACTIVES DANS L' ORDRE CROISSANT
!     ------------------------------------------------------------------
!
!
!
    character(len=8) :: typcrb, nmaila
    character(len=14) :: nmail1, nmail2
    character(len=24) :: nmail
    integer :: adr, pt, nbma, nbtma, alstma, m, p, nbpart, im, i
    integer :: adrm1, nbmail, adrm2, d3,  vlccou
    character(len=24), pointer :: nsds(:) => null()
!
!================= CORPS DE LA ROUTINE ===============================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    nbma = 0
    pt = 1
    call jeexin(courbe//'.NOMA', d3)
    if (d3 .ne. 0) then
        call jeveuo(courbe//'.NOMA', 'L', adr)
    else
        call jeveuo(courbe//'.NOMMAIL', 'L', adr)
    endif
    nmaila = zk8(adr)
    call dismoi('NB_MA_MAILLA', nmaila, 'MAILLAGE', repi=nbtma)
    call wkvect('&&RVFMAI.VEC.TEMP', 'V V I', nbtma, adr)
    do i = 1, nbtma, 1
        zi(adr + i-1) = 0
    end do
    if (d3 .ne. 0) then
        call jelira(courbe//'.NSDS', 'LONMAX', nbpart)
        call jeveuo(courbe//'.NSDS', 'L', vk24=nsds)
        do p = 1, nbpart, 1
            nmail = nsds(p)(1:13)//'.MAIL'
            call jeveuo(jexatr(nmail, 'LONCUM'), 'L', vlccou)
            call jelira(nmail, 'NMAXOC', nbmail)
            call jeveuo(nmail, 'L', adrm1)
            do im = 1, zi(vlccou + nbmail)-1, 1
                m = zi(adrm1 + im-1)
                zi(adr + m-1) = 1
            end do
        end do
    else
        call jeveuo(courbe//'.TYPCOURBE', 'L', im)
        typcrb = zk8(im)
        nmail1 = courbe//'.MAIL1'
        nmail2 = courbe//'.MAIL2'
        call jelira(nmail1, 'NMAXOC', nbpart)
        do p = 1, nbpart, 1
            call jelira(jexnum(nmail1, p), 'LONMAX', nbmail)
            call jeveuo(jexnum(nmail1, p), 'L', adrm1)
            call jeveuo(jexnum(nmail2, p), 'L', adrm2)
            if (typcrb .eq. 'LISTMAIL') then
                nbmail = nbmail - 1
            endif
            do im = 1, nbmail, 1
                m = zi(adrm1 + im-1)
                zi(adr + m-1) = 1
                m = zi(adrm2 + im-1)
                if (m .gt. 0) then
                    zi(adr + m-1) = 1
                endif
            end do
        end do
    endif
!
    do i = 1, nbtma, 1
        nbma = nbma + max(0,zi(adr + i-1))
    end do
    call wkvect(listma, 'V V I', nbma, alstma)
    do i = 1, nbtma, 1
        if (zi(adr + i-1) .ne. 0) then
            zi(alstma + pt-1) = i
            pt = pt + 1
        endif
    end do
    call jedetr('&&RVFMAI.VEC.TEMP')
    call jedema()
end subroutine
