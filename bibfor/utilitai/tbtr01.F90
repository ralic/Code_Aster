subroutine tbtr01(tabin, nbpara, nopara, nblign, nume)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbtri.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbpara, nblign, nume(*)
    character(len=*) :: tabin, nopara
! ----------------------------------------------------------------------
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
!     TRI D'UNE TABLE.
! ----------------------------------------------------------------------
! IN  : TABIN  : NOM DE LA TABLE DONT ON VEUT TRIER DES LIGNES
! IN  : NBPARA : NOMBRE DE PARAMETRE DE LA TABLE "TABIN"
! IN  : NOPARA : LA PARAMETRE A TRIER
! IN  : NBLIGN : NOMBRE DE LIGNES A TRIER
! VAR : NUME   : IN  : NUMERO DES LIGNES A TRIER DANS "TABIN"
!                OUT : LES NUMEROS DANS UN ORDRE CROISSANT
!                      LES "VIDE" EN TETE
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: jtblp, jnuvi,  i, j, jvale, jvall, nbvid, nbnvd
    integer :: lvale, jnume
    character(len=4) :: type
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, inpar, jnpar
    integer, pointer :: n_vide(:) => null()
    integer, pointer :: tri(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = tabin
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
!
    call wkvect('&&TBTR01.NUME  ', 'V V I', nblign, jnume)
    call wkvect('&&TBTR01.VIDE  ', 'V V I', nblign, jnuvi)
    AS_ALLOCATE(vi=n_vide, size=nblign)
    do i = 1, nblign
        zi(jnume+i-1) = nume(i)
    end do
!
    inpar = nopara
    do j = 1, nbpara
        jnpar = zk24(jtblp+4*(j-1))
        if (inpar .eq. jnpar) then
            type = zk24(jtblp+4*(j-1)+1)(1:4)
            nomjv = zk24(jtblp+4*(j-1)+2)
            nomjvl = zk24(jtblp+4*(j-1)+3)
            call jeveuo(nomjv, 'L', jvale)
            call jeveuo(nomjvl, 'L', jvall)
            nbvid = 0
            nbnvd = 0
            do i = 1, nblign
                if (zi(jvall+nume(i)-1) .eq. 0) then
                    nbvid = nbvid + 1
                    zi(jnuvi+nbvid-1) = nume(i)
                else
                    nbnvd = nbnvd + 1
                    n_vide(nbnvd) = nume(i)
                endif
            end do
            goto 102
        endif
     end do
102  continue
!
    if (nbnvd .eq. 0) goto 999
!
    AS_ALLOCATE(vi=tri, size=nbnvd)
!
    if (type(1:1) .eq. 'I') then
        call wkvect('&&TBTR01.VALEUR', 'V V I', nbnvd, lvale)
        do i = 1, nbnvd
            zi(lvale+i-1) = zi(jvale+n_vide(i)-1)
        end do
        call tbtri(nbnvd, tri, tabchi=zi(lvale))
        call jedetr('&&TBTR01.VALEUR')
    else if (type(1:1) .eq. 'R') then
        call wkvect('&&TBTR01.VALEUR', 'V V R', nbnvd, lvale)
        do i = 1, nbnvd
            zr(lvale+i-1) = zr(jvale+n_vide(i)-1)
        end do
        call tbtri(nbnvd, tri, tabchr=zr(lvale))
        call jedetr('&&TBTR01.VALEUR')
    else if (type(1:3) .eq. 'K80') then
        call wkvect('&&TBTR01.VALEUR', 'V V K80', nbnvd, lvale)
        do i = 1, nbnvd
            zk80(lvale+i-1) = zk80(jvale+n_vide(i)-1)
        end do
        call tbtri(nbnvd, tri, tabchk=zk80(lvale))
        call jedetr('&&TBTR01.VALEUR')
    else if (type(1:3) .eq. 'K32') then
        call wkvect('&&TBTR01.VALEUR', 'V V K32', nbnvd, lvale)
        do i = 1, nbnvd
            zk32(lvale+i-1) = zk32(jvale+n_vide(i)-1)
        end do
        call tbtri(nbnvd, tri, tabchk=zk32(lvale))
        call jedetr('&&TBTR01.VALEUR')
    else if (type(1:3) .eq. 'K24') then
        call wkvect('&&TBTR01.VALEUR', 'V V K24', nbnvd, lvale)
        do i = 1, nbnvd
            zk24(lvale+i-1) = zk24(jvale+n_vide(i)-1)
        end do
        call tbtri(nbnvd, tri, tabchk=zk24(lvale))
        call jedetr('&&TBTR01.VALEUR')
    else if (type(1:3) .eq. 'K16') then
        call wkvect('&&TBTR01.VALEUR', 'V V K16', nbnvd, lvale)
        do i = 1, nbnvd
            zk16(lvale+i-1) = zk16(jvale+n_vide(i)-1)
        end do
        call tbtri(nbnvd, tri, tabchk=zk16(lvale))
        call jedetr('&&TBTR01.VALEUR')
    else if (type(1:2) .eq. 'K8') then
        call wkvect('&&TBTR01.VALEUR', 'V V K8', nbnvd, lvale)
        do i = 1, nbnvd
            zk8(lvale+i-1) = zk8(jvale+n_vide(i)-1)
        end do
        call tbtri(nbnvd, tri, tabchk=zk8(lvale))
        call jedetr('&&TBTR01.VALEUR')
    endif
!
    do i = 1, nbvid
        nume(i) = 0
    end do
!
    do i = 1, nbnvd
        nume(nbvid+i) = zi(jnume+tri(i)-1)
    end do
!
    AS_DEALLOCATE(vi=tri)
!
999   continue
!
    call jedetr('&&TBTR01.NUME  ')
    call jedetr('&&TBTR01.VIDE  ')
    AS_DEALLOCATE(vi=n_vide)
!
    call jedema()
end subroutine
