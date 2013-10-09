subroutine acevd2(noma, nomo, mcf, lmax, nbocc)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/acevtr.h"
#include "asterfort/assert.h"
#include "asterfort/crlinu.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/verdis.h"
#include "asterfort/wkvect.h"
!
    integer :: lmax, nbocc
    character(len=8) :: noma, nomo
    character(len=*) :: mcf
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
! --- ------------------------------------------------------------------
!        AFFE_CARA_ELEM
!           TEST DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET
! --- ------------------------------------------------------------------
! IN
!     NOMA     : NOM DU MAILLAGE
!     NOMO     : NOM DU MODELE
!     MCF      :  MOT CLEF
!     LMAX     : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
!     NBOCC    : NOMBRE D'OCCURENCES DU MOT CLE DISCRET
! --- ------------------------------------------------------------------
    integer :: nbcar
    parameter     (nbcar=100)
    integer :: ier, i3d, i2d, ndim, nbmtrd, ioc, ng, nm, nj, nn, ncar, icar
    integer :: ii, nbma, ialima, nbnogr, kk
    integer :: ixnw, jdnw, jddi, jdls, jdgn
    character(len=8) :: k8b, nomu, car(nbcar)
    character(len=16) :: concep, cmd
    character(len=24) :: tmpdis, mlggno, mlgnno, modnem, grpma
    integer :: iarg
! --- ------------------------------------------------------------------
!
    call jemarq()
    call getres(nomu, concep, cmd)
    tmpdis = nomu//'.DISCRET'
    mlggno = noma//'.GROUPENO'
    mlgnno = noma//'.NOMNOE'
    modnem = nomo//'.MODELE    .NEMA'
    grpma = noma//'.GROUPEMA       '
!
! --- VERIFICATION DES DIMENSIONS / MODELISATIONS
    ier = 0
    call verdis(nomo, noma, 'F', i3d, i2d,&
                ndim, ier)
    ASSERT((mcf.eq.'DISCRET_2D').or.(mcf.eq.'DISCRET'))
!
    call jeexin(modnem, ixnw)
    nbmtrd = 0
    if (ixnw .ne. 0) then
        call jelira(modnem, 'NMAXOC', nbmtrd)
        call jeveuo(modnem, 'L', jdnw)
        call wkvect(tmpdis, 'V V I', nbmtrd, jddi)
    endif
    call wkvect('&&TMPDISCRET', 'V V K24', lmax, jdls)
!
!
! --- BOUCLE SUR LES OCCURENCES DE DISCRET
    do ioc = 1, nbocc
        call getvem(noma, 'GROUP_MA', mcf, 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', mcf, 'MAILLE', ioc,&
                    iarg, lmax, zk24( jdls), nm)
        call getvem(noma, 'GROUP_NO', mcf, 'GROUP_NO', ioc,&
                    iarg, lmax, zk24(jdls), nj)
        call getvem(noma, 'NOEUD', mcf, 'NOEUD', ioc,&
                    iarg, lmax, zk24(jdls), nn)
        call getvtx(mcf, 'CARA', iocc=ioc, nbval=nbcar, vect=car,&
                    nbret=ncar)
!
        if (ncar .gt. ncar) ASSERT(.false.)
        do icar = 1, ncar
            if (car(icar)(3:4) .eq. 'TR') goto 28
        end do
!
        goto 30
 28     continue
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
        if (ng .gt. 0) then
            do ii = 1, ng
                call jelira(jexnom(grpma, zk24(jdls+ii-1)), 'LONUTI', nbma)
                call jeveuo(jexnom(grpma, zk24(jdls+ii-1)), 'L', ialima)
                call acevtr(noma, nomo, 2, zk24(1), zi(ialima),&
                            nbma, ndim)
            end do
        endif
!
! ---   "MAILLE" = TOUTES LES MAILLES  DE LA LISTE DE MAILLES
        if (nm .gt. 0) then
            call acevtr(noma, nomo, 1, zk24(jdls), zi(1),&
                        nm, ndim)
        endif
!
! ---    SI DES MAILLES TARDIVES EXISTENT POUR CE MODELE :
        if (ixnw .ne. 0) then
! ---       "GROUP_NO" = TOUTES LES MAILLES TARDIVES  DE LA LISTE
!                                                  DE GROUPES DE NOEUDS
            if (nj .gt. 0) then
                do ii = 1, nj
                    call jeveuo(jexnom(mlggno, zk24(jdls+ii-1)), 'L', jdgn)
                    call jelira(jexnom(mlggno, zk24(jdls+ii-1)), 'LONUTI', nbnogr)
                    call crlinu('NUM', mlgnno, nbnogr, zi(jdgn), k8b,&
                                nbmtrd, zi(jdnw), zi(jddi), kk)
                    if (kk .gt. 0) then
                        call acevtr(noma, nomo, 2, zk24(1), zi(jddi),&
                                    kk, ndim)
                    endif
                end do
            endif
! ---       "NOEUD" = TOUTES LES MAILLES TARDIVES  DE LA LISTE DE NOEUDS
            if (nn .gt. 0) then
                call crlinu('NOM', mlgnno, nn, [0], zk24(jdls),&
                            nbmtrd, zi(jdnw), zi(jddi), kk)
                if (kk .gt. 0) then
                    call acevtr(noma, nomo, 2, zk24(1), zi(jddi),&
                                kk, ndim)
                endif
            endif
        endif
 30     continue
    end do
!
    if (ixnw .ne. 0) call jedetr(tmpdis)
    call jedetr('&&TMPDISCRET')
!
    call jedema()
end subroutine
