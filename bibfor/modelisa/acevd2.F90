subroutine acevd2(noma, nomo, mcf, lmax, nbocc)
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
    implicit none
    integer :: lmax, nbocc
    character(len=8) :: noma, nomo
    character(len=*) :: mcf
!
! --------------------------------------------------------------------------------------------------
!
!        AFFE_CARA_ELEM
!           TEST DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET
!
! --------------------------------------------------------------------------------------------------
!
! IN
!     NOMA     : NOM DU MAILLAGE
!     NOMO     : NOM DU MODELE
!     MCF      :  MOT CLEF
!     LMAX     : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
!     NBOCC    : NOMBRE D'OCCURENCES DU MOT CLE DISCRET
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/acevtr.h"
#include "asterfort/assert.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/verdis.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nbcar=100
    integer :: ier, i3d, i2d, ndim, ioc, ng, nm, ncar, icar
    integer :: ii, nbma, ialima
    character(len=8) :: nomu, car(nbcar)
    character(len=16) :: concep, cmd
    character(len=24) :: tmpdis, mlgnno, grpma
    integer :: iarg
! --------------------------------------------------------------------------------------------------
    character(len=24), pointer :: zjdls(:) => null()
! --------------------------------------------------------------------------------------------------
!
    call getres(nomu, concep, cmd)
    tmpdis = nomu//'.DISCRET'
    mlgnno = noma//'.NOMNOE'
    grpma  = noma//'.GROUPEMA       '
!
!   Vérification des dimensions / modélisations
    ier = 0
    call verdis(nomo, noma, 'F', i3d, i2d, ndim, ier)
    ASSERT((mcf.eq.'DISCRET_2D').or.(mcf.eq.'DISCRET'))
!
    AS_ALLOCATE(vk24=zjdls, size=lmax)
!
!   Boucle sur les occurences
    do ioc = 1, nbocc
        call getvem(noma, 'GROUP_MA', mcf, 'GROUP_MA', ioc, iarg, lmax, zjdls, ng)
        call getvem(noma, 'MAILLE',   mcf, 'MAILLE',   ioc, iarg, lmax, zjdls, nm)
        call getvtx(mcf,  'CARA', iocc=ioc, nbval=nbcar, vect=car, nbret=ncar)
!
        if (ncar .gt. ncar) then
            ASSERT(.false.)
        endif
        do icar = 1, ncar
            if (car(icar)(3:4) .eq. 'TR') then
!               GROUP_MA = toutes les mailles de tous les groupes de mailles
                if (ng .gt. 0) then
                    do ii = 1, ng
                        call jelira(jexnom(grpma, zjdls(ii)), 'LONUTI', nbma)
                        call jeveuo(jexnom(grpma, zjdls(ii)), 'L', ialima)
                        call acevtr(noma, nomo, 2, zk24(1), zi(ialima), nbma, ndim)
                    enddo
                endif
!               MAILLE = toutes les mailles  de la liste de mailles
                if (nm .gt. 0) then
                    call acevtr(noma, nomo, 1, zjdls, zi(1), nm, ndim)
                endif
            endif
        enddo
    enddo
!
    AS_DEALLOCATE(vk24=zjdls)
end subroutine
