subroutine cnocre(maz, nomgdz, nbnoz, linoe, ncmpz,&
                  licmp, cnocmp, basez, prof, cnoz)
! person_in_charge: jacques.pellet at edf.fr
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: maz, nomgdz, cnoz, basez, prof
    integer :: ncmpz, nbnoz, linoe(nbnoz), cnocmp(nbnoz*ncmpz)
    character(len=*) :: licmp(ncmpz)
! ------------------------------------------------------------------
! BUT : CREER UN CHAM_NO A VALEURS NULLES SUR UN PROFIL DEJA EXISTANT
! OU NON (SI LE PROFIL N'EXISTE PAS -- BLANCS ' ' -- IL EST CREE)
! ------------------------------------------------------------------
!     ARGUMENTS:
! MAZ     IN/JXIN  K8  : MAILLAGE DE CNOZ
! NOMGDZ  IN       K8  : NOM DE LA GRANDEUR DE CNOZ
! NBNOZ   IN       I   : NOMBRE DE NOEUDS VOULUES DANS CNOZ
! LINOE   IN       L_I : NOMS DES NOEUDS VOULUES DANS CNOZ
! NCMPZ   IN       I   : NOMBRE DE CMPS VOULUES DANS CNOZ
! LICMP   IN       L_K8: NOMS DES CMPS VOULUES DANS CNOZ
! BASEZ   IN       K1  : BASE DE CREATION POUR CNOZ : G/V/L
! PROF    IN/JXVAR K19 : SD PROF_CHNO SUR LAQUELLE LE CHAM_NO EST CREE
! CNOZ    IN/JXOUT K19 : SD CHAM_NO A CREER
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: ibid, nbno,   ino
    integer :: i, k, jcnsl, jcnsv, ncmp
    character(len=3) :: tsca
    character(len=8) :: nomgd
    character(len=19) :: cns
    character(len=8), pointer :: cnsk(:) => null()
    integer, pointer :: cnsd(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    cns = '&&CNOCRE.CNS'
    call cnscre(maz, nomgdz, ncmpz, licmp, 'V',&
                cns)
!
    call jeveuo(cns//'.CNSK', 'L', vk8=cnsk)
    call jeveuo(cns//'.CNSD', 'L', vi=cnsd)
    call jeveuo(cns//'.CNSV', 'E', jcnsv)
    call jeveuo(cns//'.CNSL', 'E', jcnsl)
!
    nomgd = cnsk(2)
    nbno = cnsd(1)
    ncmp = cnsd(2)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
    ASSERT((tsca.eq.'R') .or. (tsca.eq.'C'))
    if (tsca .eq. 'R') then
!         -----------
        if (nbnoz .eq. 0) then
            do k = 1, ncmp
                do ino = 1, nbno
                    zl(jcnsl-1+ (ino-1)*ncmp+k) = .true.
                    zr(jcnsv-1+ (ino-1)*ncmp+k) = 0.0d0
                end do
            end do
!
        else
            do i = 1, nbnoz
                ino = linoe(i)
                do k = 1, ncmp
                    if (cnocmp((i-1)*ncmp+k) .eq. 1) then
                        zl(jcnsl-1+ (ino-1)*ncmp+k) = .true.
                        zr(jcnsv-1+ (ino-1)*ncmp+k) = 0.0d0
                    endif
                end do
            end do
        endif
!
    else if (tsca.eq.'C') then
!             -----------
        if (nbnoz .eq. 0) then
            do k = 1, ncmp
                do ino = 1, nbno
                    zl(jcnsl-1+ (ino-1)*ncmp+k) = .true.
                    zc(jcnsv-1+ (ino-1)*ncmp+k) = (0.0d0,0.0d0)
                end do
            end do
!
        else
            do i = 1, nbnoz
                ino = linoe(i)
                do k = 1, ncmp
                    if (cnocmp((i-1)*ncmp+k) .eq. 1) then
                        zl(jcnsl-1+ (ino-1)*ncmp+k) = .true.
                        zc(jcnsv-1+ (ino-1)*ncmp+k) = (0.0d0,0.0d0)
                    endif
                end do
            end do
        endif
    endif
!
!
    call cnscno(cns, prof, 'NON', basez, cnoz,&
                'F', ibid)
    call detrsd('CHAM_NO_S', cns)
!
    call jedema()
end subroutine
