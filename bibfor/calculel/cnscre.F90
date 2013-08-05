subroutine cnscre(maz, nomgdz, ncmp, licmp, basez,&
                  cnsz)
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeundf.h"
#include "asterfort/verigd.h"
#include "asterfort/wkvect.h"
    character(len=*) :: maz, nomgdz, cnsz, basez
    integer :: ncmp
    character(len=*) :: licmp(ncmp)
! ------------------------------------------------------------------
! BUT : CREER UN CHAM_NO_S
! ------------------------------------------------------------------
!     ARGUMENTS:
! MAZ     IN/JXIN  K8  : SD MAILLAGE DE CNSZ
! NOMGDZ  IN       K8  : NOM DE LA GRANDEUR DE CNSZ
! NCMP    IN       I   : NOMBRE DE CMPS VOULUES DANS CNSZ
! LICMP   IN       L_K8: NOMS DES CMPS VOULUES DANS CNSZ
! BASEZ   IN       K1  : BASE DE CREATION POUR CNSZ : G/V/L
! CNSZ    IN/JXOUT K19 : SD CHAM_NO_S A CREER
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=1) :: kbid, base
    character(len=3) :: tsca
    character(len=8) :: ma, nomgd
    character(len=19) :: cns
    integer :: ibid, nbno, jcnsk, jcnsd
    integer :: jcnsc, k, jcnsl, jcnsv, iret
!     ------------------------------------------------------------------
!
    call jemarq()
    cns = cnsz
    base = basez
    nomgd = nomgdz
    ma = maz
!
    call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbno,&
                kbid, ibid)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
!
!
!     -- SI CNS EXISTE DEJA, ON LE DETRUIT :
    call detrsd('CHAM_NO_S', cns)
!
!------------------------------------------------------------------
!     1- QUELQUES VERIFS :
!     ------------------------
    ASSERT(ncmp.ne.0)
    call verigd(nomgd, licmp, ncmp, iret)
    ASSERT(iret.le.0)
!
!------------------------------------------------------------------
!     2- CREATION DE CNS.CNSK:
!     ------------------------
    call wkvect(cns//'.CNSK', base//' V K8', 2, jcnsk)
    zk8(jcnsk-1+1) = ma
    zk8(jcnsk-1+2) = nomgd
!
!------------------------------------------------------------------
!     3- CREATION DE CNS.CNSD:
!     ------------------------
    call wkvect(cns//'.CNSD', base//' V I', 2, jcnsd)
    zi(jcnsd-1+1) = nbno
    zi(jcnsd-1+2) = ncmp
!
!------------------------------------------------------------------
!     4- CREATION DE CNS.CNSC:
!     ------------------------
    call wkvect(cns//'.CNSC', base//' V K8', ncmp, jcnsc)
    do 10,k = 1,ncmp
    zk8(jcnsc-1+k) = licmp(k)
    10 end do
!
!------------------------------------------------------------------
!     5- CREATION DE CNS.CNSL:
!     ------------------------
    call wkvect(cns//'.CNSL', base//' V L', nbno*ncmp, jcnsl)
    call jeundf(cns//'.CNSL')
!
!------------------------------------------------------------------
!     6- CREATION DE CNS.CNSV:
!     ------------------------
    call wkvect(cns//'.CNSV', base//' V '//tsca, nbno*ncmp, jcnsv)
    call jeundf(cns//'.CNSV')
!
    call jedema()
end subroutine
