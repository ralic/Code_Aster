subroutine calir3(mo, nbma1, lima1, nbno2, lino2,&
                  geom2, corre1, corre2, jlisv1, iocc)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pj3dco.h"
#include "asterfort/utmess.h"
    character(len=8) :: mo
    character(len=16) :: corre1, corre2
    character(len=24) :: geom2
    integer :: nbma1, lima1(nbma1)
    integer :: nbno2, lino2(nbno2), jlisv1, iocc
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! BUT : CALCULER LES SD CORRE1 ET COORE2 UTILISEES POUR :
!       LIAISON_MAIL + TYPE_RACCORD='COQUE_MASSIF'
! ======================================================================
!
    real(kind=8) :: rbid, epais
    integer :: ino2, nuno2, jgeom2, k, ncmp,   jcnsl, ibid
    character(len=19) :: chnorm, csnorm
    integer, pointer :: cnsd(:) => null()
    real(kind=8), pointer :: cnsv(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
    call jeveuo(geom2, 'E', jgeom2)
!
    call getvid('LIAISON_MAIL', 'CHAM_NORMALE', iocc=iocc, scal=chnorm, nbret=ibid)
    call getvr8('LIAISON_MAIL', 'EPAIS', iocc=iocc, scal=epais, nbret=ibid)
!
    csnorm='&&CALIR3.CSNORM'
    call cnocns(chnorm, 'V', csnorm)
    call jeveuo(csnorm//'.CNSD', 'L', vi=cnsd)
    call jeveuo(csnorm//'.CNSL', 'L', jcnsl)
    call jeveuo(csnorm//'.CNSV', 'L', vr=cnsv)
    ncmp=cnsd(2)
    ASSERT(ncmp.eq.3)
    ASSERT(ncmp.eq.3)
!
!
!     -- ON REMPLIT L'OBJET &&CALIRC.LISV1 :
!     -------------------------------------------------
    do 20,ino2=1,nbno2
    nuno2=lino2(ino2)
    do 10,k=1,3
    if (.not.zl(jcnsl-1+3*(nuno2-1)+k)) then
        call utmess('F', 'CHAMPS_2', sk=chnorm)
    endif
    ASSERT(zl(jcnsl-1+3*(nuno2-1)+k))
    zr(jlisv1-1+3*(nuno2-1)+k)=cnsv(3*(nuno2-1)+k)*&
            epais
10  continue
    20 end do
!
!
!     -- ON MODIFIE GEOM2 (+H/2) POUR OBTENIR CORRE1 :
!     -------------------------------------------------
    do 40,ino2=1,nbno2
    nuno2=lino2(ino2)
    do 30,k=1,3
    zr(jgeom2-1+(nuno2-1)*3+k)=zr(jgeom2-1+(nuno2-1)*3+k)+&
            zr(jlisv1-1+3*(nuno2-1)+k)/2.d0
30  continue
    40 end do
    call pj3dco('PARTIE', mo, mo, nbma1, lima1,&
                nbno2, lino2, ' ', geom2, corre1,&
                .false._1, rbid)
!
!
!     -- ON MODIFIE GEOM2 (-H/2) POUR OBTENIR CORRE2 :
!     -------------------------------------------------
    do 60,ino2=1,nbno2
    nuno2=lino2(ino2)
    do 50,k=1,3
    zr(jgeom2-1+(nuno2-1)*3+k)=zr(jgeom2-1+(nuno2-1)*3+k)-&
            zr(jlisv1-1+3*(nuno2-1)+k)
50  continue
    60 end do
    call pj3dco('PARTIE', mo, mo, nbma1, lima1,&
                nbno2, lino2, ' ', geom2, corre2,&
                .false._1, rbid)
!
!     -- ON RETABLIT GEOM2 :
!     -------------------------------------------------
    do 80,ino2=1,nbno2
    nuno2=lino2(ino2)
    do 70,k=1,3
    zr(jgeom2-1+(nuno2-1)*3+k)=zr(jgeom2-1+(nuno2-1)*3+k)+&
            zr(jlisv1-1+3*(nuno2-1)+k)/2.d0
70  continue
    80 end do
!
!
    call detrsd('CHAMP', csnorm)
    call jedema()
end subroutine
