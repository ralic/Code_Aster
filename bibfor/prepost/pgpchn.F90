subroutine pgpchn(sd_pgp, iobs)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! Extract a node field from the modal basis, reduced to the requested
! degrees of freedom (node,component) 
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsred.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/pgpget.h"
#include "asterfort/pgpsav.h"
#include "asterfort/rsexch.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"


!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!   -0.1- Input/output arguments
    character(len=8), intent(in):: sd_pgp
    integer         , intent(in):: iobs
!   -0.2- Local variables
    real(kind=8)      :: undef
    integer           :: nbcmp, nbsupp, ibid, icmp,  iret
    integer           :: jcsd, jcsl, jcsv, ino, inod
    integer           :: imod, nbmodes, dec1, dec2
    character(len=4)  :: typsc
    character(len=8)  :: base, maillage, nomnod
    character(len=16) :: champ, champ2
    character(len=19) :: nomcha

    integer         , pointer :: lnoe(:)  => null()
    integer         , pointer :: indic(:) => null()
    character(len=8), pointer :: lcmp(:)  => null()
    character(len=8), pointer :: rsup1(:) => null()
    character(len=8), pointer :: rcomp(:) => null()

    real(kind=8)    , pointer :: vectr(:) => null()
    complex(kind=8) , pointer :: vectc(:) => null()

    undef = r8vide()
!   -0.3- Initialization
    call jemarq()

!   -1.1- Mesh, projection basis, number of components and nodes, number of modes
    call pgpget(sd_pgp, 'MAILLAGE', kscal=maillage)
    call pgpget(sd_pgp, 'BASE'    , kscal=base)
    call pgpget(sd_pgp, 'NOM_CMP' , iobs=iobs, lonvec=nbcmp)
    call pgpget(sd_pgp, 'NUM_NOEU', iobs=iobs, lonvec=nbsupp)
    call dismoi('NB_MODES_TOT', base, 'RESULTAT', repi=nbmodes)

!   -1.2- Field name, restitution field name, components, node numbers
    call pgpget(sd_pgp, 'NOM_CHAM', iobs=iobs, kscal=champ)
    champ2 = champ
    if (champ(5:11).eq.'_ABSOLU') champ2=champ(1:4)
    if ((champ(1:4).eq.'VITE').or.(champ(1:4).eq.'ACCE')) champ2 = 'DEPL'

    AS_ALLOCATE(vk8=lcmp, size=nbcmp)
    call pgpget(sd_pgp, 'NOM_CMP' , iobs=iobs, kvect=lcmp)

    AS_ALLOCATE(vi=lnoe, size=nbsupp)
    call pgpget(sd_pgp, 'NUM_NOEU' , iobs=iobs, ivect=lnoe)

!   -1.3- Get the scalar type of the needed field, and allocate the work vector
!         with the correct type (real or complex) and initialize to undef (+ undef j)
    call pgpget(sd_pgp, 'TYP_SCAL' , iobs=iobs, kscal=typsc)
    if (typsc(1:1).eq.'R') then 
        AS_ALLOCATE(vr=vectr, size=nbsupp*nbcmp*nbmodes)
        do ibid=1,nbsupp*nbcmp*nbmodes
            vectr(ibid) = undef
        end do
    else if (typsc(1:1).eq.'C') then 
        AS_ALLOCATE(vc=vectc, size=nbsupp*nbcmp*nbmodes)
        do ibid=1,nbsupp*nbcmp*nbmodes
            vectc(ibid) = dcmplx(undef,undef)
        end do
    end if

    AS_ALLOCATE(vk8=rsup1, size=nbsupp*nbcmp)
    AS_ALLOCATE(vk8=rcomp, size=nbsupp*nbcmp)
    AS_ALLOCATE(vi =indic, size=nbsupp*nbcmp)
!
    do imod = 1, nbmodes
        call rsexch(' ', base, champ2, imod, nomcha, iret)

        dec1 = (imod-1)*nbcmp*nbsupp
!       Transform the point(node) field to a simple node field
        call cnocns(nomcha, 'V', sd_pgp//'.CHAM_NO_S ')
!       Reduce the simple field to the nodes and components of interest      
        call cnsred(sd_pgp//'.CHAM_NO_S ', nbsupp, lnoe, nbcmp, lcmp,&
                    'V', sd_pgp//'.CHAM_NO_SR')
        call jeveuo(sd_pgp//'.CHAM_NO_SR.CNSD','L',jcsd)
        call jeveuo(sd_pgp//'.CHAM_NO_SR.CNSL','L',jcsl)
        call jeveuo(sd_pgp//'.CHAM_NO_SR.CNSV','L',jcsv)
        do icmp = 1,nbcmp
            dec2 = (icmp-1)*nbsupp
            do ino = 1,nbsupp
                inod = lnoe(ino)
                if (zl(jcsl+(inod-1)*nbcmp + icmp - 1)) then
                    if (typsc(1:1).eq.'R') then 
                        vectr(dec1+dec2+ino) = zr(jcsv+(inod-1)*nbcmp + icmp - 1)
                    else if (typsc(1:1).eq.'C') then
                        vectc(dec1+dec2+ino) = zc(jcsv+(inod-1)*nbcmp + icmp - 1)
                    end if
                end if
                if (imod.eq.1) then
                    call jenuno(jexnum(maillage//'.NOMNOE', inod), nomnod)
                    rsup1(dec2+ino) = nomnod
                    rcomp(dec2+ino) = lcmp(icmp)
                    indic(dec2+ino) = 1
                end if
            end do
        end do

        call detrsd('CHAM_NO_S',sd_pgp//'.CHAM_NO_S ')
        call detrsd('CHAM_NO_S',sd_pgp//'.CHAM_NO_SR')
    end do

    call pgpsav(sd_pgp, 'REF_SUP1', nbsupp*nbcmp, iobs=iobs, kvect=rsup1)
    call pgpsav(sd_pgp, 'REF_COMP', nbsupp*nbcmp, iobs=iobs, kvect=rcomp)
    call pgpsav(sd_pgp, 'REF_INDI', nbsupp*nbcmp, iobs=iobs, ivect=indic)

    if (typsc(1:1).eq.'R') then 
        call pgpsav(sd_pgp, 'VEC_PR_R', nbsupp*nbcmp*nbmodes, iobs=iobs, rvect=vectr)
        AS_DEALLOCATE(vr=vectr)
    else if (typsc(1:1).eq.'C') then
        call pgpsav(sd_pgp, 'VEC_PR_C', nbsupp*nbcmp*nbmodes, iobs=iobs, cvect=vectc)
        AS_DEALLOCATE(vc=vectc)
    end if

    AS_DEALLOCATE(vi=lnoe)
    AS_DEALLOCATE(vi=indic)
    AS_DEALLOCATE(vk8=lcmp)
    AS_DEALLOCATE(vk8=rsup1)
    AS_DEALLOCATE(vk8=rcomp)

    call jedema()

end subroutine
