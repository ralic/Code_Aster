subroutine pgpche(sd_pgp, iobs)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
! Extract a per-element field from the modal basis, reduced to the 
! requested elements and field components 
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/codent.h"
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
    integer           :: nbcmp, nbsupp, ibid, iad, icmp
    integer           :: iret, im, ima, jcsd, jcsl
    integer           :: jcsv, nbpt, ipt, inod, imod
    integer           :: nbmodes, nbptmx, dec1, dec2, dec3
    integer           :: jconn
    character(len=4)  :: typch, typsc
    character(len=5)  :: k_ipt
    character(len=8)  :: base, maillage, nomnod, nomma
    character(len=16) :: champ
    character(len=19) :: nomcha

    integer         , pointer :: lmai(:)  => null()
    integer         , pointer :: indic(:) => null()
    character(len=8), pointer :: lcmp(:)  => null()
    character(len=8), pointer :: rsup1(:) => null()
    character(len=8), pointer :: rsup2(:) => null()
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
    call pgpget(sd_pgp, 'NUM_MAIL', iobs=iobs, lonvec=nbsupp)
    call dismoi('NB_MODES_TOT', base, 'RESULTAT', repi=nbmodes)

!   -1.2- Field name, type, components, element numbers
    call pgpget(sd_pgp, 'NOM_CHAM', iobs=iobs, kscal=champ)
    call pgpget(sd_pgp, 'TYP_CHAM', iobs=iobs, kscal=typch)

    AS_ALLOCATE(vk8=lcmp, size=nbcmp)
    call pgpget(sd_pgp, 'NOM_CMP' , iobs=iobs, kvect=lcmp)

    AS_ALLOCATE(vi=lmai, size=nbsupp)
    call pgpget(sd_pgp, 'NUM_MAIL' , iobs=iobs, ivect=lmai)


!   -1.3- Determine the maximum number of points per element
!         Allocate and fillup rsup1 : element names, 
!                             rsup2 : node (ELNO) or pt (ELGA) names
!                             rcomp : reference component names
!                             indic : indicator (logical) that a given line is to be
!                                     considered (cf. nbpt != nbptmx)

!   Consider a sample field of the correct type (imod = 1)
    call rsexch(' ', base, champ, 1, nomcha, iret)

!   Transform the elements field to a simple elements field
    call celces(nomcha, 'V', sd_pgp//'.CHAM_EL_S ')

!   Reduce the simple field to the elements and components of interest          
    call cesred(sd_pgp//'.CHAM_EL_S ', nbsupp, lmai, nbcmp, lcmp,&
                'V', sd_pgp//'.CHAM_EL_SR')

!   Description of the maximum number of points is found in .CESD[3]
    call jeveuo(sd_pgp//'.CHAM_EL_SR.CESD','L',jcsd)
    nbptmx = zi(jcsd+3-1)

!   Allocating and filling up reference values (see pt. 1.3 for details)
    AS_ALLOCATE(vk8=rsup1, size=nbsupp*nbptmx*nbcmp)
    AS_ALLOCATE(vk8=rsup2, size=nbsupp*nbptmx*nbcmp)
    AS_ALLOCATE(vk8=rcomp, size=nbsupp*nbptmx*nbcmp)
    AS_ALLOCATE(vi =indic, size=nbsupp*nbptmx*nbcmp)
    do icmp = 1, nbcmp
        dec2 = (icmp-1)*nbsupp*nbptmx
        do im = 1, nbsupp
            dec3 = (im-1)*nbptmx
            ima = lmai(im)
            nbpt = zi(jcsd-1+5+4*(ima-1)+1)
            call jenuno(jexnum(maillage//'.NOMMAI', ima), nomma)
            if (typch.eq.'ELNO') then
                call jeveuo(jexnum(maillage//'.CONNEX', ima), 'L',jconn)
            end if
            do ipt = 1, nbpt
                if (typch.eq.'ELNO') then
                    inod = zi(jconn+ipt-1)
                    call jenuno(jexnum(maillage//'.NOMNOE', inod), nomnod)
                else
                    call codent(ipt, 'G', k_ipt)
                    nomnod = 'PT '//k_ipt
                end if
                rsup1(dec2+dec3+ipt) = nomma
                rsup2(dec2+dec3+ipt) = nomnod
                rcomp(dec2+dec3+ipt) = lcmp(icmp)
                indic(dec2+dec3+ipt) = 1
            end do
            if (nbpt.lt.nbptmx) then
                do ipt = nbpt+1, nbptmx
                    rsup1(dec2+dec3+ipt) = '-'
                    rsup2(dec2+dec3+ipt) = '-'
                    rcomp(dec2+dec3+ipt) = '-'
                    indic(dec2+dec3+ipt) = 0
                end do
            end if
        end do
    end do

!   Minor cleanup
    call detrsd('CHAM_ELEM_S',sd_pgp//'.CHAM_EL_S ')
    call detrsd('CHAM_ELEM_S',sd_pgp//'.CHAM_EL_SR')

!   -1.4- Get the scalar type of the needed field, and allocate the work vector
!         with the correct type (real or complex) and initialize to undef (+ undef j)
    call pgpget(sd_pgp, 'TYP_SCAL' , iobs=iobs, kscal=typsc)

    if (typsc(1:1).eq.'R') then 
        AS_ALLOCATE(vr=vectr, size=nbsupp*nbptmx*nbcmp*nbmodes)
        do ibid=1,nbsupp*nbptmx*nbcmp*nbmodes
            vectr(ibid) = undef
        end do
    else if (typsc(1:1).eq.'C') then
        AS_ALLOCATE(vc=vectc, size=nbsupp*nbptmx*nbcmp*nbmodes)
        do ibid=1,nbsupp*nbptmx*nbcmp*nbmodes
            vectc(ibid) = dcmplx(undef,undef)
        end do
    end if

!
    do imod = 1, nbmodes
        call rsexch(' ', base, champ, imod, nomcha, iret)

!       Transform the elements field to a simple elements field
        call celces(nomcha, 'V', sd_pgp//'.CHAM_EL_S ')
!       Reduce the simple field to the elements and components of interest          
        call cesred(sd_pgp//'.CHAM_EL_S ', nbsupp, lmai, nbcmp, lcmp,&
                    'V', sd_pgp//'.CHAM_EL_SR')

        call jeveuo(sd_pgp//'.CHAM_EL_SR.CESD','L',jcsd)
        call jeveuo(sd_pgp//'.CHAM_EL_SR.CESL','L',jcsl)
        call jeveuo(sd_pgp//'.CHAM_EL_SR.CESV','L',jcsv)

        dec1 = (imod-1)*nbcmp*nbsupp*nbptmx
        do icmp = 1, nbcmp
            dec2 = (icmp-1)*nbsupp*nbptmx
            do im = 1, nbsupp
                dec3 = (im-1)*nbptmx
                ima = lmai(im)
                nbpt = zi(jcsd-1+5+4* (ima-1)+1)
                do ipt = 1, nbpt
                    call cesexi('C',jcsd,jcsl,ima,ipt,1,icmp,iad)
                    if (iad.gt.0) then
                        if (typsc(1:1).eq.'R') then 
                            vectr(dec1+dec2+dec3+ipt) = zr(jcsv-1+iad)
                        else if (typsc(1:1).eq.'C') then
                            vectc(dec1+dec2+dec3+ipt) = zc(jcsv-1+iad)
                        end if
                    end if
                end do
            end do
        end do

        call detrsd('CHAM_ELEM_S',sd_pgp//'.CHAM_EL_S ')
        call detrsd('CHAM_ELEM_S',sd_pgp//'.CHAM_EL_SR')
    end do

    if (typsc(1:1).eq.'R') then 
        call pgpsav(sd_pgp, 'VEC_PR_R', nbsupp*nbcmp*nbptmx*nbmodes, &
                    iobs=iobs, rvect=vectr)
        AS_DEALLOCATE(vr=vectr)
    else if (typsc(1:1).eq.'C') then
        call pgpsav(sd_pgp, 'VEC_PR_C', nbsupp*nbcmp*nbptmx*nbmodes, &
                    iobs=iobs, cvect=vectc)
        AS_DEALLOCATE(vc=vectc)
    end if

    call pgpsav(sd_pgp, 'REF_SUP1', nbsupp*nbcmp*nbptmx, iobs=iobs, kvect=rsup1)
    call pgpsav(sd_pgp, 'REF_SUP2', nbsupp*nbcmp*nbptmx, iobs=iobs, kvect=rsup2)
    call pgpsav(sd_pgp, 'REF_COMP', nbsupp*nbcmp*nbptmx, iobs=iobs, kvect=rcomp)
    call pgpsav(sd_pgp, 'REF_INDI', nbsupp*nbcmp*nbptmx, iobs=iobs, ivect=indic)

    AS_DEALLOCATE(vi=lmai)
    AS_DEALLOCATE(vi=indic)
    AS_DEALLOCATE(vk8=lcmp)
    AS_DEALLOCATE(vk8=rsup1)
    AS_DEALLOCATE(vk8=rsup2)
    AS_DEALLOCATE(vk8=rcomp)

    call jedema()

end subroutine
