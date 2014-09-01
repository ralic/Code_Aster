subroutine pgpext(sd_pgp)
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
! Adds additional contributions to the nodal fields for the following
! options :
! - CORR_STAT : static correction
! - MULT_APPUI : multiple pinning (seismic studies)
! - ACCE_MONO_APPUI : entraining acceleration
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pgpget.h"
#include "asterfort/posddl.h"
#include "asterfort/utmess.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!   -0.1- Input/output arguments
    character(len=8), intent(in):: sd_pgp
!   -0.2- Local variables
    integer           :: nbobs, iobs, physlen, nbexcit, i
    integer           :: iinst, iex, nbinst, jvecr, iddl
    integer           :: jtblp, lc, nbord, ier, dec
    integer           :: inoeud, nbeq, icorrst
    real(kind=8)      :: coef, cumul, dir(3), magn, acce
    character(len=4)  :: typres
    character(len=8)  :: base, result, nume, acce_mo
    character(len=16) :: champ
    character(len=19) :: resin19
    character(len=24) :: nomjv

    real(kind=8)    , pointer :: ipsd(:) => null()
    real(kind=8)    , pointer :: lpsidel(:) => null()
    real(kind=8)    , pointer :: fxxx(:) => null()
    real(kind=8)    , pointer :: linst(:) => null()
    real(kind=8)    , pointer :: coef_dir(:) => null()
    real(kind=8)    , pointer :: facce(:) => null()
    character(len=8), pointer :: lcmp(:) => null()
    character(len=8), pointer :: nomnoeu(:) => null()
    character(len=8) ,pointer :: nomfon(:) => null()

!   ------------------------------------------------------------------------------------
!   Definition of statement functions giving the appropriate (i,j) terms for different
!   matrices/vectors
#define psidel(iddl,iex) lpsidel((iddl-1)*nbexcit+iex)
#define f_xxx(iinst,iex) fxxx((iex-1)*nbinst+iinst)
#define vale(iinst,iddl) zr(jvecr+lc+(iinst-1)*physlen+iddl-1)
!   -0.3- Initialization

    call jemarq()

    call pgpget(sd_pgp,'TYP_RESU ',kscal=typres)

!   If harmonic results, no extras are available and so no action is needed
    if (typres.eq.'HARM') goto 999

!   If transient results, then we have to repass through the different observations
!   to check for extras
    call pgpget(sd_pgp,'RESU_OUT',kscal=result)
    call pgpget(sd_pgp,'NB_OBSER',iscal=nbobs)
    call pgpget(sd_pgp,'RESU_IN ',kscal=resin19)
    call pgpget(sd_pgp, 'BASE',kscal=base)
!
    call jeveuo(result//'           .TBLP', 'L', jtblp)
    nomjv = zk24(jtblp+4*(9-1)+2)
    call jeveuo(nomjv,'L',jvecr)

!   Line counter, across the whole table (for different observations)
    lc = 0
    do iobs = 1,nbobs
        call pgpget(sd_pgp,'NOM_CHAM ',iobs=iobs, kscal=champ)
        call pgpget(sd_pgp,'REF_COMP',iobs=iobs, lonvec=physlen)
        call pgpget(sd_pgp,'DISC',iobs=iobs, lonvec=nbord)

        call pgpget(sd_pgp,'ADD_CORR',iobs=iobs, iscal=icorrst)
        call pgpget(sd_pgp,'ACC_MO_A',iobs=iobs, kscal=acce_mo)

!       CORR_STAT and MULT_APPUI extra, available for DEPL, VITE, and ACCE requests
        if ( ((champ(1:4).eq.'DEPL').or.(champ(1:4).eq.'VITE').or.(champ(1:4).eq.'ACCE'))&
            .and.(icorrst.ne.0) ) then

!           Find the ddl id's for all (node,component) couples for the observation 'iobs'
!           and from the dyna_gene extract .IPSD for all required ddl's 
!           size = [nbexcit x nbddl ]   (note : nbddl = physlen)

            call dismoi('NUME_DDL', base, 'RESU_DYNA', repk=nume, arret='F')
            call dismoi('NB_EQUA', nume, 'NUME_DDL', repi=nbeq)
            call jeveuo(resin19//'.IPSD', 'L', vr=ipsd)
            call jelira(resin19//'.IPSD', 'LONMAX', nbexcit)
            nbexcit = nbexcit/nbeq

            AS_ALLOCATE(vk8=nomnoeu , size=physlen)
            AS_ALLOCATE(vk8=lcmp , size=physlen)
            call pgpget(sd_pgp,'REF_SUP1',iobs=iobs, kvect=nomnoeu)
            call pgpget(sd_pgp,'REF_COMP',iobs=iobs, kvect=lcmp)
            AS_ALLOCATE(vr=lpsidel , size=physlen*nbexcit)
            do i = 1, physlen
                dec = (i-1)*nbexcit
                call posddl('NUME_DDL', nume, nomnoeu(i), lcmp(i), inoeud,&
                            iddl)
                do iex = 1, nbexcit
                    lpsidel(dec+iex) = ipsd((iex-1)*nbeq + iddl)
                end do
            end do
            AS_DEALLOCATE(vk8=nomnoeu)
            AS_DEALLOCATE(vk8=lcmp)

!           From the dyna_gene extract .F*** function values for all required instants
!           size = [nbexcit x nbinst ]
            call pgpget(sd_pgp,'DISC',iobs=iobs, lonvec=nbinst)
            AS_ALLOCATE(vr=linst , size=nbinst)
            call pgpget(sd_pgp, 'DISC', iobs=iobs, rvect = linst)

            AS_ALLOCATE(vr=fxxx , size=nbinst*nbexcit)
            call jeveuo(resin19//'.F'//champ(1:3), 'L', vk8=nomfon)
            do iex = 1, nbexcit
                dec = (iex-1)*nbinst
                if (nomfon(iex) .eq. ' ') then
                    call utmess('A', 'ALGORITH13_44', sk='CHARGE EN MONO APPUI')
                    do iinst = 1, nbinst
                        fxxx(dec+iinst) = 0.d0
                    end do
                else
                    do iinst = 1, nbinst
                        call fointe('F ', nomfon(iex), 1, ['INST'], [linst(iinst)],&
                                    coef, ier)
                        fxxx(dec+iinst) = coef
                    end do
                end if
            end do

!           Sum up, for all excitations, .IPSD(iddl,iexcit) * .F***(iexcit,iinst) 
!           add up to the table result for the given field at (iinst, iddl)

            do iinst = 1, nbinst
                do i = 1, physlen
                    cumul = 0.d0
                    do iex = 1, nbexcit
                        cumul = cumul + psidel(i,iex)*f_xxx(iinst,iex)
                    end do
                    vale(iinst,i) = vale(iinst,i) + cumul
                end do
            end do

            AS_DEALLOCATE(vr=lpsidel)
            AS_DEALLOCATE(vr=linst)
            AS_DEALLOCATE(vr=fxxx)
        end if

!       ACCE_MONO_APPUI extra, available for ACCE_ABSOLU request only
        if ((champ.eq.'ACCE_ABSOLU').and.(icorrst.eq.0).and.&
            (acce_mo.ne.' ')) then

!           Decompose the entraining acceleration direction on the requested components
!           DX, DY, and/or DZ. Calculate the coeffients COEF
!           size = [nbcomp]
            AS_ALLOCATE(vk8=lcmp   , size=physlen)
            AS_ALLOCATE(vr=coef_dir, size=physlen)
            call pgpget(sd_pgp,'REF_COMP',iobs=iobs, kvect=lcmp)
            call pgpget(sd_pgp,'ACC_DIR ',iobs=iobs, rvect=dir)
            magn = sqrt(dir(1)**2+dir(2)**2+dir(3)**2)
            do i = 1, physlen
                if (lcmp(i).eq.'DX') then
                    coef_dir(i) = dir(1)/magn
                else if (lcmp(i).eq.'DY') then
                    coef_dir(i) = dir(2)/magn
                else if (lcmp(i).eq.'DZ') then
                    coef_dir(i) = dir(3)/magn
                end if
            end do
            AS_DEALLOCATE(vk8=lcmp)

!           Evaluate (by interpolation or no), the function giving the MONO_APPUI
!           acceleration for all required instants facce
!           size = [nbinst]
            call pgpget(sd_pgp,'DISC',iobs=iobs, lonvec=nbinst)
            AS_ALLOCATE(vr=facce , size=nbinst)
            AS_ALLOCATE(vr=linst , size=nbinst)
            call pgpget(sd_pgp, 'DISC', iobs=iobs, rvect = linst)
            do iinst = 1, nbinst
                call fointe('F ', acce_mo, 1, ['INST'], [linst(iinst)],&
                            acce, ier)
                facce(iinst) = acce
            end do

!           Add up, in the result table for the accelerations of the requested nodes,
!           the value of coef_dir(icomp)*facce(iinst) for component icomp and instant iinst
            do iinst = 1, nbinst
                do i = 1, physlen
                    vale(iinst,i) = vale(iinst,i) + coef_dir(i)*facce(iinst)
                end do
            end do
            AS_DEALLOCATE(vr=coef_dir)
            AS_DEALLOCATE(vr=facce)
            AS_DEALLOCATE(vr=linst)
        end if
!
        lc = lc + nbord*physlen
!
    end do

999 continue

    call jedema()

end subroutine