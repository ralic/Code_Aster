subroutine pgppre(sd_pgp)
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
! Preprocess the user input for the command POST_GENE_PHYS
! 1 - Verify the coherence of input arguments
! 2 - Establish a list of *observations* to be calculated, to be saved
!     in a dedicated developper data structure (sd_pgp)
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getfac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeexin.h"
#include "asterfort/pgpche.h"
#include "asterfort/pgpchn.h"
#include "asterfort/pgpget.h"
#include "asterfort/pgpsav.h"
#include "asterfort/reliem.h"
#include "asterfort/rsexch.h"
#include "asterfort/rstran.h"
#include "asterfort/utmess.h"
#include "asterfort/utncmp.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"


!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!   -0.1- Input/output arguments
    character(len=8), intent(in):: sd_pgp
!   -0.2- Local variables
    aster_logical     :: found
    real(kind=8)      :: dir(3)
    integer           :: nbobs, iobs, nbcmp, nbsupp, ivsup
    integer           :: icomp, icmp, nbmcl, iret, jcomp
    integer           :: nbcmp1, nord, icorrst, multap
    character(len=3)  :: prsimp, corrst
    character(len=4)  :: typch, typsc
    character(len=8)  :: nomres, resin, base, base0
    character(len=8)  :: maillage, nume, ncmp, nomgd, modele
    character(len=8)  :: acce_mo, limocl(4)
    character(len=16) :: typreso, nomcmd, typresi , champ, typem
    character(len=16) :: champ2
    character(len=19) :: nomcha, resin19
    character(len=24) :: numejv, discjv

    character(len=8), pointer :: lcmp(:) => null()

!   -0.3- Initialization
    call jemarq()

!   ====================================================================
!   = 1 = Verification of the coherence of input arguments
!   ====================================================================
    call getres(nomres, typreso, nomcmd)
    call getvid(' ', 'RESU_GENE', scal=resin)
    call gettco(resin, typresi)
    call dismoi('PROJ_SIMPLE', resin, 'RESU_DYNA', repk=prsimp)
!   It isn't possible to recombine results from multiple projections (ex. sub-structuring)
    if (prsimp.eq.'NON') call utmess('F', 'PREPOST_39', sk=resin)

!   Coherence of the projcetion bases
    call getvid(' ', 'MODE_MECA', scal=base, nbret=iret)
    if (iret.ne.1) then
        call dismoi('BASE_MODALE', resin, 'RESU_DYNA', repk=base, arret='F')
    else
        call dismoi('BASE_MODALE', resin, 'RESU_DYNA', repk=base0, arret='C')
        if (base0(1:1).ne.' ') then
!           Alert in case the given modal basis is different than the one referenced
!           inside the dyna_gene result
            if (base.ne.base0) call utmess('A', 'PREPOST_40', nk=2, valk=[base0, base])
        end if
    end if

    call dismoi('NOM_MODELE' , base , 'RESULTAT' , repk=modele)
    call dismoi('NOM_MAILLA' , base , 'RESULTAT' , repk=maillage)
    call dismoi('NUME_DDL'   , base , 'RESU_DYNA', repk=nume)

    call pgpsav(sd_pgp, 'RESU_OUT', 1, kscal=nomres)
    call pgpsav(sd_pgp, 'RESU_IN' , 1, kscal=resin)
    call pgpsav(sd_pgp, 'TYP_RESU', 1, kscal=typresi(1:4))
    call pgpsav(sd_pgp, 'BASE'    , 1, kscal=base)
    call pgpsav(sd_pgp, 'MODELE'  , 1, kscal=modele)
    call pgpsav(sd_pgp, 'MAILLAGE', 1, kscal=maillage)

    call getfac('OBSERVATION', nbobs)
    call pgpsav(sd_pgp, 'NB_OBSER', 1, iscal=nbobs)

    do iobs = 1, nbobs
!       -------------------------------------------------------------------------------
!       Determining the field name (DEPL / DEPL_ABSOLU/ EFGE_ELNO / etc.)
!                             type (per-node/per-elem)
!                             scalar type (real / complex)
!       -------------------------------------------------------------------------------
!       Requested field name
        call getvtx('OBSERVATION', 'NOM_CHAM', iocc=iobs, scal=champ)

!       Name of the *real restitution field* champ2
        champ2 = champ
        if (champ(5:11).eq.'_ABSOLU') champ2=champ(1:4)
        if ((champ(1:4).eq.'VITE').or.(champ(1:4).eq.'ACCE')) champ2 = 'DEPL'

!       Verify the existence of the requested field in the modal basis
        call rsexch(' ', base, champ2, 1, nomcha, iret)
        if (iret.ne.0) call utmess('F', 'PREPOST_41', si=iobs, &
                                                      nk=3, valk=[champ, champ2, base])
!       Real or complex field ?
        call dismoi('NOM_GD', nomcha, 'CHAMP', repk=nomgd)
        call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=typsc)
        call pgpsav(sd_pgp, 'TYP_SCAL' , 1, iobs=iobs, kscal=typsc)

!       -------------------------------------------------------------------------------
!       Determining and validating the list of requested component names (NOM_CMP)
!       -------------------------------------------------------------------------------
!       Which components are available for the requested field ?
        call utncmp(nomcha, nbcmp1, sd_pgp//'.AVAIL_COMP.TEMP')
        call jeveuo(sd_pgp//'.AVAIL_COMP.TEMP','L',jcomp)

!       Component names requested by the user
        call getvtx('OBSERVATION', 'NOM_CMP', iocc=iobs, nbval=0, nbret=nbcmp)
        if (nbcmp .ne. 0) then
            nbcmp = -nbcmp
            AS_ALLOCATE(vk8=lcmp, size=nbcmp)
            call getvtx('OBSERVATION', 'NOM_CMP', iocc=iobs, nbval=nbcmp, vect=lcmp)
!           Are these components all actually available ?
            do icomp = 1, nbcmp
                ncmp = lcmp(icomp)
                found = .false.
                do icmp = 1, nbcmp1
                    if (ncmp.eq.zk8(jcomp+icmp-1)) then
                        found = .true.
                        goto 10
                    end if
                end do
10              continue
!               Not available => Alert the user and stop with fatal error
                if (.not.(found)) call utmess('F','PREPOST_42', si=iobs, &
                                                                nk=2, valk=[ncmp, champ2])
            end do
        else
!           User didn't specify the component names, use the full list components
            AS_ALLOCATE(vk8=lcmp, size=nbcmp1)
            do icmp = 1, nbcmp1
                lcmp(icmp) = zk8(jcomp+icmp-1)
            end do
            nbcmp = nbcmp1
        end if

        call pgpsav(sd_pgp, 'NOM_CHAM' , 1, iobs=iobs, kscal=champ)
        call pgpsav(sd_pgp, 'NOM_CMP' , nbcmp, iobs=iobs, kvect=lcmp)

        AS_DEALLOCATE(vk8=lcmp)
        call jedetr(sd_pgp//'.AVAIL_COMP.TEMP')


!       -------------------------------------------------------------------------------
!       Interpretation of NOEUD, MAILLE, and GROUP_XX keywords and saving the node or
!       element indices
!       -------------------------------------------------------------------------------
!       Field type and supporting elements/nodes
        if (champ(6:7).ne.'EL') then
            typch = 'NOEU'
            typem = 'NU_NOEUD'
            nbmcl = 4
            limocl(1) = 'GROUP_NO'
            limocl(2) = 'NOEUD'
            limocl(3) = 'GROUP_MA'
            limocl(4) = 'MAILLE'
        else
            typch = champ(6:9)
            typem = 'NU_MAILLE'
            nbmcl = 2
            limocl(1) = 'GROUP_MA'
            limocl(2) = 'MAILLE'
        end if
        call pgpsav(sd_pgp, 'TYP_CHAM' , 1, iobs=iobs, kscal=typch)
        call reliem(modele, maillage, typem, 'OBSERVATION', iobs,&
                    nbmcl, limocl, limocl, sd_pgp//'.INDI_SUPP.TEMP', nbsupp)
        call jeveuo(sd_pgp//'.INDI_SUPP.TEMP','L',ivsup)
        if (typch.eq.'NOEU') then
            call pgpsav(sd_pgp, 'NUM_NOEU' , nbsupp, iobs=iobs, ivect=zi(ivsup))
        else 
            call pgpsav(sd_pgp, 'NUM_MAIL' , nbsupp, iobs=iobs, ivect=zi(ivsup))
        end if
        call jedetr(sd_pgp//'.INDI_SUPP.TEMP')

!       -------------------------------------------------------------------------------
!       Filling up all the remaining information which depends on the type of field
!       (per-node or per-element)
!       -------------------------------------------------------------------------------
        if (typch.eq.'NOEU')     call pgpchn(sd_pgp,iobs)
        if (typch(1:2).eq.'EL')  call pgpche(sd_pgp,iobs)

!       -------------------------------------------------------------------------------
!       Treatment of INST, LIST_INST, TOUT_INST,
!                    FREQ, LIST_FREQ, NUME_ORDRE, TOUT_ORDRE keywords
!       -------------------------------------------------------------------------------
        call pgpget(sd_pgp, 'NUM_ORDR', iobs=iobs, savejv=numejv)
        call pgpget(sd_pgp, 'DISC', iobs=iobs, savejv=discjv)
        resin19 = resin
        call rstran('NON', resin19, 'OBSERVATION', iobs, discjv,&
                    numejv, nord, iret)
        if (iret .ne. 0) call utmess('F', 'UTILITAI4_24')


!       -------------------------------------------------------------------------------
!       Treatment of particular DEPL, VITE, and ACCE fields
!       -------------------------------------------------------------------------------
!       Static correction, if present, is by default on in MONO_APPUI
        call dismoi('CORR_STAT', resin , 'RESU_DYNA', repk=corrst)
        icorrst = 0
        if (corrst(1:3).eq.'OUI') icorrst = 1
!
!       Find out if there is information on the exciting functions, if yes, then we
!       are actually in a MULT_APPUI case. Correction is thus to be done only if
!       absolute fields are explicitely requested by the user (usind psi*delta vector)
        multap = 0
        call jeexin(resin19//'.IPSD',iret)
        if (iret.ne.0) multap = 1
        if (multap.eq.1) then
            if (champ(5:11).eq.'_ABSOLU') icorrst = 1
        end if
!       Note that icorrst identifier can be = 1, in one of two cases:
!       1 - relative fields when a static correction was provided by the user during the
!           transient calculation
!       2 - absolute fields in the case of multiply pinned systems  

        if (multap.eq.0) then
            if (champ.eq.'ACCE_ABSOLU') then
!               User requested an absolute field, but no information is found regarding
!               multiply pinned system constraints
!               Before alerting, check whether an entraining acceleration field is given
!               by the keyword ACCE_MONO_APPUI
                call getvid('OBSERVATION', 'ACCE_MONO_APPUI', iocc=iobs, nbret=iret)
                if (iret.eq.0) then
!                   ACCE_ABSOLU required but nothing to add to the relative field 
                    call utmess('A', 'PREPOST_43', si=iobs, &
                                                   nk=3, valk=[resin, champ, champ(1:4)])
                end if
            else if (champ(5:11).eq.'_ABSOLU') then
!                   ****_ABSOLU required but nothing to add to the relative field 
                call utmess('A', 'PREPOST_44', si=iobs, &
                                               nk=3, valk=[resin, champ, champ(1:4)])
            end if
        end if
        call pgpsav(sd_pgp, 'ADD_CORR' , 1, iobs=iobs, iscal=icorrst)

!       Single pinned entraining acceleration (ACCE_MONO_APPUI)
        acce_mo = ' '
        if (champ.eq.'ACCE_ABSOLU') then
            call getvid('OBSERVATION', 'ACCE_MONO_APPUI', iocc=iobs, scal=acce_mo,&
                        nbret=iret)
            if (iret.ne.0) then
                if (multap.eq.1) then
!                   Care should be taken, is this system in MONO or MULTI_APPUI ?
                    call utmess('A', 'PREPOST_45', si=iobs, sk=resin)
                else 
                    call getvr8('OBSERVATION', 'DIRECTION', iocc=iobs, nbval=3, vect=dir)
                    call pgpsav(sd_pgp, 'ACC_DIR ' , 3, iobs=iobs, rvect=dir)
                end if
            end if
        end if
        call pgpsav(sd_pgp, 'ACC_MO_A' , 1, iobs=iobs, kscal=acce_mo)

    end do

    call jedema()

end subroutine