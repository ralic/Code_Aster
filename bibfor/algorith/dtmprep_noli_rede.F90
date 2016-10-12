subroutine dtmprep_noli_rede(sd_dtm_, sd_nl_, icomp)
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! dtmprep_noli_rede : prepare the calculations for a localized nonlinearity
!                     of type : RELA_EFFO_DEPL. This routine adds one or more 
!                     occurences to sd_nl and increments NB_NOLI in sd_dtm
!
!             icomp : an integer giving the index of occurence of the 
!                     nonlinearity to be treated under the factor kw
!                     COMPORTEMENT of the command DYNA_VIBRA.
!
#include "jeveux.h"
#include "asterfort/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jenonu.h"
#include "asterfort/mgutdm.h"
#include "asterfort/nlget.h"
#include "asterfort/nlinivec.h"
#include "asterfort/nlsav.h"
#include "asterfort/posddl.h"
#include "asterfort/resmod.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_nl_
    integer          , intent(in) :: icomp
!
!   -0.2- Local variables
    integer           :: i, j, ibid, iret, mxlevel
    integer           :: gno, nbmode, icmp, inod, ino
    integer           :: nuddl, neq, nbrede
    character(len=8)  :: sd_dtm, sd_nl, sst1, noeu, grno
    character(len=8)  :: comp, mesh1, noecho(3), nume, fonc
    character(len=14) :: nume1
    character(len=16) :: motfac, typnum, valk
    character(len=24) :: nomgr1, mdssno, numero, mdgene, nomk24
!
    character(len=24), pointer :: refe(:) => null()
    real(kind=8)     , pointer :: dplred(:) => null()
    real(kind=8)     , pointer :: dplcho(:) => null()
    real(kind=8)     , pointer :: bmodal_v(:) => null()
!
#define bmodal(m,n) bmodal_v((n-1)*neq+m)
!
!   --- 0. Diverse initializations
    call jemarq()
!
    sd_dtm = sd_dtm_
    sd_nl  = sd_nl_
    motfac = 'COMPORTEMENT'
    call nlget(sd_nl, _MAX_LEVEL, iscal=mxlevel)
    i = mxlevel + 1
!
!   --- 1 - Basic information about the mesh and numbering 
!
    call dtmget(sd_dtm, _NUM_DDL, kscal=nume)
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode)
    call gettco(nume, typnum)

    if (typnum(1:16) .eq. 'NUME_DDL_SDASTER') then
        nume1 = nume
        call dismoi('NOM_MAILLA' , nume , 'NUME_DDL', repk=mesh1)
        call nlsav(sd_nl, _NUMDDL_1, 1, iocc=i, kscal=nume1(1:8))
        call nlsav(sd_nl, _MESH_1, 1, iocc=i, kscal=mesh1)

!   --- 1.2 - Case with double (or triple) projections (sub-structuring case)
    else if (typnum(1:13).eq.'NUME_DDL_GENE') then
        call jeveuo(nume//'      .NUME.REFN', 'L', vk24=refe)
        mdgene = refe(1)

        call getvtx(motfac, 'SOUS_STRUC', iocc=icomp, scal=sst1, nbret=iret)
        if (iret .eq. 0) call utmess('F', 'ALGORITH5_31', sk='SOUS_STRUC')

        mdssno = mdgene(1:14)//'.MODG.SSNO'
        call jenonu(jexnom(mdssno, sst1), iret)
        if (iret .eq. 0) then
            call utmess('F', 'ALGORITH5_64')
        endif

        call mgutdm(mdgene, sst1, ibid, 'NOM_NUME_DDL', ibid, nume1)
        call mgutdm(mdgene, sst1, ibid, 'NOM_MAILLAGE', ibid, mesh1)
!
        call nlsav(sd_nl, _SS1_NAME, 1, iocc=i, kscal=sst1)
        call nlsav(sd_nl, _NUMDDL_1, 1, iocc=i, kscal=nume1(1:8))
        call nlsav(sd_nl, _MESH_1, 1, iocc=i, kscal=mesh1)
    end if
!
    call getvtx(motfac, 'NOEUD', iocc=icomp, scal=noeu, nbret=ino)
    call getvtx(motfac, 'GROUP_NO', iocc=icomp, scal=grno, nbret=gno)
    call getvtx(motfac, 'NOM_CMP', iocc=icomp, scal=comp)
    call getvid(motfac, 'FONCTION', iocc=icomp, scal=fonc)
!
    ASSERT(ino.gt.0 .or. gno.gt.0)

    if (gno .ne. 0) then
        call getvem(mesh1, 'GROUP_NO', motfac, 'GROUP_NO', icomp,&
                        1, 1, nomgr1, ibid)
        call utnono(' ', mesh1, 'NOEUD', nomgr1, noeu, iret)
!             # Si le GROUP_NO contient plus d'un noeud
        if (iret .eq. 1)  call utmess('F','ALGORITH5_57', sk=nomgr1)
    end if

    call jenonu(jexnom(mesh1//'.NOMNOE', noeu), inod)

    call nlsav(sd_nl, _NO1_NAME, 1, iocc=i, kscal=noeu)
    call nlsav(sd_nl, _CMP_NAME, 1, iocc=i, kscal=comp(1:8))
    call nlsav(sd_nl, _FX_FONCT, 1, iocc=i, kscal=fonc)

!
    if (comp(1:2) .eq. 'DX') icmp = 1
    if (comp(1:2) .eq. 'DY') icmp = 2
    if (comp(1:2) .eq. 'DZ') icmp = 3
    if (comp(1:3) .eq. 'DRX') icmp = 4
    if (comp(1:3) .eq. 'DRY') icmp = 5
    if (comp(1:3) .eq. 'DRZ') icmp = 6
!
    call posddl('NUME_DDL', nume1, noeu, comp, inod,&
                    nuddl)
!
    if (nuddl .eq. 0) then
        valk = noeu
        call utmess('F+', 'ALGORITH15_16', sk=valk)
        if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
            valk = sst1
            call utmess('F+', 'ALGORITH15_17', sk=valk)
        endif
        valk = comp
        call utmess('F', 'ALGORITH15_18', sk=valk)
    endif
!
    call nlinivec(sd_nl, _MODAL_DEPL_NO1, 6*nbmode, iocc=i, vr=dplred)

    call dtmget(sd_dtm, _BASE_VEC, vr=bmodal_v)
    call dtmget(sd_dtm, _NB_PHYEQ, iscal=neq)
!
! ----- CALCUL DIRECT
    if (typnum .eq. 'NUME_DDL_SDASTER') then
        do j = 1, nbmode
            dplred((j-1)*6+icmp) = bmodal(nuddl,j)
        end do
!
! ----- CALCUL PAR SOUS-STRUCTURATION
    else if (typnum(1:13).eq.'NUME_DDL_GENE') then
        AS_ALLOCATE(vr=dplcho, size=nbmode*6)
        numero    = nume
        noecho(1) = noeu
        noecho(2) = sst1
        noecho(3) = nume1(1:8)
        call resmod(bmodal_v, nbmode, neq, numero, mdgene,&
                    noecho, dplcho)
        do j = 1, nbmode
            dplred((j-1)*6+icmp) = dplcho(j+(icmp-1)*nbmode)
        end do
        AS_DEALLOCATE(vr=dplcho)
    endif

!
!   --- Special treatment to save NOEUD, NOM_CMP, and FONCTION in the _NL_TITLE entry
!       This could be used by POST_DYNA_MODA_T
    nomk24 = '                        '
    nomk24( 1:8)  = noeu
    nomk24( 9:16) = comp
    nomk24(17:24) = fonc
    call nlsav(sd_nl, _NL_TITLE, 1, iocc=i, kscal=nomk24)

!
!   --- 2 - Updating indices for sd_nl and sd_dtm
    mxlevel = mxlevel + 1
    call nlsav(sd_nl, _MAX_LEVEL, 1, iscal=mxlevel)
    call dtmsav(sd_dtm, _NB_NONLI, 1, iscal=mxlevel)
!
    call nlget(sd_nl, _NB_REL_FX, iscal = nbrede)
    nbrede = nbrede + 1
    call nlsav(sd_nl, _NB_REL_FX, 1, iscal = nbrede)
!
    call nlsav(sd_nl, _NL_TYPE , 1, iocc=i, iscal=NL_FX_RELATIONSHIP)
!
    call jedema()
end subroutine
