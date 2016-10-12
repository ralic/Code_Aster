subroutine dtmprep_noli_rotf(sd_dtm_, sd_nl_, icomp)
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
! dtmprep_noli_rotf : prepare the calculations for a localized nonlinearity
!                     of type : CRACKED_ROTOR. This routine adds one or more 
!                     occurences to sd_nl and increments NB_NOLI in sd_dtm
!
!             icomp : an integer giving the index of occurence of the 
!                     nonlinearity to be treated under the factor kw
!                     COMPORTEMENT of the command DYNA_VIBRA.
!
#include "jeveux.h"
#include "asterfort/gettco.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mdchdl.h"
#include "asterfort/nlget.h"
#include "asterfort/nlinivec.h"
#include "asterfort/nlsav.h"
#include "asterfort/nltype.h"
#include "asterfort/utmess.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/angvx.h"
#include "asterfort/getvem.h"
#include "asterfort/utnono.h"
#include "asterfort/posddl.h"

!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_nl_
    integer          , intent(in) :: icomp
!
!   -0.2- Local variables
    aster_logical     :: lnoeu2, memail
    integer           :: ibid, iret, ier, ino1, ino2
    integer           :: ind1, ind2, inod, gnod, inog
    integer           :: gnog, bono1, bono2, compt1, compt2
    integer           :: iarg, ipat, j1, j2, nbmail
    integer           :: nbno, nddl1, nddl2, nbmode
    integer           :: j, neq, mxlevel, nbrfis, nn1
    integer           :: nn2, numai, i
!
    real(kind=8)      :: angini, rad, alpha, beta, axe(3)
!
    character(len=3)  :: comp(3)
    character(len=8)  :: sd_dtm, sd_nl, mesh, mesh1, mesh2
    character(len=8)  :: noeu1, noeu2, grno1, grno2, foncp
    character(len=8)  :: fk, dfk, nomno1, nomno2, nume
    character(len=8)  :: nume1, nume2, intk
    character(len=16) :: typnum, valk(2), motfac
    character(len=24) :: nomgr1, nomgr2, nl_title
!
    integer     , pointer       :: ddlcho(:)         => null()
    real(kind=8), pointer       :: vale(:)           => null()
    real(kind=8), pointer       :: defmod1(:)        => null()
    real(kind=8), pointer       :: defmod2(:)        => null()
    real(kind=8), pointer       :: bmodal_v(:)       => null()
!
#define bmodal(m,n) bmodal_v((n-1)*neq+m)
!
!   --- 0. Diverse initializations
    call jemarq()

    sd_dtm = sd_dtm_
    sd_nl  = sd_nl_

    motfac = 'COMPORTEMENT'
    call nlget(sd_nl, _MAX_LEVEL, iscal=mxlevel)
    i = mxlevel + 1

    lnoeu2 = .true.
    rad = r8dgrd()
!
!
!   Basic information about the mesh and numbering 
!
    call dtmget(sd_dtm, _NUM_DDL, kscal=nume)
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode)
    call gettco(nume, typnum)

!
!   Case with a simple modal projection (direct calculation)
    if (typnum(1:16) .eq. 'NUME_DDL_SDASTER') then
        call dismoi('NOM_MAILLA' , nume , 'NUME_DDL' , repk=mesh)
        mesh1 = mesh
        nume1 = nume
        mesh2 = mesh
        nume2 = nume
        call nlsav(sd_nl, _NUMDDL_1, 1, iocc=i, kscal=nume1(1:8))
        call nlsav(sd_nl, _MESH_1, 1, iocc=i, kscal=mesh1)
        call nlsav(sd_nl, _NUMDDL_2, 1, iocc=i, kscal=nume2(1:8))
        call nlsav(sd_nl, _MESH_2, 1, iocc=i, kscal=mesh2)

    else
        ASSERT(.false.)
    end if
!
    call getvtx(motfac, 'NOEUD_D', iocc=icomp, scal=noeu1, nbret=inod)
    call getvtx(motfac, 'GROUP_NO_D', iocc=icomp, scal=grno1, nbret=gnod)
    call getvtx(motfac, 'NOEUD_G', iocc=icomp, scal=noeu2, nbret=inog)
    call getvtx(motfac, 'GROUP_NO_G', iocc=icomp, scal=grno2, nbret=gnog)
    call getvr8(motfac, 'ANGL_INIT', iocc=icomp, scal=angini)
    call getvid(motfac, 'ANGL_ROTA', iocc=icomp, scal=foncp,nbret=iret)
    call getvid(motfac, 'K_PHI', iocc=icomp, scal=fk)
    call getvid(motfac, 'DK_DPHI', iocc=icomp, scal=dfk)

    angini=angini*rad
    call nlsav(sd_nl, _ANG_INIT, 1, iocc=i, rscal=angini)
    if (iret .ne. 0) then
        call nlsav(sd_nl, _ANG_ROTA, 1, iocc=i, kscal=foncp)
    endif
    call nlsav(sd_nl, _ROTR_FK, 1, iocc=i, kscal=fk)
    call nlsav(sd_nl, _ROTR_DFK, 1, iocc=i, kscal=dfk)


    AS_ALLOCATE(vi=ddlcho, size=6)
    call jeveuo(mesh1//'.COORDO    .VALE', 'L', vr=vale)
    comp(1)='DRX'
    comp(2)='DRY'
    comp(3)='DRZ'

    call getvem(mesh1, 'NOEUD', motfac, 'NOEUD_D', icomp, iarg, 1, nomno1, ibid)
    call getvem(mesh1, 'NOEUD', motfac, 'NOEUD_G', icomp, iarg, 1, nomno2, ibid)
!
    call getvem(mesh1, 'GROUP_NO', motfac, 'GROUP_NO_D', icomp, iarg, 1, nomgr1, ibid)
    if (ibid .ne. 0) then
        call utnono(' ', mesh1, 'NOEUD', nomgr1, nomno1, iret)
        if (iret .eq. 10) then
            call utmess('F', 'ELEMENTS_67', sk=nomgr1)
        else if (iret.eq.1) then
            valk (1) = nomgr1(1:16)
            valk (2) = nomno1
            call utmess('A', 'ALGORITH13_41', nk=2, valk=valk)
        endif
    endif
!
    call nlsav(sd_nl, _NO1_NAME, 1, iocc=i, kscal=nomno1)

    call getvem(mesh1, 'GROUP_NO', motfac, 'GROUP_NO_G', icomp, iarg, 1, nomgr2, ibid)
    if (ibid .ne. 0) then
        call utnono(' ', mesh1, 'NOEUD', nomgr2, nomno2, iret)
        if (iret .eq. 10) then
            call utmess('F', 'ELEMENTS_67', sk=nomgr2)
        else if (iret.eq.1) then
            valk (1) = nomgr2(1:16)
            valk (2) = nomno2
            call utmess('A', 'ALGORITH13_41', nk=2, valk=valk)
        endif
    endif
!
    call nlsav(sd_nl, _NO2_NAME, 1, iocc=i, kscal=nomno2)

    do ipat = 1, 3
        call posddl('NUME_DDL', nume, nomno1, comp(ipat), nn1, nddl1)
        call posddl('NUME_DDL', nume, nomno2, comp(ipat), nn2, nddl2)
        ddlcho(ipat) = nddl1
        ddlcho(ipat+3) = nddl2
    enddo
!
!   Determine the direction and orientation of the rotor
    compt1=0; compt2=0
    bono1=0; bono2=0
    call jelira(mesh1//'.CONNEX', 'NMAXOC', nbmail)
    do numai = 1, nbmail
        call jelira(jexnum(mesh1//'.CONNEX', numai), 'LONMAX', nbno)
        if ((nbno.gt.1) .and. (nbno.lt.4)) then
            call jeveuo(jexnum(mesh1//'.CONNEX', numai), 'L', ibid)
            do j1 = 1, nbno
                if (zi(ibid+j1-1) .eq. nn1) then
                    memail=.false.
                    do j2 = 1, nbno
                        if (zi(ibid+j2-1) .eq. nn2) memail= .true.
                    enddo
                    if (.not.memail) then
                        compt1=compt1+1
                        if (j1 .eq. 1) bono1=zi(ibid+1)
                        if (j1 .eq. 2) bono1=zi(ibid)
                    endif
                endif
                if (zi(ibid+j1-1) .eq. nn2) then
                    memail=.false.
                    do j2 = 1, nbno
                        if (zi(ibid+j2-1) .eq. nn1) memail= .true.
                    enddo
                    if (.not.memail) then
                        compt2=compt2+1
                        if (j1 .eq. 1) bono2=zi(ibid+1)
                        if (j1 .eq. 2) bono2=zi(ibid)
                    endif
                endif
            enddo
        endif
    enddo
    ASSERT(compt1 .ge. 1)
    ASSERT(compt2 .ge. 1)
!
    do j = 1, 3
        axe(j)=vale(1+3*(bono1-1)+j-1) - vale(1+3*(bono2-1)+j-1)
    enddo
!
!   Rotor orientation
    call angvx(axe, alpha, beta)
    call nlsav(sd_nl, _SINCOS_ANGLE_A, 2, iocc=i, rvect=[sin(alpha), cos(alpha)])
    call nlsav(sd_nl, _SINCOS_ANGLE_B, 2, iocc=i, rvect=[sin(beta), cos(beta)])
!

    call nlget(sd_nl, _NB_R_FIS, iscal = nbrfis)
    nbrfis = nbrfis + 1
    call nlsav(sd_nl, _NL_TYPE , 1, iocc=i, iscal=NL_CRACKED_ROTOR)
!
!   DOF numbering localisation index for the concerned nodes
    call mdchdl(lnoeu2, nbrfis, ddlcho, ier)
!
!   Coordinates of the nodes
    call jeveuo(mesh1//'.COORDO    .VALE', 'L', vr=vale)
    call jenonu(jexnom(mesh1//'.NOMNOE', nomno1), ino1)
    ind1 = 1+3*(ino1-1)
    ind2 = ind1+3
    call nlsav(sd_nl, _COOR_NO1, 3, iocc=i, rvect=vale(ind1:ind2))
    if (mesh2 .ne. mesh1) then
        call jeveuo(mesh2//'.COORDO    .VALE', 'L', vr=vale)
    end if
    call jenonu(jexnom(mesh2//'.NOMNOE', nomno2), ino2)
    ind1 = 1+3*(ino2-1)
    ind2 = ind1+3
    call nlsav(sd_nl, _COOR_NO2, 3, iocc=i, rvect=vale(ind1:ind2))
!
!   Modal displacements of the node(s)
!   Note : if a single node is used, we fill with zeros the
!          deformations for node_2, this simplifies the
!          case treatments for calculating the forces
    call dtmget(sd_dtm, _BASE_VEC, vr=bmodal_v)
    call dtmget(sd_dtm, _NB_PHYEQ, iscal=neq)
    call nlinivec(sd_nl, _MODAL_DEPL_NO1, 3*nbmode, iocc=i, vr=defmod1)
    call nlinivec(sd_nl, _MODAL_DEPL_NO2, 3*nbmode, iocc=i, vr=defmod2)

    do j = 1, nbmode
        defmod1(3*(j-1)+1) = bmodal(ddlcho(1)+3,j)
        defmod1(3*(j-1)+2) = bmodal(ddlcho(2)+3,j)
        defmod1(3*(j-1)+3) = bmodal(ddlcho(3)+3,j)

        defmod2(3*(j-1)+1) = bmodal(ddlcho(4)+3,j)
        defmod2(3*(j-1)+2) = bmodal(ddlcho(5)+3,j)
        defmod2(3*(j-1)+3) = bmodal(ddlcho(6)+3,j)
    enddo

    call codent(i, 'D0', intk)
    nl_title = nltype(NL_CRACKED_ROTOR)//intk
    call nlsav(sd_nl, _NL_TITLE, 1, iocc=i, kscal=nl_title)

!
!   --- 4 - Updating indices for sd_nl and sd_dtm
    call nlget(sd_nl, _MAX_LEVEL, iscal = mxlevel)
    mxlevel = mxlevel + 1
    call nlsav(sd_nl, _MAX_LEVEL, 1, iscal=mxlevel)
    call dtmsav(sd_dtm, _NB_NONLI, 1, iscal=mxlevel)
!
    call nlsav(sd_nl, _NB_R_FIS, 1, iscal = nbrfis)
!
    AS_DEALLOCATE(vi=ddlcho)
!
    call jedema()
end subroutine
