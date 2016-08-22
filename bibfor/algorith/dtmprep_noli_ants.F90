subroutine dtmprep_noli_ants(sd_dtm_, sd_nl_, icomp)
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
! dtmprep_noli_ants : prepare the calculations for a localized nonlinearity
!                     of type : anti sismic device. This routine adds a single  
!                     occurence to sd_nl and increments NB_NOLI in sd_dtm
!
!             icomp : an integer giving the index of occurence of the 
!                     nonlinearity to be treated under the factor kw
!                     COMPORTEMENT of the command DYNA_VIBRA.
!
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/getvr8.h"
#include "asterfort/gloloc.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/mdchan.h"
#include "asterfort/mdchdl.h"
#include "asterfort/mdchre.h"
#include "asterfort/nlget.h"
#include "asterfort/nlinivec.h"
#include "asterfort/nlsav.h"
#include "asterfort/nltype.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_nl_
    integer          , intent(in) :: icomp
!
!   -0.2- Local variables
    aster_logical     :: lnoeu2 
    integer           :: i, n1, ibid, nbants, nbnoli
    integer           :: nbmcl, ier, nbno1, nbno2, ino1
    integer           :: ino2, ind1, ind2, nbmode, info
    integer           :: vali, j, neq, mxlevel, nexcit
!
    real(kind=8)      :: k1, k2, seuil_fx, as_c, p_alpha
    real(kind=8)      :: xjeu, dx_max, cosb, sing, sina
    real(kind=8)      :: cosa, sinb, cosg, valr(10), ddpilo(3)
    real(kind=8)      :: dpiglo(6), dpiloc(6), one
!
    character(len=8)  :: sd_dtm, sd_nl, mesh, mesh1, mesh2
    character(len=8)  :: nume, nume1, nume2, no1_name, no2_name
    character(len=8)  :: monmot, repere, intk
    character(len=16) :: typnum, typem
    character(len=16) :: limocl(2), tymocl(2), valk(2), obst_typ, motfac
    character(len=19) :: nomres
    character(len=24) :: nl_title, mdgene
!
    integer     , pointer       :: ddlcho(:)         => null()
    real(kind=8), pointer       :: coor_no1(:)       => null()
    real(kind=8), pointer       :: coor_no2(:)       => null()
    real(kind=8), pointer       :: vale(:)           => null()
    real(kind=8), pointer       :: sincos_angle_a(:) => null()
    real(kind=8), pointer       :: sincos_angle_b(:) => null()
    real(kind=8), pointer       :: sincos_angle_g(:) => null()
    real(kind=8), pointer       :: defmod1(:)        => null()
    real(kind=8), pointer       :: defmod2(:)        => null()
    real(kind=8), pointer       :: ps2del1(:)        => null()
    real(kind=8), pointer       :: ps2del2(:)        => null()
    real(kind=8), pointer       :: origob(:)         => null()
    real(kind=8), pointer       :: bmodal_v(:)       => null()
    real(kind=8), pointer       :: ps1del_v(:)       => null()

    character(len=8) , pointer  :: noeud(:)          => null()
!
#define ps1del(m,n) ps1del_v((n-1)*neq+m)
#define bmodal(m,n) bmodal_v((n-1)*neq+m)
!
!   --- 0. Diverse initializations
    call jemarq()
!
    sd_dtm = sd_dtm_
    sd_nl  = sd_nl_
!
    lnoeu2 = .false.   
    one = 1.d0
 !
    motfac = 'COMPORTEMENT'
    call nlget(sd_nl, _MAX_LEVEL, iscal=mxlevel)
    nbnoli = mxlevel + 1
    i = mxlevel + 1   
!
    call infmaj()
    call infniv(ibid, info)
!
!   --- 1 - Basic information about the mesh and numbering 
!
    call dtmget(sd_dtm, _NUM_DDL, kscal=nume)
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode)
    call gettco(nume, typnum)

!
!   --- 1.1 - Case with a simple modal projection (direct calculation)
    if (typnum(1:16) .eq. 'NUME_DDL_SDASTER') then
        call dismoi('NOM_MAILLA' , nume , 'NUME_DDL' , repk=mesh)
        mesh1 = mesh
        nume1 = nume
        mesh2 = mesh
        nume2 = nume
        call nlsav(sd_nl, _NUMDDL_1, 1, iocc=i, kscal=nume1(1:8))
        call nlsav(sd_nl, _MESH_1, 1, iocc=i, kscal=mesh1)

!   --- 1.2 - Case with double (or triple) projections (sub-structuring case)
!             Not supported for buckling non linearities
    else if (typnum(1:13).eq.'NUME_DDL_GENE') then
        call utmess('F', 'ALGORITH5_36')
    else
        ASSERT(.false.)
    end if
!
!
!   --- 2 - Localisation (support nodes) of the buckling non linearity
!
!   --- 2.1 - Definition using nodes or nodal groups (NOEUD/GROUP_NO)
!             Unlike the chocs case, here only a single nonlinearity
!             can be defined per occurence
    typem = 'NO_NOEUD'
    nbmcl = 2
    limocl(1) = 'GROUP_NO_1'
    limocl(2) = 'NOEUD_1'
    tymocl(1) = 'GROUP_NO'
    tymocl(2) = 'NOEUD'
    call reliem(' ', mesh1, typem, motfac, icomp,&
                nbmcl, limocl, tymocl, sd_nl//'.INDI_NO1.TEMP', nbno1)
!
    if (nbno1.gt.0) then
        ASSERT(nbno1.eq.1) 
        call jeveuo(sd_nl//'.INDI_NO1.TEMP','L', vk8=noeud)
        no1_name = noeud(1)
        call nlsav(sd_nl, _NO1_NAME, 1, iocc=i, kscal=no1_name)
        call jedetr(sd_nl//'.INDI_NO1.TEMP')

        typem = 'NO_NOEUD'
        nbmcl = 2
        limocl(1) = 'GROUP_NO_2'
        limocl(2) = 'NOEUD_2'
        call reliem(' ', mesh2, typem, motfac, icomp,&
                    nbmcl, limocl, tymocl, sd_nl//'.INDI_NO2.TEMP', nbno2)

        if (nbno2.gt.0) then
            ASSERT(nbno2.eq.1)
            call jeveuo(sd_nl//'.INDI_NO2.TEMP','L', vk8=noeud)
            no2_name = noeud(1)
            call nlsav(sd_nl, _NO2_NAME, 1, iocc=i, kscal=no2_name)
            call jedetr(sd_nl//'.INDI_NO2.TEMP')
            lnoeu2 = .true.
        else
            ASSERT(.false.)
        end if

        call nlsav(sd_nl, _NUMDDL_2, 1, iocc=i, kscal=nume2(1:8))
        call nlsav(sd_nl, _MESH_2, 1, iocc=i, kscal=mesh2)
    end if
!
!   --- 3 - Filling up the sd_nl with further information regarding the 
!           nonlinearity(ies)
!
    AS_ALLOCATE(vi=ddlcho, size=6)
!
!   --- Loop over the detected nonlineary in the current COMPORTEMENT
!       occurence
!
    call nlsav(sd_nl, _NL_TYPE , 1, iocc=i, iscal=NL_ANTI_SISMIC)
!
!   --- 3.1 - DOF numbering localisation index for the concerned nodes
    call mdchdl(lnoeu2, i, ddlcho, ier)
!
!   --- 3.2 - Coordinates of the nodes
    call jeveuo(mesh1//'.COORDO    .VALE', 'L', vr=vale)
    call jenonu(jexnom(mesh1//'.NOMNOE', no1_name), ino1)
    ind1 = 1+3*(ino1-1)
    ind2 = ind1+3
    call nlsav(sd_nl, _COOR_NO1, 3, iocc=i, rvect=vale(ind1:ind2))
    if (lnoeu2) then
        if (mesh2 .ne. mesh1) then
            call jeveuo(mesh2//'.COORDO    .VALE', 'L', vr=vale)
        end if
        call jenonu(jexnom(mesh2//'.NOMNOE', no2_name), ino2)
        ind1 = 1+3*(ino2-1)
        ind2 = ind1+3
        call nlsav(sd_nl, _COOR_NO2, 3, iocc=i, rvect=vale(ind1:ind2))
    end if
!
!   --- 3.3 - Other information are read from the user input
    call codent(i, 'D0', intk)
    nl_title = nltype(NL_ANTI_SISMIC)//intk
    call nlsav(sd_nl, _NL_TITLE, 1, iocc=i, kscal=nl_title)

    call getvr8(motfac, 'RIGI_K1 ', iocc=icomp, scal=k1, nbret=n1)
    call nlsav(sd_nl, _ANTISISMIC_K1, 1, iocc=i, rscal=k1)

    call getvr8(motfac, 'RIGI_K2 ', iocc=icomp, scal=k2, nbret=n1)
    call nlsav(sd_nl, _ANTISISMIC_K2, 1, iocc=i, rscal=k2)

    call getvr8(motfac, 'SEUIL_FX ', iocc=icomp, scal=seuil_fx, nbret=n1)
    call nlsav(sd_nl, _ANTISISMIC_SEUIL_FX, 1, iocc=i, rscal=seuil_fx)

    call getvr8(motfac, 'C ', iocc=icomp, scal=as_c, nbret=n1)
    call nlsav(sd_nl, _ANTISISMIC_C, 1, iocc=i, rscal=as_c)

    call getvr8(motfac, 'PUIS_ALPHA', iocc=icomp, scal=p_alpha, nbret=n1)
    call nlsav(sd_nl, _ANTISISMIC_PUIS_ALPHA, 1, iocc=i, rscal=p_alpha)

    call getvr8(motfac, 'DX_MAX ', iocc=icomp, scal=dx_max, nbret=n1)
    call nlsav(sd_nl, _ANTISISMIC_DX_MAX, 1, iocc=i, rscal=dx_max)
    
    obst_typ = 'BI_PLANY'
    call nlsav(sd_nl, _OBST_TYP, 1, iocc=i, kscal=obst_typ)

!
!   --- 3.4 - Calculation of geometrical properties : 
!             play, orientation, local coordinates, distances
    call nlget(sd_nl, _COOR_NO1, iocc=i, vr=coor_no1)
    call nlget(sd_nl, _COOR_NO2, iocc=i, vr=coor_no2)
    xjeu =      (coor_no2(1)-coor_no1(1))**2 +&
                (coor_no2(2)-coor_no1(2))**2 +&
                (coor_no2(3)-coor_no1(3))**2
!
    call mdchre('ANTI_SISM ', icomp, i, mdgene, typnum,&
                repere, lnoeu2)
!
    call mdchan('ANTI_SISM ', icomp, i, mdgene, typnum,&
                repere, xjeu)

    call nlget(sd_nl, _COOR_ORIGIN_OBSTACLE, iocc=i, vr=origob)
    call nlget(sd_nl, _SINCOS_ANGLE_A, iocc=i, vr=sincos_angle_a)
    call nlget(sd_nl, _SINCOS_ANGLE_B, iocc=i, vr=sincos_angle_b)
    call nlget(sd_nl, _SINCOS_ANGLE_G, iocc=i, vr=sincos_angle_g)

    sina = sincos_angle_a(1)
    cosa = sincos_angle_a(2)
    sinb = sincos_angle_b(1)
    cosb = sincos_angle_b(2)
    sing = sincos_angle_g(1)
    cosg = sincos_angle_g(2)
!
!   --- Global to local (obstacle) coordinates
    dpiglo(1) = coor_no1(1)
    dpiglo(2) = coor_no1(2)
    dpiglo(3) = coor_no1(3)
    call gloloc(dpiglo, origob, sina, cosa, sinb,&
                cosb, sing, cosg, dpiloc)
!   --- Differential distance given a single node
    ddpilo(1) = dpiloc(1)
    ddpilo(2) = dpiloc(2)
    ddpilo(3) = dpiloc(3)
!
    if (obst_typ(1:2).eq.'BI') then
!       --- Initial position of node 2 in the local (obstacle) reference
        call nlget(sd_nl, _COOR_NO2, iocc=i, vr=coor_no2)
        dpiglo(4) = coor_no2(1)
        dpiglo(5) = coor_no2(2)
        dpiglo(6) = coor_no2(3)
        call gloloc(dpiglo(4), origob, sina, cosa, sinb,&
                    cosb, sing, cosg, dpiloc(4))
!   --- Differential coordinates (distance) for the binodal system
        ddpilo(1) = dpiloc(1)-dpiloc(4)
        ddpilo(2) = dpiloc(2)-dpiloc(5)
        ddpilo(3) = dpiloc(3)-dpiloc(6)
    endif
    call nlsav(sd_nl, _SIGN_DYZ, 2, iocc=i, rvect=[-sign(one,ddpilo(2)), -sign(one,ddpilo(3))])
!
!
!   --- 3.5 - Printing out user information
    if (info .eq. 2) then
        vali = i
        call nlget(sd_nl, _NO1_NAME, iocc=i, kscal=valk(1))
        call utmess('I', 'ALGORITH16_2', sk=valk(1), si=vali)

        call nlget(sd_nl, _COOR_NO1, iocc=i, rvect=valr)
        call utmess('I', 'ALGORITH16_4', nr=3, valr=valr)

        call nlget(sd_nl, _NO2_NAME, iocc=i, kscal=valk(1))
        call utmess('I', 'ALGORITH16_5', sk=valk(1))
        if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
            call nlget(sd_nl, _SS2_NAME, iocc=i, kscal=valk(1))
            call utmess('I', 'ALGORITH16_3', sk=valk(1))
        endif
        call nlget(sd_nl, _COOR_NO2, iocc=i, rvect=valr)
        call utmess('I', 'ALGORITH16_4', nr=3, valr=valr)

        valr(1)  = 0.d0
        valr(2)  = origob(1)
        valr(3)  = origob(2)
        valr(4)  = origob(3)
        valr(5)  = sincos_angle_a(1)
        valr(6)  = sincos_angle_a(2)
        valr(7)  = sincos_angle_b(1)
        valr(8)  = sincos_angle_b(2)
        valr(9)  = sincos_angle_g(1)
        valr(10) = sincos_angle_g(2)
        call utmess('I', 'ALGORITH16_8', nr=10, valr=valr)

        xjeu = sqrt(xjeu)
        valr (1) = xjeu
        call utmess('I', 'ALGORITH16_9', sr=valr(1))

        call utmess('I', 'VIDE_1')
    endif
    call nlsav(sd_nl, _GAP, 1, iocc=i, rscal=xjeu)
!
!   -- 3.6 - Modal displacements of the node(s)
!            Note : if a single node is used, we fill with zeros the
!                   deformations for node_2, this simplifies the
!                   case treatments for calculating the forces
    call dtmget(sd_dtm, _BASE_VEC, vr=bmodal_v)
    call dtmget(sd_dtm, _NB_PHYEQ, iscal=neq)
    call nlinivec(sd_nl, _MODAL_DEPL_NO1, 3*nbmode, iocc=i, vr=defmod1)
    call nlinivec(sd_nl, _MODAL_DEPL_NO2, 3*nbmode, iocc=i, vr=defmod2)

    do j = 1, nbmode
        defmod1(3*(j-1)+1) = bmodal(ddlcho(1),j)
        defmod1(3*(j-1)+2) = bmodal(ddlcho(2),j)
        defmod1(3*(j-1)+3) = bmodal(ddlcho(3),j)
        
        if (obst_typ(1:2).eq.'BI') then
            defmod2(3*(j-1)+1) = bmodal(ddlcho(4),j)
            defmod2(3*(j-1)+2) = bmodal(ddlcho(5),j)
            defmod2(3*(j-1)+3) = bmodal(ddlcho(6),j)
        else
            defmod2(3*(j-1)+1) = 0.d0
            defmod2(3*(j-1)+2) = 0.d0
            defmod2(3*(j-1)+3) = 0.d0
        endif
    enddo
!
!   --- 3.7 - Multi supported systems, filling up of psixdelta for the
!             concerned nodes
    call dtmget(sd_dtm, _MULTI_AP, kscal=monmot)
    if (monmot(1:3) .eq. 'OUI') then
        call dtmget(sd_dtm, _CALC_SD , kscal=nomres)
        call dtmget(sd_dtm, _NB_EXC_T, iscal=nexcit)
        call jeveuo(nomres//'.IPSD', 'E', vr=ps1del_v)

        call nlinivec(sd_nl, _PSI_DELT_NO1, 3*nexcit, iocc=i, vr=ps2del1)
        do j = 1, nexcit
            ps2del1(3*(j-1)+1) = ps1del(ddlcho(1),j)
            ps2del1(3*(j-1)+2) = ps1del(ddlcho(2),j)
            ps2del1(3*(j-1)+3) = ps1del(ddlcho(3),j)
        enddo
        if (obst_typ(1:2).eq.'BI') then
            call nlinivec(sd_nl, _PSI_DELT_NO2, 3*nexcit, iocc=i, vr=ps2del2)
            do j = 1, nexcit
                ps2del2(3*(j-1)+1) = ps1del(ddlcho(4),j)
                ps2del2(3*(j-1)+2) = ps1del(ddlcho(5),j)
                ps2del2(3*(j-1)+3) = ps1del(ddlcho(6),j)
            end do
        endif
    endif

!
!   --- 4 - Updating indices for sd_nl and sd_dtm
    call nlsav(sd_nl, _MAX_LEVEL, 1, iscal=nbnoli)
    call dtmsav(sd_dtm, _NB_NONLI, 1, iscal=nbnoli)
!
    call nlget(sd_nl, _NB_ANTSI, iscal = nbants)
    nbants = nbants + 1
    call nlsav(sd_nl, _NB_ANTSI, 1, iscal = nbants)
!
    AS_DEALLOCATE(vi=ddlcho)
!
    call jedema()
end subroutine
