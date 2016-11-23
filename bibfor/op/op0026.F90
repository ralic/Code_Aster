subroutine op0026()
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/catabl.h"
#include "asterfort/chpver.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/knindi.h"
#include "asterfort/medime.h"
#include "asterfort/merimo.h"
#include "asterfort/mvnume.h"
#include "asterfort/nmch1p.h"
#include "asterfort/nmch2p.h"
#include "asterfort/nmcha0.h"
#include "asterfort/nmchai.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmlect.h"
#include "asterfort/nmdorc.h"
#include "asterfort/nmvcle.h"
#include "asterfort/nmvcre.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/vebtla.h"
#include "asterfort/vefnme.h"
#include "asterfort/vrcomp.h"
#include "asterfort/nmvcpr.h"
#include "asterfort/nonlinDSConstitutiveInit.h"
#include "asterfort/nonlinDSConstitutiveCreate.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODifY
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
! ALONG WITH THIS PROGRAM; if NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!

!
! --------------------------------------------------------------------------------------------------
!
!  O P E R A T E U R    C A L C U L
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: zsolal = 17
    integer, parameter :: zvalin = 28
    character(len=19) :: valinc(zvalin), solalg(zsolal)
    integer, parameter :: nbomax = 9
    character(len=16) :: newobj(nbomax)
    character(len=24) :: newsd(nbomax)
    integer :: n1, nbopt, iterat, numins
    integer :: niv, ifm
    integer :: iret, long
    integer :: nbnobj
    real(kind=8) :: instam, instap, partps(3)
    character(len=8) :: result, newtab, oldtab
    type(NL_DS_Constitutive) :: ds_constitutive
    character(len=16) :: lopt(4), option
    character(len=19) :: list_load ='&&OP0026.LISCHA', k19bla = ' '
    character(len=19) :: linst
    character(len=24) :: model, mate, cara_elem = '&&OP0026.CARELE'
    character(len=24) :: ligrmo, k24bid
    character(len=24) :: comref = '&&OP0026.COMREF'
    character(len=19) :: commoi, complu, depplu
    character(len=19) :: depmoi, depdel, varplu, sigplu, varmoi, sigmoi
    character(len=19) :: mediri, merigi, vediri, vefint, veforc, vefori(2), vevarc_prev, vevarc_curr
    aster_logical :: lmatr, lvnod, lvfin, lcomp, l_varc_prev, l_varc_curr
    aster_logical :: l_merimo, l_medime, l_vefnme, l_etat_init
    aster_logical :: tabret(0:10)
    integer :: fonact(100)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
! - Initializations
!
    model         = ' '
    k19bla        = ' '
    fonact(1:100) = 0
    
    partps(1:2)   = 0.d0
!
! - Name of created table
!
    call getres(newtab, k24bid, k24bid)
!
! - Name of reused table
!
    call getvid(' ', 'TABLE', nbval=0, nbret=n1)
    if (n1 .eq. 0) then
        oldtab        = ' '
    else
        call getvid(' ', 'TABLE', nbval=1, scal = oldtab)
        if (oldtab .ne. newtab) then
            call utmess('F', 'CALCUL1_3')
        endif
    endif
!
! - Collecting variables
!
    call nmchai('VALINC', 'LONMAX', long)
    ASSERT(long.eq.zvalin)
    call nmch1p(valinc)
    call nmchai('SOLALG', 'LONMAX', long)
    ASSERT(long.eq.zsolal)
    call nmch2p(solalg)
!
! - Options
!
    call getvtx(' ', 'OPTION', nbval=6, vect=lopt, nbret=nbopt)
!
! - Get parameters from command file
!
    call nmlect(result, model, mate, cara_elem, list_load)
    call dismoi('NOM_LIGREL', model, 'MODELE', repk=ligrmo)
!
! - Get displacements
!
    call getvid(' ', 'DEPL', scal=depmoi)
    call getvid(' ', 'INCR_DEPL', scal=depdel)
    call nmcha0('VALINC', 'DEPMOI', depmoi, valinc)
    call nmcha0('SOLALG', 'DEPDEL', depdel, solalg)
!
! - Compute current displacements
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call mvnume(depmoi, depdel, depplu)
!
! - Get stresses
!
    call getvid(' ', 'SIGM', scal=sigmoi, nbret=n1)
    call chpver('F', sigmoi, 'ELGA', 'SIEF_R', iret)
    call nmcha0('VALINC', 'SIGMOI', sigmoi, valinc)
    l_etat_init = n1.ne.0
!
! - Get internal variables
!
    call getvid(' ', 'VARI', scal=varmoi)
    call chpver('F', varmoi, 'ELGA', 'VARI_R', iret)
    call nmcha0('VALINC', 'VARMOI', varmoi, valinc)
!
! - Create constitutive laws management datastructure
!
    call nonlinDSConstitutiveCreate(ds_constitutive)
!
! - Get comportment
!
    call nmdorc(model, mate, l_etat_init,&
                ds_constitutive%compor, ds_constitutive%carcri, ds_constitutive%mult_comp)
!
! - Initializations for constitutive laws
!
    call nonlinDSConstitutiveInit(model, cara_elem, ds_constitutive)
!
! - Get current time
!
    linst = ' '
    call getvis('INCREMENT', 'NUME_ORDRE', iocc=1, scal=numins)
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=linst)
    instam = diinst(linst,numins-1)
    instap = diinst(linst,numins)
    partps(1) = instam
    partps(2) = instap
!
! - Command variables
!
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmvcle(model, mate, cara_elem, instap, complu)
    call nmvcle(model, mate, cara_elem, instam, commoi)
!
! - Command variable reference creation
!
    call nmvcre(model, mate, cara_elem, comref)
!
! - Checking number of internal variables
!
    call jeexin(ds_constitutive%compor(1:19)//'.CESD', iret)
    if (iret .gt. 0) then
        call vrcomp(ds_constitutive%compor, varmoi, ligrmo, iret)
        if (iret .eq. 1) then
            call utmess('F', 'CALCUL1_5')
        endif
    endif
!
! - Datastructures name (automatic génération)
!
    call gcncon('_', sigplu)
    call gcncon('_', varplu)
    call gcncon('_', merigi)
    call gcncon('_', vefint)
    call gcncon('_', mediri)
    call gcncon('_', vediri)
    call gcncon('_', veforc)
    call gcncon('_', vevarc_prev)
    call gcncon('_', vevarc_curr)
    call gcncon('_', ds_constitutive%comp_error)
!
! - Changeing names of variables
!
    call nmcha0('VALINC', 'SIGPLU', sigplu, valinc)
    call nmcha0('VALINC', 'VARPLU', varplu, valinc)
!
! - What we are computing
!
    lcomp = .false.
    lmatr = .false.
    lvfin = .false.
    lvnod = .false.
    l_varc_prev = .false.
    l_varc_curr = .false.
    if (knindi(16,'COMPORTEMENT',lopt,nbopt) .gt. 0) then
        lcomp = .true.
    endif
    if (knindi(16,'MATR_TANG_ELEM',lopt,nbopt) .gt. 0) then
        lmatr = .true.
    endif
    if (knindi(16,'FORC_INTE_ELEM',lopt,nbopt) .gt. 0) then
        lvfin = .true.
    endif
    if (knindi(16,'FORC_NODA_ELEM',lopt,nbopt) .gt. 0) then
        lvnod = .true.
    endif
    if (knindi(16,'FORC_VARC_ELEM_M',lopt,nbopt) .gt. 0) then
        l_varc_prev = .true.
    endif
    if (knindi(16,'FORC_VARC_ELEM_P',lopt,nbopt) .gt. 0) then
        l_varc_curr = .true.
    endif
!
! - Where we are computing
!
    l_merimo = .false.
    l_medime = .false.
    l_vefnme = .false.
    if (lcomp .or. lmatr .or. lvfin) l_merimo = .true.
    if (lmatr) l_medime = .true.
    if (lvnod) l_vefnme = .true.
!
! - How we are computing
!
    option = ' '
    if (lmatr) then
        option = 'FULL_MECA'
    else
        option = 'RAPH_MECA'
    endif
!
! - Physical dof computation
!
    if (l_merimo) then
        iterat=1
        call merimo('G', model, cara_elem, mate, comref,&
                    ds_constitutive, iterat, fonact, k19bla,&
                    valinc, solalg, merigi, vefint, option,&
                    tabret)
    endif
!
! - Lagrange dof computation
!
    if (l_medime) then
        call medime('G', 'CUMU', model, list_load, merigi)
        call vebtla('G', model, mate, cara_elem, depplu,&
                    list_load, vediri)
    endif
!
! - Nodal forces
!
    if (l_vefnme) then
        option = 'FORC_NODA'
        partps(1)=0.d0
        partps(2)=0.d0
        partps(3)=0.d0
        vefori(1)=veforc
        vefori(2)=' '

        if (.not.l_merimo) call copisd('CHAMP_GD', 'V', sigmoi, sigplu)
        call vefnme(option, 'G', model, mate, cara_elem,&
                    ds_constitutive%compor, partps, 0, ligrmo, complu,&
                    sigplu, k24bid, depplu, ' ', vefori)
    endif
!
! - State variables
!
    if (l_varc_prev) then
        call nmvcpr(model, mate       , cara_elem, comref     , ds_constitutive%compor   ,&
                    valinc, base_ = 'G', vect_elem_prev_ = vevarc_prev)
    endif
    if (l_varc_curr) then
        call nmvcpr(model, mate       , cara_elem, comref     , ds_constitutive%compor   ,&
                    valinc, base_ = 'G', vect_elem_curr_ = vevarc_curr)
    endif
!
! - New objects in table
!
    nbnobj = 0
    if (l_medime) then
        nbnobj = nbnobj + 1
        ASSERT(nbnobj.le.nbomax)
        newobj(nbnobj) = 'FORC_DIRI_ELEM'
        newsd(nbnobj) = vediri
    endif
    if (l_merimo) then
        nbnobj = nbnobj + 1
        ASSERT(nbnobj.le.nbomax)
        newobj(nbnobj) = 'FORC_INTE_ELEM'
        newsd(nbnobj) = vefint
        nbnobj = nbnobj + 1
        ASSERT(nbnobj.le.nbomax)
        newobj(nbnobj) = 'SIEF_ELGA'
        newsd(nbnobj) = sigplu
        nbnobj = nbnobj + 1
        ASSERT(nbnobj.le.nbomax)
        newobj(nbnobj) = 'VARI_ELGA'
        newsd(nbnobj) = varplu
        nbnobj = nbnobj + 1
        ASSERT(nbnobj.le.nbomax)
        newobj(nbnobj) = 'CODE_RETOUR_INTE'
        newsd(nbnobj) = ds_constitutive%comp_error
        if (lmatr) then
            nbnobj = nbnobj + 1
            ASSERT(nbnobj.le.nbomax)
            newobj(nbnobj) = 'MATR_TANG_ELEM'
            newsd(nbnobj) = merigi
        endif
    endif
    if (l_vefnme) then
        nbnobj = nbnobj + 1
        ASSERT(nbnobj.le.nbomax)
        newobj(nbnobj) = 'FORC_NODA_ELEM'
        newsd(nbnobj) = veforc
    endif
    if (l_varc_prev) then
        nbnobj = nbnobj + 1
        ASSERT(nbnobj.le.nbomax)
        newobj(nbnobj) = 'FORC_VARC_ELEM_M'
        newsd(nbnobj) = vevarc_prev
    endif
    if (l_varc_curr) then
        nbnobj = nbnobj + 1
        ASSERT(nbnobj.le.nbomax)
        newobj(nbnobj) = 'FORC_VARC_ELEM_P'
        newsd(nbnobj) = vevarc_curr
    endif
!
! - Table management
!
    call catabl(newtab, oldtab, instap, numins, nbnobj,&
                newobj, newsd)
!
    call jedema()
!
end subroutine
