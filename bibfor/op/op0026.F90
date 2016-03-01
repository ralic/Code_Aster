subroutine op0026()
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
#include "asterfort/nmdome.h"
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
    integer :: iret, nuord, long
    integer :: nbnobj
    real(kind=8) :: instam, instap, partps(3)
    character(len=8) :: result, newtab, oldtab
    character(len=16) :: lopt(4), option
    character(len=19) :: lischa ='&&OP0026.LISCHA', k19bla = ' '
    character(len=19) :: linst
    character(len=24) :: modele, mate, carele = '&&OP0026.CARELE'
    character(len=24) :: compor, carcri ='&&OP0026.CARCRI'
    character(len=24) :: codere, ligrmo, k24bid
    character(len=24) :: comref = '&&OP0026.COMREF'
    character(len=19) :: commoi, complu, depplu
    character(len=19) :: depmoi, depdel, varplu, sigplu, varmoi, sigmoi
    character(len=19) :: mediri, merigi, vediri, vefint, veforc, vevarc_prev, vevarc_curr
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
    nuord         = 0
    modele        = ' '
    k19bla        = ' '
    fonact(1:100) = 0
    oldtab        = ' '
    partps(1:2)   = 0.d0
!
! - Name of created table
!
    call getres(newtab, k24bid, k24bid)
!
! - Name of reused table
!
    call getvid(' ', 'TABLE', nbval=0, nbret=n1)
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
! - Model, material and loadings
!
    call nmdome(modele, mate, carele, lischa, result,&
                nuord)
    call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrmo)
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
! - Get comportment
!
    call nmdorc(modele(1:8), mate, l_etat_init, compor, carcri)
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
    call nmvcle(modele, mate, carele, instap, complu)
    call nmvcle(modele, mate, carele, instam, commoi)
!
! - Command variable reference creation
!
    call nmvcre(modele, mate, carele, comref)
!
! - Checking number of internal variables
!
    call jeexin(compor(1:19)//'.CESD', iret)
    if (iret .gt. 0) then
        call vrcomp(compor, varmoi, ligrmo, iret)
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
    call gcncon('_', codere)
    call gcncon('_', veforc)
    call gcncon('_', vevarc_prev)
    call gcncon('_', vevarc_curr)
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
        call merimo('G', modele, carele, mate, comref,&
                    compor, carcri, iterat, fonact, k19bla,&
                    valinc, solalg, merigi, vefint, option,&
                    tabret, codere)
    endif
!
! - Lagrange dof computation
!
    if (l_medime) then
        call medime('G', 'CUMU', modele, lischa, merigi)
        call vebtla('G', modele, mate, carele, depplu,&
                    lischa, vediri)
    endif
!
! - Nodal forces
!
    if (l_vefnme) then
        option = 'FORC_NODA'
        partps(1)=0.d0
        partps(2)=0.d0
        partps(3)=0.d0
        if (.not.l_merimo) call copisd('CHAMP_GD', 'V', sigmoi, sigplu)
        call vefnme(option, 'G', modele, mate, carele,&
                    compor, partps, 0, ligrmo, complu,&
                    sigplu, k24bid, depplu, ' ', veforc)
    endif
!
! - State variables
!
    if (l_varc_prev) then
        call nmvcpr(modele, mate       , carele, comref     , compor   ,&
                    valinc, base_ = 'G', vect_elem_prev_ = vevarc_prev)
    endif
    if (l_varc_curr) then
        call nmvcpr(modele, mate       , carele, comref     , compor   ,&
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
        newsd(nbnobj) = codere
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
