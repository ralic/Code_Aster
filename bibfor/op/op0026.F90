subroutine op0026()
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
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
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mesg.h"
#include "asterfort/vebtla.h"
#include "asterfort/vrcomp.h"
!
! ----------------------------------------------------------------------
!
!           O P E R A T E U R    C A L C U L
!           ================================
!
! ----------------------------------------------------------------------
!
!
    integer :: zsolal, zvalin
    parameter       (zsolal=17,zvalin=28)
    character(len=19) :: valinc(zvalin), solalg(zsolal)
!
    integer :: nbomax
    parameter       (nbomax=7)
    character(len=16) :: newobj(nbomax)
    character(len=24) :: newsd(nbomax)
!-----------------------------------------------------------------------
    integer :: n1, nbopt, iterat, numins, i, ibid
    integer :: niv, ifm
    integer :: iret, nuord, long
    integer :: nbnobj
    real(kind=8) :: instam, instap, partps(3)
    character(len=2) :: codret
    character(len=8) :: result, newtab, oldtab
    character(len=16) :: lopt(4), option
    character(len=19) :: lischa, k19bla
    character(len=19) :: linst
    character(len=24) :: modele, mate, carele, compor, carcri
    character(len=24) :: codere, ligrmo
    character(len=24) :: comref, k24bid
    character(len=19) :: commoi, complu, depplu
    character(len=19) :: depmoi, depdel, varplu, sigplu, varmoi, sigmoi
    character(len=19) :: mediri, merigi, vediri, vefint, veforc
    logical :: lmatr, lvnod, lvfin, lcomp
    logical :: l_merimo, l_medime, l_vefnme
    logical :: tabret(0:10)
    integer :: fonact(100)
!-----------------------------------------------------------------------
    data lischa     /'&&OP0026.LISCHA'/
    data carele     /'&&OP0026.CARELE'/
    data carcri     /'&&OP0026.CARCRI'/
    data comref     /'&&OP0026.COMREF'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
! - Initializations
!
    nuord = 0
    modele = ' '
    k19bla = ' '
    do i = 1, 100
        fonact(i) = 0
    end do
    oldtab = ' '
    partps(1) = 0.d0
    partps(2) = 0.d0
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
    call getvtx(' ', 'OPTION', nbval=4, vect=lopt, nbret=nbopt)
!
! - Model, material and loadings
!
    call nmdome(modele, mate, carele, lischa, result,&
                nuord)
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrmo, iret)
!
! - Get displacements
!
    call getvid(' ', 'DEPL', scal=depmoi, nbret=n1)
    call getvid(' ', 'INCR_DEPL', scal=depdel, nbret=n1)
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
!
! - Get internal variables
!
    call getvid(' ', 'VARI', scal=varmoi, nbret=n1)
    call chpver('F', varmoi, 'ELGA', 'VARI_R', iret)
    call nmcha0('VALINC', 'VARMOI', varmoi, valinc)
!
! - Get comportment
!
    call nmdorc(modele, compor, carcri)
!
! - Get current time
!
    linst = ' '
    call getvis('INCREMENT', 'NUME_ORDRE', iocc=1, scal=numins, nbret=n1)
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=linst, nbret=n1)
    instap = diinst(linst,numins-1)
    instam = diinst(linst,numins)
    partps(1) = instam
    partps(2) = instap
!
! - Command variables
!
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmvcle(modele, mate, carele, lischa, instap,&
                complu, codret)
    call nmvcle(modele, mate, carele, lischa, instam,&
                commoi, codret)
!
! - Command variable reference creation
!
    call nmvcre(modele, mate, carele, comref)
!
! - Checking number of internal variables
!
    call jeexin(compor(1:19)//'.CESD', iret)
    if (iret .gt. 0) call vrcomp(' ', compor, varmoi, ligrmo)
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
        call vefnme(modele, sigplu, carele, depplu, ' ',&
                    veforc, mate, compor, 0, .false.,&
                    partps, k24bid, complu, ligrmo, option,&
                    carcri, 'G')
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
!
! - Table management
!
    call catabl(newtab, oldtab, instap, numins, nbnobj,&
                newobj, newsd)
!
    call jedema()
!
end subroutine
