subroutine op0186()
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detmat.h"
#include "asterfort/didern.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/impfot.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/medith.h"
#include "asterfort/ntdata.h"
#include "asterfort/ntarch.h"
#include "asterfort/ntobsv.h"
#include "asterfort/nxini0.h"
#include "asterfort/nxacmv.h"
#include "asterfort/nxinit.h"
#include "asterfort/nxlect.h"
#include "asterfort/nxnewt.h"
#include "asterfort/nxpred.h"
#include "asterfort/nxrech.h"
#include "asterfort/rsinch.h"
#include "asterfort/sigusr.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vtcreb.h"
#include "asterfort/vtzero.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!

!
! --------------------------------------------------------------------------------------------------
!
! THER_NON_LINE
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_stat, matcst, coecst, reasma, arret, conver, itemax, reasvc
    aster_logical :: reasvt, reasmt, reasrg, reasms, l_dry, l_line_search, finpas, l_evol
    aster_logical :: force
    integer :: ther_crit_i(3), numins, k, icoret, nbcham, iterho
    integer :: itmax, ifm, niv, neq, iterat, jtempp, jtemp
    integer :: itab(2)
    real(kind=8) :: tpsthe(6), deltat, timet, timtdt, tps1(7)
    real(kind=8) :: tps2(4), tps3(4), tpex, ther_crit_r(2), theta, khi, rho, testr
    real(kind=8) :: testm, para(2), instap, tconso
    real(kind=8) :: rtab(2), theta_read
    character(len=1) :: creas, base
    character(len=3) :: kreas
    character(len=8) :: result, result_dry, mesh
    character(len=19) :: sdobse
    character(len=16) :: tysd
    character(len=19) :: solver, maprec, sddisc, sdcrit, varc_curr, list_load
    character(len=24) :: model, mate, cara_elem
    character(len=24) :: time, tmpchi, tmpchf, compor, vtemp, vtempm, vtempp
    character(len=24) :: vtempr, vec2nd, vec2ni, nume_dof, mediri, matass, cndirp, cn2mbr
    character(len=24) :: cnchci, cnresi, vabtla, vhydr, vhydrp
    character(len=24) :: tpscvt
    character(len=76) :: fmt2, fmt3, fmt4
    character(len=85) :: fmt1
    real(kind=8), pointer :: crtr(:) => null()
    real(kind=8), pointer :: tempm(:) => null()
!
    type(NL_DS_InOut)     :: ds_inout
    type(NL_DS_AlgoPara)  :: ds_algopara
!
    data sdcrit/'&&OP0186.CRITERE'/
    data maprec/'&&OP0186.MAPREC'/
    data result/' '/
    data cndirp/1*' '/
    data cnchci/1*' '/
    data vec2nd/'&&OP0186.2ND'/
    data vec2ni/'&&OP0186.2NI'/
    data cn2mbr/'&&OP0186.2MBRE'/
    data tmpchi,tmpchf/'&&OP0186.TCHI','&&OP0186.TCHF'/
    data vhydr,vhydrp/'&&OP0186.HY','&&OP0186.HYP'/
    data mediri/' '/
    data matass/'&&MTHASS'/
    data fmt1/'(85(''-''))'/
    data fmt2/'(A,1X,A,6X,A,9X,A,6X,A,3X,A,3X,A,1X,A)'/
    data fmt3/'(A,16X,A,8X,A,6X,A,3X,A,6X,A,4X,A)'/
    data fmt4/'(A,12X,A,2X,A,17X,A,9X,A,4X,A)'/
    data sddisc            /'&&OP0186.PARTPS'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
! - Initializations
!
    solver    = '&&OP0186.SOLVER'
    list_load = '&&OP0186.LISCHA'
    varc_curr = '&&OP0186.CHVARC'
! --- CE BOOLEEN ARRET EST DESTINE AUX DEVELOPPEUR QUI VOUDRAIENT
! --- FORCER LE CALCUL MEME SI ON N'A PAS CONVERGENCE (ARRET=TRUE)
    arret = .false.
!
! - Creation of datastructures
!
    call nxini0(ds_algopara, ds_inout)
!
! - Read parameters (linear)
!
    call ntdata(list_load, solver, matcst   , coecst  , result    ,&
                model    , mate  , cara_elem, ds_inout, theta_read)
    para(1)    = theta_read
!
! - Read parameters (non-linear)
!
    call nxlect(result     , model     , ther_crit_i, ther_crit_r, ds_inout     ,&
                ds_algopara, result_dry, compor     , l_dry      , l_line_search)
    itmax      = ther_crit_i(3)
!
! - Initializations
!
    call nxinit(model   , mate    , cara_elem, compor, list_load,&
                para    , vhydr   , sdobse   , sddisc, sdcrit   ,&
                ds_inout, nume_dof, l_stat   , l_evol, mesh     ,&
                time    )
!
    if (l_stat) then
        numins=0
    else
        numins=1
    endif
    deltat=-1.d150
!
! --- CREATION DES OBJETS DE TRAVAIL ET DES STRUCTURES DE DONNEES
    vtemp ='&&NXLECTVAR_____'
    vtempp='&&NXLECTVAR_T_MO'
    vtempm='&&NXLECTVAR_T_PL'
    vtempr='&&NXLECTVAR_INIT'


    if (l_stat) then
        call vtcreb(vtempm, 'V', 'R', nume_ddlz = nume_dof)
        call vtcreb(vtempp, 'V', 'R', nume_ddlz = nume_dof)
        call vtcreb(vtempr, 'V', 'R', nume_ddlz = nume_dof)
        call vtcreb(vec2nd, 'V', 'R', nume_ddlz = nume_dof)
        call vtcreb(vec2ni, 'V', 'R', nume_ddlz = nume_dof)
    else
        call copisd('CHAMP_GD', 'V', vtemp, vtempm)
        call copisd('CHAMP_GD', 'V', vtemp, vtempp)
        call copisd('CHAMP_GD', 'V', vtemp, vtempr)
        call copisd('CHAMP_GD', 'V', vtemp, vec2nd)
        call copisd('CHAMP_GD', 'V', vtemp, vec2ni)
    endif

    call copisd('CHAMP_GD', 'V', vhydr, vhydrp)
!
! - Total second member
!
    call copisd('CHAMP_GD', 'V', vtemp, cn2mbr)
!
! --- CALCUL DES MATRICES ELEMENTAIRES DES DIRICHLETS
    call medith(model, list_load, mediri)
!
! **********************************************************************
!                 BOUCLE SUR LES PAS DE TEMPS
! **********************************************************************
!
    call uttcpu('CPU.OP0186.1', 'INIT', ' ')
    call uttcpr('CPU.OP0186.1', 7, tps1)
    tpex = tps1(7)
    call uttcpu('CPU.OP0186.2', 'INIT', ' ')
    call uttcpu('CPU.OP0186.3', 'INIT', ' ')
    call uttcpr('CPU.OP0186.3', 4, tps3)
    reasrg = .false.
    reasms = .false.
200 continue
! --- RECUPERATION DU PAS DE TEMPS ET DES PARAMETRES DE RESOLUTION
!
    if (l_stat) then
        if (.not.l_evol) then
            instap=0.d0
            deltat=-1.d150
            theta=1.d0
            khi=0.d0
        else
            instap=diinst(sddisc, numins)
            deltat=-1.d150
            theta=1.d0
            khi=0.d0
        endif
    else
        instap = diinst(sddisc, numins)
        deltat = instap-diinst(sddisc, numins-1)
        theta=theta_read
        khi=1.d0
    endif
    para(2) = deltat
!
! --- MATRICE TANGENTE REACTUALISEE POUR UN NOUVEAU DT
!
    reasma = .true.
!
    call uttcpu('CPU.OP0186.1', 'DEBUT', ' ')
    tpsthe(1) = instap
    tpsthe(2) = deltat
    tpsthe(3) = theta
    tpsthe(4) = khi
    tpsthe(5) = r8vide()
    tpsthe(6) = r8vide()
    call utmess('I', 'MECANONLINE6_6', sr=instap)
    write (ifm,fmt1)
    write (ifm,fmt2) '|','ITERATION','RESIDU','RESIDU',&
     &      'ITERATION','COEFFICIENT','ACTUALISATION','|'
    write (ifm,fmt3) '|','RELATIF','ABSOLU','RECH. LIN.',&
     &      'RECH. LIN.','MATRICE','|'
    write (ifm,fmt4) '|','RESI_GLOB_RELA','RESI_GLOB_MAXI','RHO',&
     &      'TANGENTE','|'
    write (ifm,fmt1)
    call jelira(vtempm(1:19)//'.VALE', 'LONMAX', neq)
!
! RECUPERATION DE:
! VTEMP  --> T+,I+1BIS
! VTEMPP --> T-
    vtemp='&&NXLECTVAR_____'
!
! --- RECUPERATION DU CHAMP DE TEMPERATURE A T ET T+DT POUR LE SECHAGE
!     LOIS SECH_GRANGER ET SECH_NAPPE
    if (l_dry) then
        call gettco(result_dry, tysd)
        if (tysd(1:9) .eq. 'EVOL_THER') then
            call dismoi('NB_CHAMP_UTI', result_dry, 'RESULTAT', repi=nbcham)
            if (nbcham .gt. 0) then
                timet = instap
                timtdt = instap + deltat
                base = 'V'
                call rsinch(result_dry, 'TEMP', 'INST', timet, tmpchi,&
                            'CONSTANT', 'CONSTANT', 1, base, icoret)
                if (icoret .ge. 10) then
                    call utmess('F', 'ALGORITH8_94', sk=result_dry, si=icoret, sr=timet)
                endif
                call rsinch(result_dry, 'TEMP', 'INST', timtdt, tmpchf,&
                            'CONSTANT', 'CONSTANT', 1, base, icoret)
                if (icoret .ge. 10) then
                    call utmess('F', 'ALGORITH8_94', sk=result_dry, si=icoret, sr=timtdt)
                endif
            else
                call utmess('F', 'ALGORITH8_99', sk=result_dry)
            endif
        endif
    endif
! RE-ASSEMBLAGE DES SECONDS MEMBRES DE VECHTH/VECHNL
    reasvc = .true.
! RE-ASSEMBLAGE DES SECONDS MEMBRES DE VETNTH
    reasvt = .true.
! RE-ASSEMBLAGE DE LA MATRICE:
    reasmt = .true.
!
! ======================================================================
!  ACTUALISATION DES MATRICES ET VECTEURS POUR LE NOUVEAU PAS DE TEMPS
! ======================================================================
!
! --- ACTUALISATION DU CHARGEMENT A TMOINS
! ON ASSEMBLE LES SECONDS MEMBRES CHAR_THER_LINEAIRE+CHAR_THER_NONLIN+
! CHAR_THER_EVOLNI EN BETA DANS VEC2ND (IDEM EN RHO_CP DANS VEC2NI)
! ON ASSEMBLE LA MATRICE A = TANGENTE (MTAN_*) + DIRICHLET
    call nxacmv(model , mate  , cara_elem, list_load, nume_dof,&
                solver, l_stat, time     , tpsthe   , reasvc  ,&
                reasvt, reasmt, reasrg   , reasms   , creas   ,&
                vtemp , vhydr , varc_curr, tmpchi   , tmpchf  ,&
                vec2nd, vec2ni, matass   , maprec   , cndirp  ,&
                cnchci, mediri, compor)

!
! ======================================================================
!                        PHASE DE PREDICTION
! ======================================================================
! SECONDS MEMBRES ASSEMBLES B
! EN STATIONNAIRE: |VEC2ND - RESI_THER - (BT)*LAGRANGE|
!                  | DIRICHLET - B*TEMPERATURE INIT   |
! EN TRANSITOIRE : |            VEC2NI                |
!                  |           DIRICHLET              |
! SYSTEME LINEAIRE RESOLU:  A * (T+,1 - T-) = B
! SOLUTION: VTEMP= T- ET VTEMPM = T+,1
!
    call nxpred(model , mate  , cara_elem, list_load, nume_dof,&
                solver, l_stat, tpsthe   , time     , matass  ,&
                neq   , maprec, varc_curr, vtemp    , vtempm  ,&
                cn2mbr, vhydr , vhydrp   , tmpchi   , tmpchf  ,&
                compor, cndirp, cnchci   , vec2nd   , vec2ni  )

!
! ======================================================================
!              ITERATIONS DE LA METHODE DE NEWTON-RAPHSON
! ======================================================================
!
    iterat = 0
    itemax = .false.
    conver = .false.
!
! --- REPRISE DE LA BOUCLE D'ITERATIONS DE NEWTON-RAPHSON
!
 20 continue
!
! --- DOIT ON REACTUALISER LA MATRICE TANGENTE
!
    call uttcpu('CPU.OP0186.2', 'DEBUT', ' ')
    iterat = iterat + 1
    reasma = .false.
    kreas = 'NON'
    if (iterat .ge. itmax) itemax = .true.
    if ((ds_algopara%reac_iter.ne.0)) then
        if (mod(iterat,ds_algopara%reac_iter) .eq. 0) then
            reasma = .true.
            kreas = 'OUI'
        endif
    endif
!
! ON ASSEMBLE LE SECOND MEMBRE B= |VEC2ND - RESI_THER - (BT)*LAGRANGE|
!                                 |             0                    |
! SYSTEME LINEAIRE RESOLU:  A * (T+,I+1 - T+,I) = B
! SOLUTION: VTEMPP = T+,I+1 - T+,I
!
    call nxnewt(model      , mate       , cara_elem  , list_load, nume_dof,&
                solver     , tpsthe     , time       , matass   , cn2mbr  ,&
                maprec     , cnchci     , varc_curr  , vtemp    , vtempm  ,&
                vtempp     , vec2nd     , mediri     , conver   , vhydr   ,&
                vhydrp     , tmpchi     , tmpchf     , compor   , vabtla  ,&
                cnresi     , ther_crit_i, ther_crit_r, reasma   , testr   ,&
                testm)
!
! --- SI NON CONVERGENCE ALORS RECHERCHE LINEAIRE
!       (CALCUL DE RHO) SUR L INCREMENT VTEMPP
! --- ACTUALISATION DE LA TEMPERATURE VTEMPM AVEC L INCREMENT VTEMPP
!     MULTIPLIE PAR RHO
    rho = 0.d0
    iterho = 0
    if (.not.conver) then
        if (l_line_search) then
!
! ON CALCULE LE RHO/ VTEMPR = T+,I+1BIS = T+,1 + RHO * (T+,I+1 - T+,I)
! MINIMISE VEC2ND - RESI_THER(T+,I+1BIS) - (BT)*LAGRANGE
            call nxrech(model , mate  , cara_elem, list_load  , nume_dof   ,&
                        tpsthe, time  , neq      , compor     , varc_curr  ,&
                        vtempm, vtempp, vtempr   , vtemp      , vhydr      ,&
                        vhydrp, tmpchi, tmpchf   , vec2nd     , vabtla     ,&
                        cnresi, rho   , iterho   , ds_algopara)
        else
            rho = 1.d0
        endif
        call jeveuo(vtempp(1:19)//'.VALE', 'L', jtempp)
        call jeveuo(vtempm(1:19)//'.VALE', 'E', vr=tempm)
!
! SOLUTION: VTEMPM = VTEMPR = T+,I+1BIS
        do k = 1, neq
            tempm(k) = tempm(k) + rho*zr(jtempp+k-1)
        end do
    endif
!
    write (ifm,&
     &      '(A,1X,I5,6X,1PE12.5,4X,1PE12.5,7X,I2,5X,1PE12.5,8X,A,6X,A)'&
     &        ) '|',iterat,testr,testm,iterho,rho,kreas,'|'
!
    if (itemax .and. .not.conver) then
        write (ifm,fmt1)
        call utmess('I', 'MECANONLINE10_3')
    endif
    call uttcpu('CPU.OP0186.2', 'FIN', ' ')
    call uttcpr('CPU.OP0186.2', 4, tps2)
    if ((.not.conver) .and. (.not.itemax)) then
        if (2.d0*tps2(4) .gt. 0.95d0*tps2(1)-tps3(4)) then
            write (ifm,fmt1)
            itab(1) = numins
            rtab(1) = tps2(4)
            rtab(2) = tps2(1)
            call utmess('Z', 'DISCRETISATION2_79', si=itab(1), nr=2, valr=rtab,&
                        num_except=28)
        else
            goto 20
        endif
    else if ((.not.conver) .and. itemax .and. (.not.arret)) then
        write (ifm,fmt1)
        itab(1) = numins
        itab(2) = iterat
        call utmess('Z', 'THERNONLINE4_85', ni=2, vali=itab, num_except=22)
    endif
    write (ifm,fmt1)
!
! --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
    if (etausr() .eq. 1) then
        call sigusr()
    endif
!
! ======================================================================
!                   ACTUALISATIONS ET ARCHIVAGE
! ======================================================================
!
    call uttcpu('CPU.OP0186.3', 'DEBUT', ' ')
    call copisd('CHAMP_GD', 'V', vhydrp(1:19), vhydr(1:19))
!
    if (niv .eq. 2) then
        write (ifm,*)
        write (ifm,*) '**************************************'
        write (ifm,*) ' THER_NON_LINE: OP00186'
        write (ifm,*)
        write (ifm,*) ' T+       :',vtemp
        write (ifm,*) ' T-       :',vtempp
        write (ifm,*)
    endif
!
! ======================================================================
! -- PREPARATION DES PARAMETRES ARCHIVES  ------------------------------
! ======================================================================
    if (conver) then
        call jeveuo(sdcrit(1:19)//'.CRTR', 'E', vr=crtr)
        crtr(1) = iterat
        crtr(2) = iterho
        crtr(3) = testr
        crtr(4) = testm
        crtr(5) = rho
    endif
!
    finpas = didern(sddisc, numins)
!
    call jeveuo(vtempm(1:19)//'.VALE', 'L', jtempp)
    call jeveuo(vtemp(1:19)//'.VALE', 'E', jtemp)
! VTEMPM --> VTEMP
    do k = 1, neq
        zr(jtemp+k-1) = zr(jtempp+k-1)
    end do
    call uttcpu('CPU.OP0186.3', 'FIN', ' ')
    call uttcpr('CPU.OP0186.3', 4, tps3)
!
! ------- ARCHIVAGE
!
    if (.not.l_evol) then
        force = .true.
    else
        force = .false.
    endif
    call ntarch(numins, model   , mate , cara_elem, para,&
                sddisc, ds_inout, force, sdcrit)
!
! - Make observation
!
    if (l_evol) then
        call ntobsv(mesh, sdobse, numins, instap)
    endif
!
! ------- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
    if (etausr() .eq. 1) then
        call sigusr()
    endif
!
! ------- TEMPS DISPONIBLE POUR CONTINUER ?
!
    call uttcpu('CPU.OP0186.1', 'FIN', ' ')
    call uttcpr('CPU.OP0186.1', 7, tps1)
    tconso = tps1(7) - tpex
    call impfot(tconso, tpscvt)
    call utmess('I', 'MECANONLINE7_1', sk=tpscvt)
    write (ifm,'(/)')
    tpex = tps1(7)
    if (tps1(4) .gt. 0.48d0*tps1(1)) then
        itab(1) = numins
        rtab(1) = tps2(4)
        rtab(2) = tps2(1)
        call utmess('Z', 'DISCRETISATION2_80', si=itab(1), nr=2, valr=rtab,&
                    num_except=28)
    endif
!
    if (finpas) goto 500
!
!----- NOUVEAU PAS DE TEMPS
    if (l_stat) then
        l_stat=.false.
    endif
    numins = numins + 1
    goto 200
!
!
500 continue
!
    call titre()
!
! --- DESTRUCTION DE TOUTES LES MATRICES CREEES
!
    call detmat()
!
    call jedema()
!
end subroutine
