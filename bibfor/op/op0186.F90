subroutine op0186()
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: Christophe-mmn.durand at edf.fr
! ----------------------------------------------------------------------
!
!     COMMANDE:  THER_NON_LINE
!
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1501
    implicit none
!
! 0.2  ==> ARGUMENTS
!
! 0.3. ==> VARIABLES LOCALES
    include 'jeveux.h'
    include 'asterc/etausr.h'
    include 'asterc/getres.h'
    include 'asterc/gettco.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detmat.h'
    include 'asterfort/didern.h'
    include 'asterfort/diinst.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/impfot.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/medith.h'
    include 'asterfort/ntarch.h'
    include 'asterfort/ntobsv.h'
    include 'asterfort/nxacmv.h'
    include 'asterfort/nxinit.h'
    include 'asterfort/nxlect.h'
    include 'asterfort/nxnewt.h'
    include 'asterfort/nxpred.h'
    include 'asterfort/nxrech.h'
    include 'asterfort/rsinch.h'
    include 'asterfort/sigusr.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utexcm.h'
    include 'asterfort/uttcpr.h'
    include 'asterfort/uttcpu.h'
    logical :: lostat, matcst, coecst, reasma, arret, conver, itemax, reasvc
    logical :: reasvt, reasmt, reasrg, reasms, lsecha, rechli, finpas, levol
    logical :: force, lnonl
    integer :: parmei(2), parcri(3), numins, k, ierd, icoret, nbcham, iterho
    integer :: itmax, ifm, niv, neq, iterat, jtempp, jtempm, jtemp, jcrr
    integer :: itab(2)
    real(kind=8) :: parmer(2), tpsthe(6), deltat, timet, timtdt, tps1(4)
    real(kind=8) :: tps2(4), tps3(4), tpex, parcrr(2), theta, khi, rho, testr
    real(kind=8) :: testm, para(2), instap, tconso
    real(kind=8) :: rtab(2)
    character(len=1) :: creas, base
    character(len=3) :: kreas
    character(len=8) :: evolsc, mailla
    character(len=8) :: k8bid
    character(len=19) :: sdobse
    character(len=16) :: tysd, k16b1, k16b2
    character(len=19) :: lischa, lisch2
    character(len=19) :: solveu, maprec, sddisc, sdcrit
    character(len=24) :: modele, mate, carele, fomult, charge, infoch, result
    character(len=24) :: time, tmpchi, tmpchf, compor, vtemp, vtempm, vtempp
    character(len=24) :: vtempr, vec2nd, vec2ni, numedd, mediri, matass, cndirp
    character(len=24) :: cnchci, cnresi, vabtla, vhydr, vhydrp
    character(len=24) :: sdieto, tpscvt
    character(len=76) :: fmt2, fmt3, fmt4
    character(len=85) :: fmt1
    character(len=8) :: k8b
!
! ----------------------------------------------------------------------
    data lischa/'&&OP0186.LISCHA'/
    data sdcrit/'&&OP0186.CRITERE'/
    data solveu/'&&OP0186.SOLVEUR'/
    data maprec/'&&OP0186.MAPREC'/
    data result/' '/
    data cndirp/1*' '/
    data cnchci/1*' '/
    data vec2nd/'&&OP0186.2ND'/
    data vec2ni/'&&OP0186.2NI'/
    data tmpchi,tmpchf/'&&OP0186.TCHI','&&OP0186.TCHF'/
    data vhydr,vhydrp/'&&OP0186.HY','&&OP0186.HYP'/
    data mediri/' '/
    data matass/'&&MTHASS'/
    data fmt1/'(85(''-''))'/
    data fmt2/'(A,1X,A,6X,A,9X,A,6X,A,3X,A,3X,A,1X,A)'/
    data fmt3/'(A,16X,A,8X,A,6X,A,3X,A,6X,A,4X,A)'/
    data fmt4/'(A,12X,A,2X,A,17X,A,9X,A,4X,A)'/
    data sddisc            /'&&OP0186.PARTPS'/
    data sdobse            /'&&OP0186.OBSER'/
    data sdieto            /'&&OP0186.SDIETO'/
! ----------------------------------------------------------------------
!
!     MESURE DE TEMPS CPU :
!
!      1 : PAS DE TEMPS
!      2 : ITERATIONS
!      3 : ACTUALISATIONS ET ARCHIVAGE
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! **********************************************************************
!                    RECUPERATION DES OPERANDES
! **********************************************************************
!
!--- RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
!
! --- LECTURE DES OPERANDES DE LA COMMANDE
!
    call nxlect(result, modele, mate, carele, matcst,&
                coecst, fomult, lischa, charge, infoch,&
                parmei, parmer, solveu, parcri, parcrr,&
                compor, evolsc)
    para(1) = parmer(1)
    itmax = parcri(3)
    rechli = .false.
!
! EST-ON DANS UN CALCUL DE SECHAGE ?
    if (evolsc(1:1) .ne. ' ') then
        lsecha = .true.
    else
        lsecha = .false.
    endif
! --- CE BOOLEEN ARRET EST DESTINE AUX DEVELOPPEUR QUI VOUDRAIENT
! --- FORCER LE CALCUL MEME SI ON N'A PAS CONVERGENCE (ARRET=TRUE)
    arret = .false.
    if (parmei(2) .gt. 0) rechli = .true.
!
! **********************************************************************
!    INITIALISATIONS ET DUPLICATION DES STRUCTURES DE DONNEES
! **********************************************************************
!
! --- INITIALISATIONS
!
    call nxinit(result, modele, mate, carele, compor,&
                lischa, lisch2, solveu, para, numedd,&
                lostat, levol, lnonl, sddisc, sdieto,&
                vhydr, sdobse, mailla, sdcrit, time)
    call assert(lnonl)
!
    if (lostat) then
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
    call copisd('CHAMP_GD', 'V', vtemp(1:19), vtempm(1:19))
    call copisd('CHAMP_GD', 'V', vtemp(1:19), vtempp(1:19))
    call copisd('CHAMP_GD', 'V', vtemp(1:19), vtempr(1:19))
    call copisd('CHAMP_GD', 'V', vtemp(1:19), vec2nd(1:19))
    call copisd('CHAMP_GD', 'V', vtemp(1:19), vec2ni(1:19))
    call copisd('CHAMP_GD', 'V', vhydr(1:19), vhydrp(1:19))
!
! --- CALCUL DES MATRICES ELEMENTAIRES DES DIRICHLETS
    call medith(modele, charge, infoch, mediri)
!
! **********************************************************************
!                 BOUCLE SUR LES PAS DE TEMPS
! **********************************************************************
!
    call uttcpu('CPU.OP0186.1', 'INIT', ' ')
    call uttcpr('CPU.OP0186.1', 4, tps1)
    tpex = tps1(3)
    call uttcpu('CPU.OP0186.2', 'INIT', ' ')
    call uttcpu('CPU.OP0186.3', 'INIT', ' ')
    call uttcpr('CPU.OP0186.3', 4, tps3)
    reasrg = .false.
    reasms = .false.
200  continue
! --- RECUPERATION DU PAS DE TEMPS ET DES PARAMETRES DE RESOLUTION
!
    if (lostat) then
        if (.not.levol) then
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
        theta=parmer(1)
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
    call u2mesg('I', 'MECANONLINE6_6', 0, ' ', 0,&
                0, 1, instap)
    write (ifm,fmt1)
    write (ifm,fmt2) '|','ITERATION','RESIDU','RESIDU',&
     &      'ITERATION','COEFFICIENT','ACTUALISATION','|'
    write (ifm,fmt3) '|','RELATIF','ABSOLU','RECH. LIN.',&
     &      'RECH. LIN.','MATRICE','|'
    write (ifm,fmt4) '|','RESI_GLOB_RELA','RESI_GLOB_MAXI','RHO',&
     &      'TANGENTE','|'
    write (ifm,fmt1)
    call jelira(vtempm(1:19)//'.VALE', 'LONMAX', neq, k8bid)
!
! RECUPERATION DE:
! RESULT --> NOM DE LA SD RESULTAT
! VTEMP  --> T+,I+1BIS
! VTEMPP --> T-
    call getres(result, k16b1, k16b2)
    vtemp='&&NXLECTVAR_____'
!
! --- RECUPERATION DU CHAMP DE TEMPERATURE A T ET T+DT POUR LE SECHAGE
!     LOIS SECH_GRANGER ET SECH_NAPPE
    if (lsecha) then
        call gettco(evolsc, tysd)
        if (tysd(1:9) .eq. 'EVOL_THER') then
            call dismoi('F', 'NB_CHAMP_UTI', evolsc, 'RESULTAT', nbcham,&
                        k8bid, ierd)
            if (nbcham .gt. 0) then
                timet = instap
                timtdt = instap + deltat
                base = 'V'
                call rsinch(evolsc, 'TEMP', 'INST', timet, tmpchi,&
                            'CONSTANT', 'CONSTANT', 1, base, icoret)
                if (icoret .ge. 10) then
                    call u2mesg('F', 'ALGORITH8_94', 1, evolsc, 1,&
                                icoret, 1, timet)
                endif
                call rsinch(evolsc, 'TEMP', 'INST', timtdt, tmpchf,&
                            'CONSTANT', 'CONSTANT', 1, base, icoret)
                if (icoret .ge. 10) then
                    call u2mesg('F', 'ALGORITH8_94', 1, evolsc, 1,&
                                icoret, 1, timtdt)
                endif
            else
                call u2mesk('F', 'ALGORITH8_99', 1, evolsc)
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
    call nxacmv(modele, mate, carele, fomult, charge,&
                lischa, infoch, numedd, solveu, lostat,&
                time, tpsthe, reasvc, reasvt, reasmt,&
                reasrg, reasms, creas, vtemp, vhydr,&
                tmpchi, tmpchf, vec2nd, vec2ni, matass,&
                maprec, cndirp, cnchci, mediri, compor)
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
    call nxpred(modele, mate, carele, charge, infoch,&
                numedd, solveu, lostat, time, neq,&
                matass, maprec, vtemp, vtempm, vtempp,&
                vhydr, vhydrp, tmpchi, tmpchf, compor,&
                cndirp, cnchci, vec2nd, vec2ni)
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
20  continue
!
! --- DOIT ON REACTUALISER LA MATRICE TANGENTE
!
    call uttcpu('CPU.OP0186.2', 'DEBUT', ' ')
    iterat = iterat + 1
    reasma = .false.
    kreas = 'NON'
    if (iterat .ge. itmax) itemax = .true.
    if ((parmei(1).ne.0)) then
        if (mod(iterat,parmei(1)) .eq. 0) then
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
    call nxnewt(modele, mate, carele, charge, lischa,&
                infoch, numedd, solveu, time, neq,&
                matass, maprec, cnchci, vtemp, vtempm,&
                vtempp, vec2nd, mediri, conver, vhydr,&
                vhydrp, tmpchi, tmpchf, compor, vabtla,&
                cnresi, parcri, parcrr, reasma, testr,&
                testm)
!
! --- SI NON CONVERGENCE ALORS RECHERCHE LINEAIRE
!       (CALCUL DE RHO) SUR L INCREMENT VTEMPP
! --- ACTUALISATION DE LA TEMPERATURE VTEMPM AVEC L INCREMENT VTEMPP
!     MULTIPLIE PAR RHO
    rho = 0.d0
    iterho = 0
    if (.not.conver) then
        if (rechli) then
!
! ON CALCULE LE RHO/ VTEMPR = T+,I+1BIS = T+,1 + RHO * (T+,I+1 - T+,I)
! MINIMISE VEC2ND - RESI_THER(T+,I+1BIS) - (BT)*LAGRANGE
            call nxrech(modele, mate, carele, charge, infoch,&
                        numedd, time, neq, compor, vtempm,&
                        vtempp, vtempr, vtemp, vhydr, vhydrp,&
                        tmpchi, tmpchf, vec2nd, vabtla, cnresi,&
                        rho, iterho, parmer, parmei)
        else
            rho = 1.d0
        endif
        call jeveuo(vtempp(1:19)//'.VALE', 'L', jtempp)
        call jeveuo(vtempm(1:19)//'.VALE', 'E', jtempm)
        call jeveuo(vtemp(1:19)//'.VALE', 'L', jtemp)
!
! SOLUTION: VTEMPM = VTEMPR = T+,I+1BIS
        do 30 k = 1, neq
            zr(jtempm+k-1) = zr(jtempm+k-1) + rho*zr(jtempp+k-1)
30      continue
    endif
!
    write (ifm,&
     &      '(A,1X,I5,6X,1PE12.5,4X,1PE12.5,7X,I2,5X,1PE12.5,8X,A,6X,A)'&
     &        ) '|',iterat,testr,testm,iterho,rho,kreas,'|'
!
    if (itemax .and. .not.conver) then
        write (ifm,fmt1)
        call u2mess('I', 'MECANONLINE10_3')
    endif
    call uttcpu('CPU.OP0186.2', 'FIN', ' ')
    call uttcpr('CPU.OP0186.2', 4, tps2)
    if ((.not.conver) .and. (.not.itemax)) then
        if (2.d0*tps2(4) .gt. 0.95d0*tps2(1)-tps3(4)) then
            write (ifm,fmt1)
            itab(1) = numins
            rtab(1) = tps2(4)
            rtab(2) = tps2(1)
            call utexcm(28, 'DISCRETISATION2_79', 0, k8b, 1,&
                        itab, 2, rtab)
        else
            goto 20
        endif
    else if ((.not.conver) .and. itemax .and. (.not.arret)) then
        write (ifm,fmt1)
        itab(1) = numins
        itab(2) = iterat
        call utexcm(22, 'THERNONLINE4_85', 0, k8b, 2,&
                    itab, 0, rtab)
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
        call jeveuo(sdcrit(1:19)//'.CRTR', 'E', jcrr)
        zr(jcrr+0) = iterat
        zr(jcrr+1) = iterho
        zr(jcrr+2) = testr
        zr(jcrr+3) = testm
        zr(jcrr+4) = rho
    endif
!
    finpas = didern(sddisc, numins)
!
    call jeveuo(vtempm(1:19)//'.VALE', 'L', jtempp)
    call jeveuo(vtemp(1:19)//'.VALE', 'E', jtemp)
! VTEMPM --> VTEMP
    do 145 k = 1, neq
        zr(jtemp+k-1) = zr(jtempp+k-1)
145  continue
    call uttcpu('CPU.OP0186.3', 'FIN', ' ')
    call uttcpr('CPU.OP0186.3', 4, tps3)
!
! ------- ARCHIVAGE
!
    if (.not.levol) then
        force = .true.
    else
        force = .false.
    endif
    call ntarch(numins, modele, mate, carele, lnonl,&
                para, sddisc, sdcrit, sdieto, lisch2,&
                force)
!
! ------- OBSERVATION EVENTUELLE
!
    if (levol) then
        call ntobsv(mailla, sdieto, sdobse, numins, instap)
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
    call uttcpr('CPU.OP0186.1', 4, tps1)
    tconso=tps1(3) - tpex
    call impfot(tconso, tpscvt)
    call u2mesk('I', 'MECANONLINE7_1', 1, tpscvt)
    write (ifm,'(/)')
    tpex = tps1(3)
    if (tps1(4) .gt. 0.48d0*tps1(1)) then
        itab(1) = numins
        rtab(1) = tps2(4)
        rtab(2) = tps2(1)
        call utexcm(28, 'DISCRETISATION2_80', 0, k8bid, 1,&
                    itab, 2, rtab)
    endif
!
    if (finpas) goto 500
!
!----- NOUVEAU PAS DE TEMPS
    if (lostat) then
        lostat=.false.
    endif
    numins = numins + 1
    goto 200
!
!
500  continue
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
