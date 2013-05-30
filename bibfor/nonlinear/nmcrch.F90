subroutine nmcrch(numedd, fonact, sddyna, defico, valinc,&
                  solalg, veasse)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/vtcreb.h'
    integer :: fonact(*)
    character(len=19) :: sddyna
    character(len=24) :: numedd, defico
    character(len=19) :: solalg(*), veasse(*)
    character(len=19) :: valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - INITIALISATIONS)
!
! CREATION DES VECTEURS D'INCONNUS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  DEFICO : DEFINITION CONTACT
! IN  NUMEDD : NUME_DDL
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    logical :: ldyna, lammo, lmpas, lrefe, lmacr, lmuap, lviss
    logical :: leltc, leltf
    logical :: lunil, lctcd, lctfd, lpenac, lallv
    logical :: lsstf, limpe
    logical :: ldidi, lpilo, lener
    integer :: neq
    character(len=19) :: depplu, vitplu, accplu
    character(len=19) :: depmoi, vitmoi, accmoi
    character(len=19) :: fexmoi, fammoi, flimoi, fnomoi
    character(len=19) :: fexplu, famplu, fliplu, fnoplu
    character(len=19) :: depso1, depso2
    character(len=19) :: depdel, depold, ddepla, deppr1, deppr2
    character(len=19) :: vitdel, vitold, dvitla, vitpr1, vitpr2
    character(len=19) :: accdel, accold, daccla, accpr1, accpr2
    character(len=19) :: depkm1, vitkm1, acckm1, romkm1, romk
    character(len=19) :: depent, vitent, accent
    character(len=19) :: depabs, vitabs, accabs
    character(len=19) :: cndyna, cnmodp, cnmodc
    character(len=19) :: cnfext, cnvcf1
    character(len=19) :: cnfedo, cnfsdo, cndidi, cnfint
    character(len=19) :: cndido, cncine, cndiri
    character(len=19) :: cnondp, cnlapl, cnviss
    character(len=19) :: cnsstf, cnsstr
    character(len=19) :: cnctdc, cnctdf, cnunil
    character(len=19) :: cneltc, cneltf
    character(len=19) :: cnimpp, cnimpc
    character(len=19) :: cnfepi, cndipi, cnrefe
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION VECTEURS INCONNUES'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lmacr = isfonc(fonact,'MACR_ELEM_STAT')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lammo = ndynlo(sddyna,'AMOR_MODAL')
    lmpas = ndynlo(sddyna,'MULTI_PAS')
    lctcd = isfonc(fonact,'CONT_DISCRET')
    lctfd = isfonc(fonact,'FROT_DISCRET')
    lallv = isfonc(fonact,'CONT_ALL_VERIF')
    lunil = isfonc(fonact,'LIAISON_UNILATER')
    ldidi = isfonc(fonact,'DIDI')
    lpilo = isfonc(fonact,'PILOTAGE')
    lsstf = isfonc(fonact,'SOUS_STRUC')
    lrefe = isfonc(fonact,'RESI_REFE')
    limpe = ndynlo(sddyna,'IMPE_ABSO')
    lviss = ndynlo(sddyna,'VECT_ISS' )
    lener = isfonc(fonact,'ENERGIE')
    lmuap = ndynlo(sddyna,'MULTI_APPUI')
    if (lctcd) then
        lpenac = cfdisl(defico,'CONT_PENA' )
    else
        lpenac = .false.
    endif
    leltc = isfonc(fonact,'ELT_CONTACT')
    leltf = isfonc(fonact,'ELT_FROTTEMENT')
!
! --- ENERGIE
!
    if (lener) then
        call nmchex(valinc, 'VALINC', 'FEXMOI', fexmoi)
        call nmchex(valinc, 'VALINC', 'FAMMOI', fammoi)
        call nmchex(valinc, 'VALINC', 'FLIMOI', flimoi)
        call nmchex(valinc, 'VALINC', 'FNOMOI', fnomoi)
        call nmchex(valinc, 'VALINC', 'FEXPLU', fexplu)
        call nmchex(valinc, 'VALINC', 'FAMPLU', famplu)
        call nmchex(valinc, 'VALINC', 'FLIPLU', fliplu)
        call nmchex(valinc, 'VALINC', 'FNOPLU', fnoplu)
        call vtcreb(fexmoi, numedd, 'V', 'R', neq)
        call vtcreb(fammoi, numedd, 'V', 'R', neq)
        call vtcreb(flimoi, numedd, 'V', 'R', neq)
        call vtcreb(fnomoi, numedd, 'V', 'R', neq)
        call vtcreb(fexplu, numedd, 'V', 'R', neq)
        call vtcreb(famplu, numedd, 'V', 'R', neq)
        call vtcreb(fliplu, numedd, 'V', 'R', neq)
        call vtcreb(fnoplu, numedd, 'V', 'R', neq)
    endif
!
! --- CREATION DES CHAMPS DE BASE - ETAT EN T-
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
    call vtcreb(depmoi, numedd, 'V', 'R', neq)
    if (ldyna) then
        call vtcreb(vitmoi, numedd, 'V', 'R', neq)
        call vtcreb(accmoi, numedd, 'V', 'R', neq)
    endif
!
! --- CREATION DES CHAMPS DE BASE - ETAT EN T+
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call vtcreb(depplu, numedd, 'V', 'R', neq)
    if (ldyna) then
        call vtcreb(vitplu, numedd, 'V', 'R', neq)
        call vtcreb(accplu, numedd, 'V', 'R', neq)
    endif
!
! --- CREATION DES CHAMPS DE BASE - POUTRES EN GRANDES ROTATIONS
!
    call nmchex(valinc, 'VALINC', 'DEPKM1', depkm1)
    call nmchex(valinc, 'VALINC', 'VITKM1', vitkm1)
    call nmchex(valinc, 'VALINC', 'ACCKM1', acckm1)
    call nmchex(valinc, 'VALINC', 'ROMKM1', romkm1)
    call nmchex(valinc, 'VALINC', 'ROMK  ', romk)
    call vtcreb(depkm1, numedd, 'V', 'R', neq)
    call vtcreb(vitkm1, numedd, 'V', 'R', neq)
    call vtcreb(acckm1, numedd, 'V', 'R', neq)
    call vtcreb(romkm1, numedd, 'V', 'R', neq)
    call vtcreb(romk, numedd, 'V', 'R', neq)
!
! --- CREATION DES CHAMPS DE BASE - INCREMENTS SOLUTIONS
!
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DEPPR1', deppr1)
    call nmchex(solalg, 'SOLALG', 'DEPPR2', deppr2)
    call nmchex(solalg, 'SOLALG', 'DEPOLD', depold)
    call vtcreb(depdel, numedd, 'V', 'R', neq)
    call vtcreb(ddepla, numedd, 'V', 'R', neq)
    call vtcreb(depold, numedd, 'V', 'R', neq)
    call vtcreb(deppr1, numedd, 'V', 'R', neq)
    call vtcreb(deppr2, numedd, 'V', 'R', neq)
    if (ldyna) then
        call nmchex(solalg, 'SOLALG', 'VITDEL', vitdel)
        call nmchex(solalg, 'SOLALG', 'DVITLA', dvitla)
        call nmchex(solalg, 'SOLALG', 'VITPR1', vitpr1)
        call nmchex(solalg, 'SOLALG', 'VITPR2', vitpr2)
        call nmchex(solalg, 'SOLALG', 'VITOLD', vitold)
        call vtcreb(vitdel, numedd, 'V', 'R', neq)
        call vtcreb(dvitla, numedd, 'V', 'R', neq)
        call vtcreb(vitold, numedd, 'V', 'R', neq)
        call vtcreb(vitpr1, numedd, 'V', 'R', neq)
        call vtcreb(vitpr2, numedd, 'V', 'R', neq)
        call nmchex(solalg, 'SOLALG', 'ACCDEL', accdel)
        call nmchex(solalg, 'SOLALG', 'DACCLA', daccla)
        call nmchex(solalg, 'SOLALG', 'ACCPR1', accpr1)
        call nmchex(solalg, 'SOLALG', 'ACCPR2', accpr2)
        call nmchex(solalg, 'SOLALG', 'ACCOLD', accold)
        call vtcreb(accdel, numedd, 'V', 'R', neq)
        call vtcreb(daccla, numedd, 'V', 'R', neq)
        call vtcreb(accold, numedd, 'V', 'R', neq)
        call vtcreb(accpr1, numedd, 'V', 'R', neq)
        call vtcreb(accpr2, numedd, 'V', 'R', neq)
    endif
!
! --- REACTIONS D'APPUI BT.LAMBDA
!
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call vtcreb(cndiri, numedd, 'V', 'R', neq)
!
! --- VECTEURS SOLUTION
!
    call nmchex(solalg, 'SOLALG', 'DEPSO1', depso1)
    call nmchex(solalg, 'SOLALG', 'DEPSO2', depso2)
    call vtcreb(depso1, numedd, 'V', 'R', neq)
    call vtcreb(depso2, numedd, 'V', 'R', neq)
!
! --- CREATION DES CHAMPS DE BASE - DEPL/VITE/ACCE D'ENTRAINEMENT
!
    if (ldyna) then
        call ndynkk(sddyna, 'DEPENT', depent)
        call ndynkk(sddyna, 'VITENT', vitent)
        call ndynkk(sddyna, 'ACCENT', accent)
        call vtcreb(depent, numedd, 'V', 'R', neq)
        call vtcreb(vitent, numedd, 'V', 'R', neq)
        call vtcreb(accent, numedd, 'V', 'R', neq)
    endif
!
! --- CREATION DES CHAMPS DEPL/VITE/ACCE ABSOLUS POUR LE MULTI-APPUIS
!
    if (lmuap) then
        call ndynkk(sddyna, 'DEPABS', depabs)
        call ndynkk(sddyna, 'VITABS', vitabs)
        call ndynkk(sddyna, 'ACCABS', accabs)
        call vtcreb(depabs, numedd, 'V', 'R', neq)
        call vtcreb(vitabs, numedd, 'V', 'R', neq)
        call vtcreb(accabs, numedd, 'V', 'R', neq)
    endif
!
! --- FORCES DE SOL
!
    if (lviss) then
        call nmchex(veasse, 'VEASSE', 'CNVISS', cnviss)
        call vtcreb(cnviss, numedd, 'V', 'R', neq)
    endif
!
! --- FORCES D'IMPEDANCES (PREDICTION ET CORRECTION)
!
    if (limpe) then
        call nmchex(veasse, 'VEASSE', 'CNIMPP', cnimpp)
        call vtcreb(cnimpp, numedd, 'V', 'R', neq)
        call nmchex(veasse, 'VEASSE', 'CNIMPC', cnimpc)
        call vtcreb(cnimpc, numedd, 'V', 'R', neq)
    endif
!
! --- SECOND MEMBRE
!
    call nmchex(veasse, 'VEASSE', 'CNFEDO', cnfedo)
    call vtcreb(cnfedo, numedd, 'V', 'R', neq)
    call nmchex(veasse, 'VEASSE', 'CNFSDO', cnfsdo)
    call vtcreb(cnfsdo, numedd, 'V', 'R', neq)
    call nmchex(veasse, 'VEASSE', 'CNDIDO', cndido)
    if (ldidi) then
        call nmchex(veasse, 'VEASSE', 'CNDIDI', cndidi)
        call vtcreb(cndidi, numedd, 'V', 'R', neq)
    endif
    if (lpilo) then
        call nmchex(veasse, 'VEASSE', 'CNFEPI', cnfepi)
        call vtcreb(cnfepi, numedd, 'V', 'R', neq)
        call nmchex(veasse, 'VEASSE', 'CNDIPI', cndipi)
        call vtcreb(cndipi, numedd, 'V', 'R', neq)
    endif
!
! --- PAS VRAIMENT DES VECT_ELEM MAIS DES CHAM_NO A CREER
!
    call nmchex(veasse, 'VEASSE', 'CNFEXT', cnfext)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veasse, 'VEASSE', 'CNVCF1', cnvcf1)
    call vtcreb(cnfext, numedd, 'V', 'R', neq)
    call vtcreb(cnfint, numedd, 'V', 'R', neq)
    call vtcreb(cnvcf1, numedd, 'V', 'R', neq)
!
    if (ldyna) then
        call nmchex(veasse, 'VEASSE', 'CNDYNA', cndyna)
        call vtcreb(cndyna, numedd, 'V', 'R', neq)
        if (lmpas) then
            call ndynkk(sddyna, 'OLDP_CNFEDO', cnfedo)
            call vtcreb(cnfedo, numedd, 'V', 'R', neq)
            call ndynkk(sddyna, 'OLDP_CNFSDO', cnfsdo)
            call vtcreb(cnfsdo, numedd, 'V', 'R', neq)
            call ndynkk(sddyna, 'OLDP_CNDIDO', cndido)
            call vtcreb(cndido, numedd, 'V', 'R', neq)
            call ndynkk(sddyna, 'OLDP_CNDIDI', cndidi)
            call vtcreb(cndidi, numedd, 'V', 'R', neq)
            call ndynkk(sddyna, 'OLDP_CNFINT', cnfint)
            call vtcreb(cnfint, numedd, 'V', 'R', neq)
            call ndynkk(sddyna, 'OLDP_CNONDP', cnondp)
            call vtcreb(cnondp, numedd, 'V', 'R', neq)
            call ndynkk(sddyna, 'OLDP_CNLAPL', cnlapl)
            call vtcreb(cnlapl, numedd, 'V', 'R', neq)
            call ndynkk(sddyna, 'OLDP_CNSSTF', cnsstf)
            call vtcreb(cnsstf, numedd, 'V', 'R', neq)
            call ndynkk(sddyna, 'OLDP_CNCINE', cncine)
            call vtcreb(cncine, numedd, 'V', 'R', neq)
        endif
    endif
!
! --- FORCES ISSUES DES MACRO-ELEMENTS STATIQUES
!
    if (lmacr) then
        call nmchex(veasse, 'VEASSE', 'CNSSTR', cnsstr)
        call vtcreb(cnsstr, numedd, 'V', 'R', neq)
    endif
!
! --- CALCUL PAR SOUS-STRUCTURATION
!
    if (lsstf) then
        call nmchex(veasse, 'VEASSE', 'CNSSTF', cnsstf)
        call vtcreb(cnsstf, numedd, 'V', 'R', neq)
    endif
!
! --- AMORTISSEMENT MODAL
!
    if (lammo) then
        call nmchex(veasse, 'VEASSE', 'CNMODP', cnmodp)
        call nmchex(veasse, 'VEASSE', 'CNMODC', cnmodc)
        call vtcreb(cnmodp, numedd, 'V', 'R', neq)
        call vtcreb(cnmodc, numedd, 'V', 'R', neq)
    endif
!
! --- CONTACT/FROTTEMENT DISCRET
!
    if (lctcd .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNCTDC', cnctdc)
        call vtcreb(cnctdc, numedd, 'V', 'R', neq)
    endif
    if ((lctfd.or.lpenac) .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNCTDF', cnctdf)
        call vtcreb(cnctdf, numedd, 'V', 'R', neq)
    endif
!
! --- LIAISON UNILATERALE
!
    if (lunil) then
        call nmchex(veasse, 'VEASSE', 'CNUNIL', cnunil)
        call vtcreb(cnunil, numedd, 'V', 'R', neq)
    endif
!
! --- CONTACT AVEC DES ELEMENTS FINIS (CONTINUE/XFEM)
!
    if (leltc .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNELTC', cneltc)
        call vtcreb(cneltc, numedd, 'V', 'R', neq)
    endif
!
! --- FROTTEMENT AVEC DES ELEMENTS FINIS (CONTINUE/XFEM)
!
    if (leltf .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNELTF', cneltf)
        call vtcreb(cneltf, numedd, 'V', 'R', neq)
    endif
!
! --- RESIDU DE REFERENCE
!
    if (lrefe) then
        call nmchex(veasse, 'VEASSE', 'CNREFE', cnrefe)
        call vtcreb(cnrefe, numedd, 'V', 'R', neq)
    endif
!
! --- CREATION DE CHAMPS NODAUX PARTAGES (PASSES EN SOUTERRAIN)
!      OBJECTIFS :
!         NE PAS FRAGMENTER LA MEMOIRE
!      REGLES :
!         CNZERO : LECTURE SEULE -> IL VAUT TJRS 0
!         CNTMPX : NE TRANSITENT PAS D'UNE ROUTINE A L'AUTRE
!
    call vtcreb('&&CNPART.ZERO', numedd, 'V', 'R', neq)
    call vtcreb('&&CNPART.CHP1', numedd, 'V', 'R', neq)
    call vtcreb('&&CNPART.CHP2', numedd, 'V', 'R', neq)
    call vtcreb('&&CNPART.CHP3', numedd, 'V', 'R', neq)
    call vtcreb('&&CNREPL.CHP1', numedd, 'V', 'R', neq)
    call vtcreb('&&CNREPL.CHP2', numedd, 'V', 'R', neq)
    call vtcreb('&&CNREPL.CHP3', numedd, 'V', 'R', neq)
    call vtcreb('&&CNREPL.CHP4', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCETA.CHP0', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCETA.CHP1', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCETA.CHP2', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.FFDO', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.FFPI', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.DFDO', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.DFPI', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.FVDO', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.FVDY', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.DUMM', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.CINE', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.DONN', numedd, 'V', 'R', neq)
    call vtcreb('&&CNCHAR.PILO', numedd, 'V', 'R', neq)
!
    call jedema()
end subroutine
