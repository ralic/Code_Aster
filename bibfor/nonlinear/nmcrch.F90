subroutine nmcrch(numedd, fonact, sddyna, ds_contact, valinc,&
                  solalg, veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/vtcreb.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: numedd
    integer, intent(in) :: fonact(*)
    character(len=19), intent(in) :: sddyna
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: valinc(*)
    character(len=19), intent(in) :: solalg(*)
    character(len=19), intent(in) :: veasse(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - INITIALISATIONS)
!
! CREATION DES VECTEURS D'INCONNUS
!
! ----------------------------------------------------------------------
!
! IN  SDDYNA : SD DYNAMIQUE
! In  ds_contact       : datastructure for contact management
! IN  NUMEDD : NUME_DDL
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: sdcont_defi
    aster_logical :: ldyna, lammo, lmpas, lrefe, lmacr, lmuap, lviss
    aster_logical :: leltc, leltf
    aster_logical :: lunil, lctcd, lctfd, lpenac, lallv
    aster_logical :: lsstf, limpe
    aster_logical :: ldidi, lpilo, lener
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
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION VECTEURS INCONNUES'
    endif
!
! - Initializations
!
    sdcont_defi = ds_contact%sdcont_defi
!
! - Active functionnalities
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
        lpenac = cfdisl(sdcont_defi,'CONT_PENA' )
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
        call vtcreb(fexmoi, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(fammoi, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(flimoi, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(fnomoi, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(fexplu, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(famplu, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(fliplu, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(fnoplu, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- CREATION DES CHAMPS DE BASE - ETAT EN T-
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
    call vtcreb(depmoi, 'V', 'R', nume_ddlz = numedd)
    if (ldyna) then
        call vtcreb(vitmoi, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(accmoi, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- CREATION DES CHAMPS DE BASE - ETAT EN T+
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call vtcreb(depplu, 'V', 'R', nume_ddlz = numedd)
    if (ldyna) then
        call vtcreb(vitplu, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(accplu, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- CREATION DES CHAMPS DE BASE - POUTRES EN GRANDES ROTATIONS
!
    call nmchex(valinc, 'VALINC', 'DEPKM1', depkm1)
    call nmchex(valinc, 'VALINC', 'VITKM1', vitkm1)
    call nmchex(valinc, 'VALINC', 'ACCKM1', acckm1)
    call nmchex(valinc, 'VALINC', 'ROMKM1', romkm1)
    call nmchex(valinc, 'VALINC', 'ROMK  ', romk)
    call vtcreb(depkm1, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(vitkm1, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(acckm1, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(romkm1, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(romk, 'V', 'R', nume_ddlz = numedd)
!
! --- CREATION DES CHAMPS DE BASE - INCREMENTS SOLUTIONS
!
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DEPPR1', deppr1)
    call nmchex(solalg, 'SOLALG', 'DEPPR2', deppr2)
    call nmchex(solalg, 'SOLALG', 'DEPOLD', depold)
    call vtcreb(depdel, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(ddepla, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(depold, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(deppr1, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(deppr2, 'V', 'R', nume_ddlz = numedd)
    if (ldyna) then
        call nmchex(solalg, 'SOLALG', 'VITDEL', vitdel)
        call nmchex(solalg, 'SOLALG', 'DVITLA', dvitla)
        call nmchex(solalg, 'SOLALG', 'VITPR1', vitpr1)
        call nmchex(solalg, 'SOLALG', 'VITPR2', vitpr2)
        call nmchex(solalg, 'SOLALG', 'VITOLD', vitold)
        call vtcreb(vitdel, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(dvitla, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(vitold, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(vitpr1, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(vitpr2, 'V', 'R', nume_ddlz = numedd)
        call nmchex(solalg, 'SOLALG', 'ACCDEL', accdel)
        call nmchex(solalg, 'SOLALG', 'DACCLA', daccla)
        call nmchex(solalg, 'SOLALG', 'ACCPR1', accpr1)
        call nmchex(solalg, 'SOLALG', 'ACCPR2', accpr2)
        call nmchex(solalg, 'SOLALG', 'ACCOLD', accold)
        call vtcreb(accdel, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(daccla, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(accold, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(accpr1, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(accpr2, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- REACTIONS D'APPUI BT.LAMBDA
!
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call vtcreb(cndiri, 'V', 'R', nume_ddlz = numedd)
!
! --- VECTEURS SOLUTION
!
    call nmchex(solalg, 'SOLALG', 'DEPSO1', depso1)
    call nmchex(solalg, 'SOLALG', 'DEPSO2', depso2)
    call vtcreb(depso1, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(depso2, 'V', 'R', nume_ddlz = numedd)
!
! --- CREATION DES CHAMPS DE BASE - DEPL/VITE/ACCE D'ENTRAINEMENT
!
    if (ldyna) then
        call ndynkk(sddyna, 'DEPENT', depent)
        call ndynkk(sddyna, 'VITENT', vitent)
        call ndynkk(sddyna, 'ACCENT', accent)
        call vtcreb(depent, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(vitent, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(accent, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- CREATION DES CHAMPS DEPL/VITE/ACCE ABSOLUS POUR LE MULTI-APPUIS
!
    if (lmuap) then
        call ndynkk(sddyna, 'DEPABS', depabs)
        call ndynkk(sddyna, 'VITABS', vitabs)
        call ndynkk(sddyna, 'ACCABS', accabs)
        call vtcreb(depabs, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(vitabs, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(accabs, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- FORCES DE SOL
!
    if (lviss) then
        call nmchex(veasse, 'VEASSE', 'CNVISS', cnviss)
        call vtcreb(cnviss, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- FORCES D'IMPEDANCES (PREDICTION ET CORRECTION)
!
    if (limpe) then
        call nmchex(veasse, 'VEASSE', 'CNIMPP', cnimpp)
        call vtcreb(cnimpp, 'V', 'R', nume_ddlz = numedd)
        call nmchex(veasse, 'VEASSE', 'CNIMPC', cnimpc)
        call vtcreb(cnimpc, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- SECOND MEMBRE
!
    call nmchex(veasse, 'VEASSE', 'CNFEDO', cnfedo)
    call vtcreb(cnfedo, 'V', 'R', nume_ddlz = numedd)
    call nmchex(veasse, 'VEASSE', 'CNFSDO', cnfsdo)
    call vtcreb(cnfsdo, 'V', 'R', nume_ddlz = numedd)
    call nmchex(veasse, 'VEASSE', 'CNDIDO', cndido)
    if (ldidi) then
        call nmchex(veasse, 'VEASSE', 'CNDIDI', cndidi)
        call vtcreb(cndidi, 'V', 'R', nume_ddlz = numedd)
    endif
    if (lpilo) then
        call nmchex(veasse, 'VEASSE', 'CNFEPI', cnfepi)
        call vtcreb(cnfepi, 'V', 'R', nume_ddlz = numedd)
        call nmchex(veasse, 'VEASSE', 'CNDIPI', cndipi)
        call vtcreb(cndipi, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- PAS VRAIMENT DES VECT_ELEM MAIS DES CHAM_NO A CREER
!
    call nmchex(veasse, 'VEASSE', 'CNFEXT', cnfext)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veasse, 'VEASSE', 'CNVCF1', cnvcf1)
    call vtcreb(cnfext, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(cnfint, 'V', 'R', nume_ddlz = numedd)
    call vtcreb(cnvcf1, 'V', 'R', nume_ddlz = numedd)
!
    if (ldyna) then
        call nmchex(veasse, 'VEASSE', 'CNDYNA', cndyna)
        call vtcreb(cndyna, 'V', 'R', nume_ddlz = numedd)
        if (lmpas) then
            call ndynkk(sddyna, 'OLDP_CNFEDO', cnfedo)
            call vtcreb(cnfedo, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNFSDO', cnfsdo)
            call vtcreb(cnfsdo, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNDIDO', cndido)
            call vtcreb(cndido, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNDIDI', cndidi)
            call vtcreb(cndidi, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNFINT', cnfint)
            call vtcreb(cnfint, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNONDP', cnondp)
            call vtcreb(cnondp, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNLAPL', cnlapl)
            call vtcreb(cnlapl, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNSSTF', cnsstf)
            call vtcreb(cnsstf, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNCINE', cncine)
            call vtcreb(cncine, 'V', 'R', nume_ddlz = numedd)
            call ndynkk(sddyna, 'OLDP_CNVISS', cnviss)
            call vtcreb(cnviss, 'V', 'R', nume_ddlz = numedd)
        endif
    endif
!
! --- FORCES ISSUES DES MACRO-ELEMENTS STATIQUES
!
    if (lmacr) then
        call nmchex(veasse, 'VEASSE', 'CNSSTR', cnsstr)
        call vtcreb(cnsstr, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- CALCUL PAR SOUS-STRUCTURATION
!
    if (lsstf) then
        call nmchex(veasse, 'VEASSE', 'CNSSTF', cnsstf)
        call vtcreb(cnsstf, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- AMORTISSEMENT MODAL
!
    if (lammo) then
        call nmchex(veasse, 'VEASSE', 'CNMODP', cnmodp)
        call nmchex(veasse, 'VEASSE', 'CNMODC', cnmodc)
        call vtcreb(cnmodp, 'V', 'R', nume_ddlz = numedd)
        call vtcreb(cnmodc, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- CONTACT/FROTTEMENT DISCRET
!
    if (lctcd .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNCTDC', cnctdc)
        call vtcreb(cnctdc, 'V', 'R', nume_ddlz = numedd)
    endif
    if ((lctfd.or.lpenac) .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNCTDF', cnctdf)
        call vtcreb(cnctdf, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- LIAISON UNILATERALE
!
    if (lunil) then
        call nmchex(veasse, 'VEASSE', 'CNUNIL', cnunil)
        call vtcreb(cnunil, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- CONTACT AVEC DES ELEMENTS FINIS (CONTINUE/XFEM)
!
    if (leltc .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNELTC', cneltc)
        call vtcreb(cneltc, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- FROTTEMENT AVEC DES ELEMENTS FINIS (CONTINUE/XFEM)
!
    if (leltf .and. (.not.lallv)) then
        call nmchex(veasse, 'VEASSE', 'CNELTF', cneltf)
        call vtcreb(cneltf, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- RESIDU DE REFERENCE
!
    if (lrefe) then
        call nmchex(veasse, 'VEASSE', 'CNREFE', cnrefe)
        call vtcreb(cnrefe, 'V', 'R', nume_ddlz = numedd)
    endif
!
! --- CREATION DE CHAMPS NODAUX PARTAGES (PASSES EN SOUTERRAIN)
!      OBJECTIFS :
!         NE PAS FRAGMENTER LA MEMOIRE
!      REGLES :
!         CNZERO : LECTURE SEULE -> IL VAUT TJRS 0
!         CNTMPX : NE TRANSITENT PAS D'UNE ROUTINE A L'AUTRE
!
    call vtcreb('&&CNPART.ZERO', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNPART.CHP1', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNPART.CHP2', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNPART.CHP3', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNREPL.CHP1', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNREPL.CHP2', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNREPL.CHP3', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNREPL.CHP4', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCETA.CHP0', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCETA.CHP1', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCETA.CHP2', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.FFDO', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.FFPI', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.DFDO', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.DFPI', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.FVDO', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.FVDY', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.DUMM', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.CINE', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.DONN', 'V', 'R', nume_ddlz = numedd)
    call vtcreb('&&CNCHAR.PILO', 'V', 'R', nume_ddlz = numedd)
!
    call jedema()
end subroutine
