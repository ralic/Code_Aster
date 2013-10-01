subroutine nmimac(sdimpr, sdsuiv, fonact)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/impfoi.h"
#include "asterfort/infniv.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/obgetb.h"
#include "asterfort/obgeti.h"
#include "asterfort/obgeto.h"
#include "asterfort/oblgoi.h"
#include "asterfort/oblraz.h"
#include "asterfort/oblsap.h"
#include "asterfort/obsetb.h"
#include "asterfort/obtcla.h"
    character(len=24) :: sdimpr, sdsuiv
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (IMPRESSION - INITIALISATIONS)
!
! ACTIVATION DES COLONNES POUR LE PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDSUIV : SD SUIVI_DDL
! IN  FONACT : FONCTIONNALITES ACTIVEES
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    logical :: lreli, lpilo, lctcd, lctcc
    logical :: lborst, lrefe, lcomp
    logical :: lboucc, lboucf, lboucg, lallv, lnewtf, lnewtc, lnewtg
    character(len=24) :: sdtabc, slcolo, sdcolo
    integer :: icolo, nbcolo
    character(len=24) :: suiinf
    integer :: jsuiin
    logical :: linfre, linftp, lcsv
    integer :: isuiv, nbsuiv
    character(len=1) :: indsui
    character(len=9) :: typcol
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- FONCTIONNALITES ACTIVEES
!
    lreli = isfonc(fonact,'RECH_LINE')
    lpilo = isfonc(fonact,'PILOTAGE')
    lctcd = isfonc(fonact,'CONT_DISCRET')
    lctcc = isfonc(fonact,'CONT_CONTINU')
    lrefe = isfonc(fonact,'RESI_REFE')
    lborst = isfonc(fonact,'DEBORST')
    lboucf = isfonc(fonact,'BOUCLE_EXT_FROT')
    lboucg = isfonc(fonact,'BOUCLE_EXT_GEOM')
    lboucc = isfonc(fonact,'BOUCLE_EXT_CONT')
    lnewtf = isfonc(fonact,'FROT_NEWTON')
    lnewtc = isfonc(fonact,'CONT_NEWTON')
    lnewtg = isfonc(fonact,'GEOM_NEWTON')
    lcomp = isfonc(fonact,'RESI_COMP')
    lallv = isfonc(fonact,'CONT_ALL_VERIF')
!
! --- TABLEAU DE CONVERGENCE ET OPTIONS
!
    call obgeto(sdimpr, 'TABLEAU_CONV', sdtabc)
    call obgetb(sdimpr, 'INFO_RESIDU', linfre)
    call obgetb(sdimpr, 'INFO_TEMPS', linftp)
    call obgetb(sdtabc, 'SORTIE_CSV', lcsv)
!
! --- LISTE DES COLONNES DISPONIBLES
!
    call obgeto(sdtabc, 'COLONNES_DISPOS', slcolo)
    call obgeti(slcolo, 'NBRE_STRUCTS', nbcolo)
!
! --- DESACTIVATION TOUTES LES COLONNES
!
    call oblraz(slcolo)
!
! --- PAS DE TEMPS
!
    if (lcsv) call oblsap(slcolo, 'INCR_INST', .true.)
!
! --- CONTACT - BOUCLE GEOMETRIQUE
!
    if (lboucg) call oblsap(slcolo, 'BOUC_GEOM', .true.)
!
! --- CONTACT - BOUCLE FROTTEMENT
!
    if (lboucf) call oblsap(slcolo, 'BOUC_FROT', .true.)
!
! --- CONTACT - BOUCLE CONTACT
!
    if (lboucc) call oblsap(slcolo, 'BOUC_CONT', .true.)
!
! --- ITERATIONS DE NEWTON
!
    call oblsap(slcolo, 'ITER_NUME', .true.)
!
! --- RESI_GLOB_RELA
!
    call oblsap(slcolo, 'RESI_RELA', .true.)
    if (linfre) call oblsap(slcolo, 'RELA_NOEU', .true.)
!
! --- RESI_GLOB_MAXI
!
    call oblsap(slcolo, 'RESI_MAXI', .true.)
    if (linfre) call oblsap(slcolo, 'MAXI_NOEU', .true.)
!
! --- GEOMETRIE (NEWTON GENERALISE)
!
    if (lnewtg) then
        call oblsap(slcolo, 'GEOM_NEWT', .true.)
        if (linfre) then
            call oblsap(slcolo, 'GEOM_NOEU', .true.)
        endif
    endif
!
! --- SEUIL DE COULOMB (NEWTON GENERALISE)
!
    if (lnewtf) then
        call oblsap(slcolo, 'FROT_NEWT', .true.)
        if (linfre) then
            call oblsap(slcolo, 'FROT_NOEU', .true.)
        endif
    endif
!
! --- STATUTS DE CONTACT (NEWTON GENERALISE)
!
    if (lnewtc) then
        call oblsap(slcolo, 'CONT_NEWT', .true.)
    endif
!
! --- RESIDU PAR REFERENCE
!
    if (lrefe) then
        call oblsap(slcolo, 'RESI_REFE', .true.)
        if (linfre) then
            call oblsap(slcolo, 'REFE_NOEU', .true.)
        endif
    endif
!
! --- RESIDU PAR FORCES NODALES CMP (THM)
!
    if (lcomp) then
        call oblsap(slcolo, 'RESI_COMP', .true.)
        if (linfre) then
            call oblsap(slcolo, 'COMP_NOEU', .true.)
        endif
    endif
!
! --- BOUCLES CONTACT
!
    if (lboucg .or. lboucf .or. lboucc) then
        call oblsap(slcolo, 'BOUC_VALE', .true.)
        if (linfre) then
            call oblsap(slcolo, 'BOUC_NOEU', .true.)
        endif
    endif
!
! --- METHODE DE BORST
!
    if (lborst) then
        call oblsap(slcolo, 'DEBORST  ', .true.)
    endif
!
! --- RECHERCHE LINEAIRE
!
    if (lreli) then
        call oblsap(slcolo, 'RELI_NBIT', .true.)
        call oblsap(slcolo, 'RELI_COEF', .true.)
    endif
!
! --- PILOTAGE
!
    if (lpilo) then
        call oblsap(slcolo, 'PILO_COEF', .true.)
    endif
!
! --- OPTION D'ASSEMBLAGE
!
    call oblsap(slcolo, 'MATR_ASSE', .true.)
!
! --- CONTACT DISCRET - INFORMATIONS SUR SOUS-ITERATIONS
!
    if (lctcd .and. (.not.lallv)) then
        call oblsap(slcolo, 'CTCD_NBIT', .true.)
    endif
!
! --- CONTACT CONTINU - DETECTION CYCLAGE
!
    if (lctcc .and. (.not.lallv)) then
        call oblsap(slcolo, 'CTCC_CYCL', .true.)
    endif
!
!
! --- TEMPS PASSE DANS L'ITERATION
!
    if (linftp) then
        call oblsap(slcolo, 'ITER_TIME', .true.)
    endif
!
! --- ACTIVATION DES COLONNES POUR LE SUIVI EN TEMPS REEL
!
    suiinf = sdsuiv(1:14)//'     .INFO'
    call jeveuo(suiinf, 'L', jsuiin)
    nbsuiv = zi(jsuiin+2-1)
    do isuiv = 1, nbsuiv
        call impfoi(0, 1, isuiv, indsui)
        typcol = 'SUIVDDL'//indsui
        call oblsap(slcolo, typcol, .true.)
    end do
!
! --- CALCUL DE LA LARGEUR DE LA LIGNE
!
    call obtcla(sdtabc)
!
! --- PAS DE VALEUR AFFECTEE
!
    do icolo = 1, nbcolo
        call oblgoi(slcolo, icolo, sdcolo)
        call obsetb(sdcolo, 'VALE_AFFE', .false.)
    end do
!
    call jedema()
!
end subroutine
