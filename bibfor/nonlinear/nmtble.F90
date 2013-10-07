subroutine nmtble(modele, noma, mate, defico, resoco,&
                  niveau, fonact, sdimpr, sdstat, sdtime,&
                  sddyna, sderro, sdconv, sddisc, numins,&
                  valinc, solalg)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/diinst.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mm_cycl_erase.h"
#include "asterfort/mm_cycl_init.h"
#include "asterfort/nmaffi.h"
#include "asterfort/nmctcc.h"
#include "asterfort/nmctcf.h"
#include "asterfort/nmctgo.h"
#include "asterfort/nmevcv.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmleeb.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
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
    integer :: niveau
    integer :: numins
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=24) :: sdstat, sdimpr, sdtime, sderro, sdconv
    character(len=24) :: modele, mate
    character(len=19) :: sddyna, sddisc, valinc(*), solalg(*)
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DEBUT DE BOUCLE POINTS FIXES
!
! ----------------------------------------------------------------------
!
!
! LES ITERATIONS ONT LIEU ENTRE CETTE ROUTINE ET SA COUSINE
! (NMIBLE) QUI COMMUNIQUENT PAR LA VARIABLE NIVEAU
!
! I/O NIVEAU : INDICATEUR D'UTILISATION DE LA BOUCLE DE POINT FIXE
!                  0     ON N'UTILISE PAS CETTE BOUCLE
!                  3     BOUCLE GEOMETRIE
!                  2     BOUCLE SEUILS DE FROTTEMENT
!                  1     BOUCLE CONTRAINTES ACTIVES
! IN  MODELE : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  MATE   : SD MATERIAU
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SDIMPR : SD AFFICHAGE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDYNA : SD POUR DYNAMIQUE
! IN  SDERRO : SD ERREUR
! IN  SDCONV : SD CONVERGENCE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    logical :: mmcvca, mmcvfr, mmcvgo
    logical :: lboucf, lboucg, lboucc
    integer :: mmitgo, mmitfr, mmitco
    character(len=4) :: etnewt
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NEWTON A NECESSAIREMENT CONVERGE
!
    call nmleeb(sderro, 'NEWT', etnewt)
    if ((niveau.eq.0) .or. (etnewt.ne.'CONV')) then
        goto 998
    endif
!
! --- INFOS SUR LES BOUCLES
!
    lboucf = isfonc(fonact,'BOUCLE_EXT_FROT')
    lboucg = isfonc(fonact,'BOUCLE_EXT_GEOM')
    lboucc = isfonc(fonact,'BOUCLE_EXT_CONT')
!
! --- INITIALISATIONS
!
    mmcvca = .false.
    mmcvfr = .false.
    mmcvgo = .false.
    instan = diinst(sddisc,numins)
!
! --- NIVEAU: 1   BOUCLE CONTRAINTES ACTIVES
!
    if (niveau .le. 1) then
!
! --- EVALUATION STATUTS DU CONTACT
!
    if (lboucc) then
        niveau = 1
        call nmtime(sdtime, 'INI', 'CTCC_CONT')
        call nmtime(sdtime, 'RUN', 'CTCC_CONT')
        call nmctcc(noma  , modele, mate  , sddyna, sderro, &
                    sdstat, defico, resoco, valinc, solalg, &
                    mmcvca, instan)
        call nmtime(sdtime, 'END', 'CTCC_CONT')
        call nmrinc(sdstat, 'CTCC_CONT')
!
! ----- ON CONTINUE LA BOUCLE
!
            if (.not.mmcvca) then
                niveau = 1
                goto 999
            endif
        endif
    endif
!
! --- NIVEAU: 2   BOUCLE SEUILS DE FROTTEMENT
!
    if (niveau .le. 2) then
!
! --- CALCUL SEUILS DE FROTTEMENT
!
        if (lboucf) then
            niveau = 2
            call nmtime(sdtime, 'INI', 'CTCC_FROT')
            call nmtime(sdtime, 'RUN', 'CTCC_FROT')
            call nmctcf(noma, modele, sdimpr, sderro, defico,&
                        resoco, valinc, mmcvfr)
            call nmtime(sdtime, 'END', 'CTCC_FROT')
            call nmrinc(sdstat, 'CTCC_FROT')
!
! ----- ON CONTINUE LA BOUCLE
!
            if (.not.mmcvfr) then
                niveau = 2
                goto 999
            endif
        endif
    endif
!
! --- NIVEAU: 3   BOUCLE GEOMETRIE
!
    if (niveau .le. 3) then
!
! --- CALCUL SEUILS DE GEOMETRIE
!
        if (lboucg) then
            niveau = 3
            call nmctgo(noma, sdimpr, sderro, defico, resoco,&
                        valinc, mmcvgo)
!
! ----- ON CONTINUE LA BOUCLE
!
            if (.not.mmcvgo) then
                niveau = 3
                goto 999
            endif
        endif
    endif
!
999 continue
!
! - Initialization of data structures for cycling detection and treatment
!
    if (mmcvca .or. mmcvfr .or. mmcvgo) then
        call mm_cycl_erase(defico, resoco, 0, 0)
    endif
!
! --- AFFICHAGES PENDANT LA BOUCLE DE POINT FIXE
!
    call nmaffi(fonact, sdconv, sdimpr, sderro, sddisc,&
                'FIXE')
!
! --- INCREMENTATION DES COMPTEURS
!
    if (.not.mmcvca .and. niveau .eq. 1) call mmbouc(resoco, 'CONT', 'INCR', mmitco)
    if (.not.mmcvfr .and. niveau .eq. 2) call mmbouc(resoco, 'FROT', 'INCR', mmitfr)
    if (.not.mmcvgo .and. niveau .eq. 3) call mmbouc(resoco, 'GEOM', 'INCR', mmitgo)
!
! --- MISE A JOUR DES ITERATEURS DE BOUCLE
!
    call mmbouc(resoco, 'CONT', 'READ', mmitco)
    call mmbouc(resoco, 'FROT', 'READ', mmitfr)
    call mmbouc(resoco, 'GEOM', 'READ', mmitgo)
    call nmimci(sdimpr, 'BOUC_CONT', mmitco, .true.)
    call nmimci(sdimpr, 'BOUC_FROT', mmitfr, .true.)
    call nmimci(sdimpr, 'BOUC_GEOM', mmitgo, .true.)
!
! --- ETAT DE LA CONVERGENCE POINT FIXE
!
998 continue
    call nmevcv(sderro, fonact, 'FIXE')
!
    call jedema()
end subroutine
