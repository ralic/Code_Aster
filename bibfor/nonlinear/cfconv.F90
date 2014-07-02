subroutine cfconv(noma, sdstat, sdimpr, sderro, defico,&
                  resoco, solalg)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/cfcgeo.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmrvai.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco, sdimpr, sderro, sdstat
    character(len=19) :: solalg(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES)
!
! CONVERGENCE LIEE AU CONTACT DISCRET
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDIMPR : SD AFFICHAGE
! IN  SDSTAT : SD STATISTIQUES
! IN  SDERRO : GESTION DES ERREURS
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    aster_logical :: lallv, lreag
    character(len=24) :: clreac
    integer :: jclrea
    character(len=16) :: geonoe, k16bla
    real(kind=8) :: geoval, r8bid
    aster_logical :: ctderg, cvresi
    aster_logical :: dvpfix, dvfixg
    integer :: ctcite
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    dvpfix = .false.
    dvfixg = .false.
    ctderg = .false.
    lreag = .false.
    k16bla = ' '
    geonoe = ' '
    geoval = r8vide()
    call nmrvai(sdstat, 'CTCD_ALGO_ITER', 'N', ctcite)
!
! --- VALEURS NON AFFECTEES DANS LE TABLEAU
!
    call nmimck(sdimpr, 'BOUC_NOEU', k16bla, .false._1)
    call nmimcr(sdimpr, 'BOUC_VALE', r8bid, .false._1)
!
! --- CONVERGENCE DES RESIDUS D'EQUILIBRE ?
!
    call nmlecv(sderro, 'RESI', cvresi)
!
! --- ACCES SD CONTACT
!
    clreac = resoco(1:14)//'.REAL'
!
! --- FONCTIONNALISTES ACTIVEES
!
    lallv = cfdisl(defico,'ALL_VERIF')
!
! --- CONVERGENCE
!
    if (.not.lallv) then
!
        call jeveuo(clreac, 'E', jclrea)
!
! ----- ATTENTE POINT FIXE ?
!
        dvpfix = zl(jclrea+2-1)
        lreag = .false.
!
! ----- EVALUATION DE LA REACUALISATION GEOMETRIQUE POUR CONTACT DISCRET
!
        if (cvresi) then
            if (.not.dvpfix) then
                call cfcgeo(noma, defico, resoco, solalg, dvfixg,&
                            ctderg, geonoe, geoval)
                zl(jclrea+1-1) = dvfixg
                zl(jclrea+4-1) = ctderg
                lreag = .true.
            endif
        endif
    endif
!
! --- ENREGISTREMENT DES VALEURS POUR AFFICHAGE
!
    if (lreag) then
        call nmimck(sdimpr, 'BOUC_NOEU', geonoe, .true._1)
        call nmimcr(sdimpr, 'BOUC_VALE', geoval, .true._1)
    endif
    call nmimci(sdimpr, 'CTCD_NBIT', ctcite, .true._1)
!
! --- ENREGISTREMENT DES EVENEMENTS - DIVERGENCES
!
    call nmcrel(sderro, 'DIVE_PFIX', dvpfix)
!
    call jedema()
end subroutine
