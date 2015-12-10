subroutine nmcofr(mesh  , depplu, depdel    , ddepla, solveu,&
                  numedd, matass, ds_contact, iterat, resigr,&
                  sdstat, sdtime, ctccvg    , instan)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfalgo.h"
#include "asterfort/cfgeom.h"
#include "asterfort/cfsvfr.h"
#include "asterfort/cfsvmu.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmtime.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8) :: mesh
    character(len=19) :: depplu
    character(len=19) :: depdel, ddepla
    character(len=14) :: numedd
    character(len=24) :: sdtime, sdstat
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: solveu, matass
    real(kind=8) :: resigr
    integer :: iterat, ctccvg
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES)
!
! TRAITEMENT DU CONTACT AVEC OU SANS FROTTEMENT DANS STAT_NON_LINE.
! BRANCHEMENT SUR LES ROUTINES DE RESOLUTION
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEPPLU : CHAMP DE DEPLACEMENTS A L'ITERATION DE NEWTON PRECEDENTE
! IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
! IN  DEPPLA : INCREMENT DE DEPLACEMENTS CALCULE EN IGNORANT LE CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  NUMEDD : NUME_DDL
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! In  ds_contact       : datastructure for contact management
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  RESIGR : RESI_GLOB_RELA
! OUT CTCCVG : CODE RETOUR CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: clreac
    integer :: jclrea
    aster_logical :: reageo, ctcfix, reapre
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> DEBUT DU TRAITEMENT DES CONDITIONS DE CONTACT'
    endif
!
! --- INITIALISATIONS
!
    ctccvg = -1
!
! --- ACCES OBJETS
!
    clreac = ds_contact%sdcont_solv(1:14)//'.REAL'
    call jeveuo(clreac, 'E', jclrea)
!
! --- PARAMETRES POUR BOUCLES GEOMETRIQUE/PT FIXE
!
    reageo = zl(jclrea+1-1)
    ctcfix = zl(jclrea+2-1)
!
! --- SAUVEGARDE AVANT APPARIEMENT
!
    if (reageo) then
        call cfsvmu(ds_contact, .false._1)
        call cfsvfr(ds_contact, .false._1)
    endif
!
! --- APPARIEMENT
!
    call cfgeom(reageo, iterat, mesh, sdtime, sdstat,&
                ds_contact, depplu, instan)
!
! --- ALGORITHMES DE CONTACT
!
    call nmtime(sdtime, 'INI', 'CTCD_ALGO')
    call nmtime(sdtime, 'RUN', 'CTCD_ALGO')
    call cfalgo(mesh, sdstat, resigr, iterat, ds_contact,&
                solveu, numedd, matass, ddepla,&
                depdel, ctccvg, ctcfix)
    call nmtime(sdtime, 'END', 'CTCD_ALGO')
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> FIN DU TRAITEMENT DES CONDITIONS DE CONTACT'
    endif
!
! --- DESACTIVATION REAC_GEOM
!
    reageo = .false.
    reapre = .false.
!
! --- SAUVEGARDE
!
    zl(jclrea+1-1) = reageo
    zl(jclrea+2-1) = ctcfix
    zl(jclrea+3-1) = reapre
!
! --- LE CALCUL DE CONTACT A FORCEMENT ETE REALISE
!
    ASSERT(ctccvg.ge.0)
!
    call jedema()
end subroutine
