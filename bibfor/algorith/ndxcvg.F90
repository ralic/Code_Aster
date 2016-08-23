subroutine ndxcvg(sddisc, sderro, valinc)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmevel.h"
#include "asterfort/nmltev.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19) :: sddisc, valinc(*)
    character(len=24) :: sderro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! VERIFICATION DES CRITERES D'ARRET - CAS EXPLICITE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : GESTION DES ERREURS
! IN  VALINC : VARIABLE CHAPEAU INCREMENTS DES VARIABLES
!
!
!
!
    integer :: ifm, niv
    integer :: ievdac, numins
    aster_logical :: lerrne, lerrst
    aster_logical :: lsvimx, ldvres, linsta, lcritl, conver, lresmx
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> EVALUATION DE LA CONVERGENCE'
    endif
!
! --- PAR DEFINITION LES RESIDUS ET NEWTON SONT TOUJOURS CONVERGES
!
    call nmeceb(sderro, 'RESI', 'CONV')
    call nmeceb(sderro, 'NEWT', 'CONV')
!
! --- INITIALISATIONS
!
    lsvimx = .false.
    ldvres = .false.
    linsta = .false.
    lcritl = .false.
    conver = .true.
    numins = -1
    call nmltev(sderro, 'ERRI', 'NEWT', lerrne)
    call nmltev(sderro, 'ERRI', 'CALC', lerrst)
!
! --- ERREUR FATALE
!
    if (lerrst) then
        call nmeceb(sderro, 'NEWT', 'STOP')
    endif
!
! --- ERREUR NON FATALE
!
    if (lerrne) then
        call nmeceb(sderro, 'NEWT', 'ERRE')
    endif
!
! --- VERIFICATION DU DECLENCHEMENT DES EVENT-DRIVEN
!
    call nmevel(sddisc, numins, valinc,&
                'NEWT', lsvimx, ldvres, lresmx, linsta, lcritl,&
                lerrne, conver)
!
! --- UN EVENEMENT SE DECLENCHE
!
    call nmacto(sddisc, ievdac)
    if (ievdac .gt. 0) call nmeceb(sderro, 'NEWT', 'EVEN')
!
end subroutine
