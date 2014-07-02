subroutine pmevdr(sddisc, tabinc, liccvg, itemax, conver,&
                  actite)
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
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmevel.h"
    aster_logical :: itemax, conver
    character(len=19) :: sddisc, tabinc(*)
    integer :: liccvg(*), actite
!
! ----------------------------------------------------------------------
!
! ROUTINE SIMU_POINT_MAT
!
! VERIFICATION DES CRITERES DE DIVERGENCE DE TYPE EVENT-DRIVEN
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  TABINC : TABLE INCREMENTS DES VARIABLES
! IN  ITEMAX : .TRUE. SI ITERATION MAXIMUM ATTEINTE
! IN  CONVER : .TRUE. SI CONVERGENCE REALISEE
! IN  LICCVG : CODES RETOURS D'ERREUR
!              (2) : INTEGRATION DE LA LOI DE COMPORTEMENT
!                  = 0 OK
!                  = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
!                  = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
!                  = 1 MATRICE DE CONTACT SINGULIERE
!              (5) : MATRICE DU SYSTEME (MATASS)
!                  = 0 OK
!                  = 1 MATRICE SINGULIERE
!                  = 3 ON NE SAIT PAS SI SINGULIERE
! OUT ACTITE : BOUCLE NEWTON -> ACTION POUR LA SUITE
!     0 - NEWTON OK   - ON SORT
!     1 - NEWTON NOOK - IL FAUT FAIRE QUELQUE CHOSE
!     2 - NEWTON NCVG - ON CONTINUE NEWTON
!     3 - NEWTON STOP - TEMPS/USR1
!
!
!
!
    integer :: ifm, niv
    integer :: faccvg, ldccvg, numins
    aster_logical :: lerror, lsvimx, ldvres, linsta, lcritl
    character(len=24) :: k24bla
    integer :: ievdac
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATIONS
!
    ldccvg = liccvg(2)
    faccvg = liccvg(5)
    lerror =(ldccvg.eq.1) .or. (faccvg.ne.0) .or. itemax
    ievdac = 0
    k24bla = ' '
    lsvimx = .false.
    ldvres = .false.
    linsta = .false.
    lcritl = .false.
    numins = -1
!
! --- NEWTON A CONVERGE ?
!
    if (conver) then
        actite = 0
    else
        actite = 2
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<SIMUPOINTMAT> EVALUATION DES EVENT-DRIVEN'
    endif
!
! --- DETECTION DU PREMIER EVENEMENT DECLENCHE
!
    call nmevel(sddisc, numins, k24bla, k24bla, tabinc,&
                'NEWT', lsvimx, ldvres, linsta, lcritl,&
                lerror, conver)
!
! --- UN EVENEMENT SE DECLENCHE
!
    call nmacto(sddisc, ievdac)
    if (ievdac .ne. 0) then
        actite = 1
    endif
!
    call jedema()
end subroutine
