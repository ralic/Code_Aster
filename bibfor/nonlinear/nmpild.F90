subroutine nmpild(numedd, sddyna, solalg, eta, rho,&
                  offset)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/nmpilk.h"
    character(len=24) :: numedd
    character(len=19) :: solalg(*), sddyna
    real(kind=8) :: eta, rho, offset
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! AJUSTEMENT DE LA DIRECTION DE DESCENTE
!
! ----------------------------------------------------------------------
!
! IN  NUMEDD : NOM DU NUME_DDL
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  SDDYNA : SD DYNAMIQUE
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  RHO    : PARAMETRE DE RECHERCHE LINEAIRE
! IN  OFFSET : DECALAGE DU PARMAETRE DE PILOTAGE
!
! ----------------------------------------------------------------------
!
    integer :: neq
    character(len=19) :: ddepla, deppr1, deppr2
    character(len=19) :: dvitla, vitpr1, vitpr2
    character(len=19) :: daccla, accpr1, accpr2
    logical(kind=1) :: ldyna
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> AJUSTEMENT DIRECTION DE '//&
        'DESCENTE'
    endif
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DEPPR1', deppr1)
    call nmchex(solalg, 'SOLALG', 'DEPPR2', deppr2)
    call nmchex(solalg, 'SOLALG', 'DVITLA', dvitla)
    call nmchex(solalg, 'SOLALG', 'VITPR1', vitpr1)
    call nmchex(solalg, 'SOLALG', 'VITPR2', vitpr2)
    call nmchex(solalg, 'SOLALG', 'DACCLA', daccla)
    call nmchex(solalg, 'SOLALG', 'ACCPR1', accpr1)
    call nmchex(solalg, 'SOLALG', 'ACCPR2', accpr2)
!
! --- CALCUL DE LA DIRECTION DE DESCENTE
!
    call nmpilk(deppr1, deppr2, ddepla, neq, eta,&
                rho, offset)
    if (ldyna) then
        call nmpilk(vitpr1, vitpr2, dvitla, neq, eta,&
                    rho, offset)
        call nmpilk(accpr1, accpr2, daccla, neq, eta,&
                    rho, offset)
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... RHO    : ',rho
        write (ifm,*) '<MECANONLINE> ... ETA    : ',eta
        write (ifm,*) '<MECANONLINE> ... OFFSET : ',offset
        write (ifm,*) '<MECANONLINE> ... DEPL. PRED. (1) : '
        call nmdebg('VECT', deppr1, ifm)
        write (ifm,*) '<MECANONLINE> ... DEPL. PRED. (2) : '
        call nmdebg('VECT', deppr2, ifm)
        write (ifm,*) '<MECANONLINE> ... DEPL. SOLU.     : '
        call nmdebg('VECT', ddepla, ifm)
        if (ldyna) then
            write (ifm,*) '<MECANONLINE> ... VITE. PRED. (1) : '
            call nmdebg('VECT', vitpr1, ifm)
            write (ifm,*) '<MECANONLINE> ... VITE. PRED. (2) : '
            call nmdebg('VECT', vitpr2, ifm)
            write (ifm,*) '<MECANONLINE> ... VITE. SOLU.     : '
            call nmdebg('VECT', dvitla, ifm)
            write (ifm,*) '<MECANONLINE> ... ACCE. PRED. (1) : '
            call nmdebg('VECT', accpr1, ifm)
            write (ifm,*) '<MECANONLINE> ... ACCE. PRED. (2) : '
            call nmdebg('VECT', accpr2, ifm)
            write (ifm,*) '<MECANONLINE> ... ACCE. SOLU.     : '
            call nmdebg('VECT', daccla, ifm)
        endif
    endif
!
    call jedema()
end subroutine
