subroutine elmddl(raide, option, neq, ddl, nddle,&
                  nbddl, vecddl)
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
!
! aslint: disable=W1306
    implicit none
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/pteddl.h"
    character(len=19) :: raide
    character(len=14) :: option
    integer :: neq, nbddl, vecddl(neq), nddle
    character(len=8) :: ddl(nddle)
!
! ----------------------------------------------------------------------
!
! CONSTRUCTION D'UN TABLEAU D'ENTIERS REPERANT LA POSITION DES DDL
! EXCLUS DE LA RECHERCHE DE VALEURS PROPRES
!
! ----------------------------------------------------------------------
!
! IN  RAIDEUR : NOM DE LA MATRICE DE "RAIDEUR"
! IN  OPTION  : TYPE DE DDL A TROUVER
! IN  NEQ     : NPMBRE DE DDL
! IN  DDL     : NOM DU DDL A ELIMINER
! IN  NDDLE   : NOMBRE DE TYPES DE DDL EXCLUS
! OUT NBDDL   : NOMBRE DE DDL A ELIMINER
! OUT VECDDL  : POSITION DES DDL A ELIMINER
!
! ----------------------------------------------------------------------
!
    integer :: ieq, ifm, niv, i, inter(neq)
    character(len=14) :: nume
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATIONS
!
    nbddl = 0
    call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=nume)
    do ieq = 1, neq
        vecddl(ieq) = 1
    end do
!
! --- CALCUL DU NOMBRE DE DDL A ELIMINER
!
    if (nddle .gt. 0) then
        do i = 1, nddle
!
! ------- RECUPERATION DES POSITIONS DES DDL
!
            call pteddl('NUME_DDL', nume, 1, ddl(i), neq,&
                        list_equa = inter)
!
! ------- CALCUL DU NOMBRE DE 'DDL': NBDDL
!
            do ieq = 1, neq
                nbddl = nbddl + inter(ieq)
            end do
!
! ------- STOP SI ON CHERCHE A ELIM UN DDL ABSENT DE LA MODELISATION
!
            if (nbddl .eq. 0) then
                ASSERT(.false.)
            endif
!
! ------- INVERSION : INTER = 0 SI DDL TROUVE ET 1 SINON
!
            do ieq = 1, neq
                inter(ieq) = abs(inter(ieq)-1)
            end do
!
            do ieq = 1, neq
                vecddl(ieq) = vecddl(ieq)*inter(ieq)
            end do
!
        end do
    endif
!
! --- IMPRESSION DES DDL
!
    if (niv .ge. 1) then
        if (nbddl .gt. 0) then
            write (ifm,*) option
            do i = 1, nddle
                write (ifm,910) ddl(i)
            end do
            write (ifm,950) nbddl
            write (ifm,960)
        else
            write (ifm,901)
        endif
    endif
!
! ----------------------------------------------------------------------
!
    call jedema()
!
    901 format (1x,'PAS DE DDL_TROUVE')
    910 format (13x,a8,/)
    950 format (1x,'NOMBRE DE DDL',10x,i7,/)
    960 format (72('-'))
!
end subroutine
