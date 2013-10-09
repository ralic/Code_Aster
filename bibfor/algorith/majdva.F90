subroutine majdva(numedd, sdnume, sddyna, valinc, solalg)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndgrot.h"
#include "asterfort/nmchex.h"
    character(len=24) :: numedd
    character(len=19) :: sddyna, sdnume
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - UTILITAIRE - DYNAMIQUE)
!
! MET A JOUR LES ACCELERATIONS/VITESSES/ROTATIONS DANS
! LE CAS DES POUTRES EN GRANDES ROTATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL
! IN  SDNUME : SD NUMEROTATION
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
!
!
!
    character(len=19) :: vitplu, accplu
    integer :: jvitp, jaccp
    character(len=19) :: ddepla, dvitla, daccla
    integer :: jddepl, jdvite, jdacce
    character(len=19) :: romk
    integer :: jromk
    integer :: i, icomp, iran(3), indro
    integer :: neq
    real(kind=8) :: theta1(3), theta2(3), deldet(3)
    character(len=19) :: depplu, depdel
    integer :: jdepp, jdepde
    character(len=19) :: depkm1, vitkm1, acckm1, romkm1
    integer :: jdepkm, jvitkm, jacckm, jromkm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- POUTRES EN GRANDES ROTATIONS
!
    call jeveuo(sdnume//'.NDRO', 'L', indro)
!
! --- INITIALISATIONS
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(valinc, 'VALINC', 'DEPKM1', depkm1)
    call nmchex(valinc, 'VALINC', 'VITKM1', vitkm1)
    call nmchex(valinc, 'VALINC', 'ACCKM1', acckm1)
    call nmchex(valinc, 'VALINC', 'ROMKM1', romkm1)
    call nmchex(valinc, 'VALINC', 'ROMK  ', romk)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DVITLA', dvitla)
    call nmchex(solalg, 'SOLALG', 'DACCLA', daccla)
!
! --- RECUPERATION DES ADRESSES
!
    call jeveuo(ddepla(1:19)//'.VALE', 'L', jddepl)
    call jeveuo(dvitla(1:19)//'.VALE', 'L', jdvite)
    call jeveuo(daccla(1:19)//'.VALE', 'L', jdacce)
    call jeveuo(depdel(1:19)//'.VALE', 'E', jdepde)
    call jeveuo(depplu(1:19)//'.VALE', 'E', jdepp)
    call jeveuo(vitplu(1:19)//'.VALE', 'E', jvitp)
    call jeveuo(accplu(1:19)//'.VALE', 'E', jaccp)
    call jeveuo(depkm1(1:19)//'.VALE', 'E', jdepkm)
    call jeveuo(vitkm1(1:19)//'.VALE', 'E', jvitkm)
    call jeveuo(acckm1(1:19)//'.VALE', 'E', jacckm)
    call jeveuo(romkm1(1:19)//'.VALE', 'E', jromkm)
    call jeveuo(romk(1:19) //'.VALE', 'L', jromk)
!
! --- MISE A JOUR DEPL/VITE/ACCE
!
    icomp = 0
    do i = 1, neq
        if (zi(indro+i-1) .eq. 0) then
            zr(jdepde+i-1) = zr(jdepde+i-1) + zr(jddepl+i-1)
            zr(jdepp+i-1) = zr(jdepp+i-1) + zr(jddepl+i-1)
            zr(jvitp+i-1) = zr(jvitp+i-1) + zr(jdvite+i-1)
            zr(jaccp+i-1) = zr(jaccp+i-1) + zr(jdacce+i-1)
        else if (zi(indro+i-1).eq.1) then
            zr(jdepkm+i-1) = zr(jdepp+i-1)
            zr(jvitkm+i-1) = zr(jvitp+i-1)
            zr(jacckm+i-1) = zr(jaccp+i-1)
            zr(jromkm+i-1) = zr(jromk+i-1)
            icomp = icomp + 1
            iran(icomp) = i
            deldet(icomp) = zr(jddepl+i-1)
            theta1(icomp) = zr(jdepp+i-1)
            theta2(icomp) = zr(jromk+i-1)
            if (icomp .eq. 3) then
                icomp = 0
                call ndgrot(sddyna, valinc, solalg, deldet, theta1,&
                            theta2, iran)
            endif
        else
            ASSERT(.false.)
        endif
    end do
!
    call jedema()
end subroutine
