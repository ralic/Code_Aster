subroutine nmceni(numedd, depdel, deppr1, deppr2, rho,&
                  sdpilo, eta, isxfe, f)
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
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: numedd
    character(len=19) :: sdpilo, depdel, deppr1, deppr2
    real(kind=8) :: eta, rho, f
    logical :: isxfe
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - SELECTION PARAMETRE)
!
! CALCUL DU PARAMETRE DE SELECTION DE TYPE NORM_INCR_DEPL
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL
! IN  SDPILO : SD PILOTAGE
! IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
! IN  DEPPR1 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
! IN  DEPPR2 : INCREMENT DE DEPLACEMENT K-1.F_PILO
! IN  RHO    : PARAMETRE DE RECHERCHE LINEAIRE
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  ISXFE  : INDIQUE S'IL S'AGIT D'UN MODELE XFEM
! OUT F      : VALEUR DU CRITERE
!
!
!
!
    character(len=19) :: profch, chapil, chapic
    integer :: neq, i, j
    real(kind=8) :: dn, dc, dp
    integer, pointer :: deeq(:) => null()
    real(kind=8), pointer :: coee(:) => null()
    real(kind=8), pointer :: coef(:) => null()
    real(kind=8), pointer :: depde(:) => null()
    real(kind=8), pointer :: du0(:) => null()
    real(kind=8), pointer :: du1(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    if (isxfe) then
        chapil = sdpilo(1:14)//'.PLCR'
        call jeveuo(chapil(1:19)//'.VALE', 'L', vr=coef)
        chapic = sdpilo(1:14)//'.PLCI'
        call jeveuo(chapic(1:19)//'.VALE', 'L', vr=coee)
    endif
!
! --- INITIALISATIONS
!
    f = 0.d0
!
! --- INFORMATIONS SUR NUMEROTATION
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    call dismoi('PROF_CHNO', depdel, 'CHAM_NO', repk=profch)
    call jeveuo(profch(1:19)//'.DEEQ', 'L', vi=deeq)
!
! --- ACCES AUX VECTEURS SOLUTIONS
!
    call jeveuo(depdel(1:19)//'.VALE', 'L', vr=depde)
    call jeveuo(deppr1(1:19)//'.VALE', 'L', vr=du0)
    call jeveuo(deppr2(1:19)//'.VALE', 'L', vr=du1)
!
!
! --- CALCUL DE LA NORME
!
    if (isxfe) then
        do i = 1, neq
            if (deeq(2*i ) .gt. 0) then
                if (coee(i) .eq. 0.d0) then
                    f = f + coef(i)**2* (depde(i)+rho*du0(i)+ eta*du1(i)&
                        &)**2
                else
                    dn = 0.d0
                    dc = 0.d0
                    dp = 0.d0
                    do j = i+1, neq
                        if (coee(i) .eq. coee(j)) then
                            dn = dn + coef(i)*depde(i)+ coef(j)*depde(j)
                            dc = dc + coef(i)*du0(i)+ coef(j)*du0(j)
                            dp = dp + coef(i)*du1(i)+ coef(j)*du1(j)
                        endif
                    end do
                    f = f + (dn+rho*dc+eta*dp)**2
                endif
            endif
        end do
    else
        do i = 1, neq
            if (deeq(2*i + 2) .gt. 0) then
                f = f + (depde(1+i)+rho*du0(1+i)+eta*du1(1+i))** 2
            endif
        end do
    endif
    call jedema()
end subroutine
