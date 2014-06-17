subroutine nmpila(numedd, sdpilo, isxfe, dtau, depdel,&
                  ddepl0, ddepl1, nbeffe, eta, pilcvg)
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
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/zerop2.h"
    integer :: pilcvg, nbeffe
    character(len=19) :: sdpilo
    character(len=24) :: numedd
    character(len=19) :: ddepl0, ddepl1, depdel
    real(kind=8) :: dtau, eta(2)
    logical :: isxfe
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - CALCUL DE ETA)
!
! RESOLUTION DE L'EQUATION DE PILOTAGE PAR LONGUEUR D'ARC
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL
! IN  SDPILO : SD PILOTAGE
! IN  ISXFE  : INDIQUE S'IL S'AGIT D'UN MODELE XFEM
! IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
! IN  DDEPL0 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
! IN  DDEPL1 : INCREMENT DE DEPLACEMENT K-1.F_PILO
! IN  DTAU   : SECOND MEMBRE DE L'EQUATION DE PILOTAGE
! OUT NBEFFE : NOMBRE DE SOLUTIONS EFFECTIVES
! OUT ETA    : ETA_PILOTAGE
! OUT PILCVG : CODE DE CONVERGENCE POUR LE PILOTAGE
!                -1 : PAS DE CALCUL DU PILOTAGE
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : PAS DE SOLUTION
!                 2 : BORNE ATTEINTE -> FIN DU CALCUL
!
!
!
!
    integer :: i, j, nrac
    real(kind=8) :: r0, d0, r1, d1, r2, dtau2, rac(2)
    integer ::   jdepde
    integer :: neq
    integer :: ifm, niv
    character(len=19) :: chapil, chapic
    real(kind=8), pointer :: coee(:) => null()
    real(kind=8), pointer :: coef(:) => null()
    real(kind=8), pointer :: dep0(:) => null()
    real(kind=8), pointer :: dep1(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PILOTAGE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<PILOTAGE> ...... PILOTAGE PAR LONGUEUR D''ARC'
    endif
!
! --- INITIALISATIONS
!
    pilcvg = -1
    dtau2 = dtau**2
    r0 = - dtau2
    r1 = 0.d0
    r2 = 0.d0
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- ACCES OBJETS JEVEUX
!
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', vr=dep0)
    call jeveuo(ddepl1(1:19)//'.VALE', 'L', vr=dep1)
    call jeveuo(depdel(1:19)//'.VALE', 'L', jdepde)
    chapil = sdpilo(1:14)//'.PLCR'
    call jeveuo(chapil(1:19)//'.VALE', 'L', vr=coef)
    if (isxfe) then
        chapic = sdpilo(1:14)//'.PLCI'
        call jeveuo(chapic(1:19)//'.VALE', 'L', vr=coee)
    endif
!
! --- CALCUL DES COEFFICIENTS DU POLYNOME DE DEGRE 2
!
    if (isxfe) then
        do i = 1, neq
            if (coee(i) .eq. 0.d0) then
                r0 = r0 + coef(i)**2* (zr(jdepde+i-1)+dep0(i))**2
                r1 = r1 + coef(i)**2* (zr(jdepde+i-1)+dep0(i))*dep1(i)
                r2 = r2 + coef(i)**2* dep1(i) * dep1(i)
            else
                d0 = 0.d0
                d1 = 0.d0
                do j = i+1, neq
                    if (coee(i) .eq. coee(j)) then
                        d0 = d0 + coef(i)*(zr(jdepde+i-1)+dep0(i))+ coef(j)*(z&
                             &r(jdepde+j-1)+dep0(j))
                        d1 = d1 + coef(i)*dep1(i)+ coef(j)*dep1(j)
                    endif
                end do
                r0 = r0 + d0**2
                r1 = r1 + d1*d0
                r2 = r2 + d1**2
            endif
        end do
    else
        do i = 1, neq
            r0 = r0 + coef(i) * (zr(jdepde+i-1)+dep0(i))** 2
            r1 = r1 + coef(i) * (zr(jdepde+i-1)+dep0(i))* dep1(i)
            r2 = r2 + coef(i) * dep1(i) * dep1(i)
        end do
    endif
!
    r1 = 2.d0*r1
    if (r2 .eq. 0) then
        ASSERT(.false.)
    endif
    if (niv .ge. 2) then
        write (ifm,*) '<PILOTAGE> ....EQUATION X2+BX+C: ',r1/r2,r0/r2
    endif
!
! --- RESOLUTION DE L'EQUATION DE DEGRE DEUX
!
    call zerop2(r1/r2, r0/r2, rac, nrac)
!
    if (nrac .eq. 0) then
        pilcvg = 1
    else if (nrac.eq.1) then
        pilcvg = 0
        nbeffe = 1
        eta(1) = rac(1)
    else
        pilcvg = 0
        nbeffe = 2
        eta(1) = rac(1)
        eta(2) = rac(2)
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<PILOTAGE> ...... SOLUTIONS: ',nrac,rac
    endif
!
    call jedema()
!
end subroutine
