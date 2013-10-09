subroutine nmpidd(numedd, sdpilo, dtau, depdel, ddepl0,&
                  ddepl1, eta, pilcvg, nbeffe)
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
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "blas/ddot.h"
    integer :: nbeffe
    integer :: pilcvg
    real(kind=8) :: eta, dtau
    character(len=19) :: sdpilo
    character(len=19) :: ddepl0, ddepl1, depdel
    character(len=24) :: numedd
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - CALCUL DE ETA)
!
! RESOLUTION DE L'EQUATION DE PILOTAGE PAR UN DDL IMPOSE
!
! ----------------------------------------------------------------------
!
!
! IN  SDPILO : SD PILOTAGE
! IN  NUMEDD : NUME_DDL
! IN  DTAU   : SECOND MEMBRE DE L'EQUATION DE PILOTAGE
! IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
! IN  DDEPL0 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
! IN  DDEPL1 : INCREMENT DE DEPLACEMENT K-1.F_PILO
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
    real(kind=8) :: du, rn, rd
    integer :: jcoef, jdepde, jdep0, jdep1
    integer :: neq
    character(len=19) :: chapil
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PILOTAGE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<PILOTAGE> ...... PILOTAGE PAR DDL IMPOSE'
    endif
!
! --- INITIALISATIONS
!
    pilcvg = -1
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- ACCES OBJETS JEVEUX
!
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', jdep0)
    call jeveuo(ddepl1(1:19)//'.VALE', 'L', jdep1)
    call jeveuo(depdel(1:19)//'.VALE', 'L', jdepde)
    chapil = sdpilo(1:14)//'.PLCR'
    call jeveuo(chapil(1:19)//'.VALE', 'L', jcoef)
!
! --- RESOLUTION DE L'EQUATION
!
    rn = ddot(neq,zr(jdep0) ,1,zr(jcoef),1)
    rd = ddot(neq,zr(jdep1) ,1,zr(jcoef),1)
    du = ddot(neq,zr(jdepde),1,zr(jcoef),1)
    if (rd .eq. 0.d0) then
        pilcvg = 1
    else
        eta = (dtau - du - rn) / rd
        nbeffe = 1
        pilcvg = 0
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<PILOTAGE> ...... (RN,RD,DU) : ',rn,rd,du
    endif
!
    call jedema()
!
end subroutine
