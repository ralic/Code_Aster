subroutine te0222(option, nomte)
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
!.......................................................................
!
!     BUT: SYMETRISATION DES MATRICES ELEMENTAIRES NON_SYMETRIQUES
!          DE LA MANIERE SUIVANTE :
!          MAT2 = 1/2*(MAT1 + MAT1_T)
!
!          OPTION : 'SYME_MDNS_R  ' POUR LA MECANIQUE
!                   'SYME_MTNS_R  ' POUR LA THERMIQUE
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
#include "jeveux.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
    real(kind=8) :: undemi
    integer :: itab1(3), itab2(3)
    integer :: ij, iddl, jddl, nddl, idmat1, idmat2
!
!
!-----------------------------------------------------------------------
    integer :: iret
!-----------------------------------------------------------------------
    undemi = 0.5d0
!
    call tecach('OON', 'PNOSYM', 'L', iret, nval=3,&
                itab=itab1)
!
    if (iret .ne. 0) then
        if (iret .eq. 3) then
!         -- UN RESUELEM N'EST INCOMPLET QUE SI IL N'EXISTE
!            PAS DU TOUT SUR LE GREL.
!            DANS CE CAS, IL N'Y A RIEN A FAIRE
            goto 9999
        else
            call utmess('F', 'ELEMENTS3_46', sk=nomte)
        endif
    endif
    call tecach('OON', 'PSYM', 'E', iret, nval=3,&
                itab=itab2)
!
! --- NOMBRE DE LIGNES DE LA MATRICE ELEMENTAIRE NON-SYMETRIQUE
! --- EN ENTREE :
!     ---------
    nddl = nint(sqrt(dble(itab1(2))))
!
    if (itab1(2) .ne. (nddl*nddl)) then
        call utmess('F', 'ELEMENTS3_47')
    endif
!
    if (2*itab2(2) .ne. (nddl*(nddl+1))) then
        call utmess('F', 'ELEMENTS3_48')
    endif
!
    ij = 0
    idmat1 = itab1(1)
    idmat2 = itab2(1)
!
    do 10 iddl = 1, nddl
        do 20 jddl = 1, iddl
            ij = ij + 1
            zr(idmat2+ij-1) = undemi*(&
                              zr(idmat1+nddl*(iddl-1)+jddl-1) + zr(idmat1+nddl*(jddl-1)+iddl-1))
20      continue
10  end do
!
!
9999  continue
end subroutine
