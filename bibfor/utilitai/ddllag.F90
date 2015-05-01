subroutine ddllag(nume, iddl, neq, lagr1, lagr2)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: iddl, neq, lagr1, lagr2
    character(len=*) :: nume
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!
!     RECHERCHE LES DEUX LAGRANGES ASSOCIES AU DDL IDDL.
!     CE IDDL DDL EST BLOQUE ET ON NE LE VERIFIE PAS.
!     DANS LE CAS OU IDDL N'EST PAS BLOQUE, LAGR1=LAGR2=0
!
! IN  : NUME   : NOM D'UN NUME_DDL
! IN  : IDDL   : NUMERO D'UN DDL BLOQUE
! IN  : NEQ    : NOMBRE D'EQUATIONS
! OUT : LAGR1  : PREMIER LAGRANGE ASSOCIE
! OUT : LAGR2  : DEUXIEME LAGRANGE ASSOCIE
! ----------------------------------------------------------------------
    character(len=24) :: nomnu
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i,  icas, icmp, inoe, nc, nn
    integer, pointer :: deeq(:) => null()
!
!-----------------------------------------------------------------------
    call jemarq()
    lagr1 = 0
    lagr2 = 0
    nomnu(1:14) = nume
    nomnu(15:19) = '.NUME'
    call jeveuo(nomnu(1:19)//'.DEEQ', 'L', vi=deeq)
!
    inoe = deeq(1+ (2*(iddl-1)) + 1 - 1 )
    icmp = -deeq(1+ (2*(iddl-1)) + 2 - 1 )
    icas = 1
    do 10 i = 1, neq
        nn = deeq(1+ (2*(i-1)) + 1 - 1 )
        nc = deeq(1+ (2*(i-1)) + 2 - 1 )
        if (nn .eq. inoe .and. nc .eq. icmp) then
            if (icas .eq. 1) then
                lagr1 = i
                icas = 2
            else
                lagr2 = i
                goto 9999
            endif
        endif
10  end do
!
9999  continue
    call jedema()
end subroutine
