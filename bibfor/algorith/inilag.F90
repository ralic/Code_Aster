subroutine inilag(fmli, icar)
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
!***********************************************************************
!    P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT:      < INITIALISATION DES MATRICE LAGRANGE-LAGRANGE >
    implicit none
!
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! FMLI     /I/: FAMILLE DES MATRICE DE LIAISON
! ICAR     /I/: CARACTERISTIQUE DE LA LIAISON
!
!
!
!
!
!
!   PARAMETER REPRESENTANT LE NOMBRE MAX DE COMPOSANTE DE LA GRANDEUR
!   SOUS-JACENTE TRAITES
!
#include "jeveux.h"
!
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=24) :: fmli
    real(kind=8) :: moinun, zero5
    integer :: icar(4)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iblo, ldmat, nblig
!-----------------------------------------------------------------------
    data moinun /-1.0d+00/
    data zero5 /0.5d+00/
!-----------------------------------------------------------------------
!
    call jemarq()
    nblig=icar(1)
    iblo=icar(3)
!
!
    call jecroc(jexnum(fmli, iblo))
    call jeecra(jexnum(fmli, iblo), 'LONMAX', nblig*2)
    call jeveuo(jexnum(fmli, iblo), 'E', ldmat)
!
!-- LE ICAR(4) INDIQUE LE NUMERO DE LA SOUS STRUCTURE MAITRE
!-- DANS LA LIAISON. LA MULTIPLICATION DE LA MATRICE
!-- LAGRANGE / LAGRANGE PAR UN REEL NE CHANGE PAS LE RESULTAT
    do 10 i = 1, nblig
        zr(ldmat+i-1)=moinun*icar(4)
        zr(ldmat+nblig+i-1)=zero5*icar(4)
10  end do
!
!
!
    call jedema()
end subroutine
