subroutine gilig1(nfic, ndim, nbval, nbpoin)
    implicit   none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: nfic, ndim, nbval, nbpoin
! ----------------------------------------------------------------------
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
!
!     BUT: LIRE LES N LIGNES DES POINTS DU MAILLAGE GIBI :
!                 ( PROCEDURE SAUVER)
!
!     IN: NFIC   : UNITE DE LECTURE
!         NDIM   : DIMENSION DU MAILLAGE : 2D OU 3D.
!         NBVAL  : NOMBRE DE VALEURS A LIRE
!
! ----------------------------------------------------------------------
!
    integer :: iacoor, iacoo1, nbfois, nbrest, icoj, i, j
    real(kind=8) :: rbid(3)
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     -- ON CREE L'OBJET QUI CONTIENDRA LES COORDONNEES DES POINTS:
!        (ON CREE AUSSI UN OBJET TEMPORAIRE A CAUSE DES DENSITES)
!
    call wkvect('&&GILIRE.COORDO', 'V V R', nbpoin*ndim, iacoor)
    call wkvect('&&GILIRE.COORD1', 'V V R', nbpoin*(ndim+1), iacoo1)
!
!     -- ON LIT LES COORDONNEES DES NOEUDS:
!
    nbfois = nbval / 3
    nbrest = nbval - 3*nbfois
    icoj = 0
    do 10 i = 1, nbfois
        read(nfic,1000) ( rbid(j), j=1,3 )
        zr(iacoo1-1+ icoj +1) = rbid(1)
        zr(iacoo1-1+ icoj +2) = rbid(2)
        zr(iacoo1-1+ icoj +3) = rbid(3)
        icoj = icoj + 3
10  end do
    if (nbrest .gt. 0) then
        read(nfic,1000) ( rbid(j), j=1,nbrest )
        do 12 i = 1, nbrest
            zr(iacoo1-1+ icoj +i) = rbid(i)
12      continue
    endif
!
!     -- ON RECOPIE LES COORDONNEES EN OTANT LES DENSITES:
    do 20 i = 1, nbpoin
        do 22 j = 1, ndim
            zr(iacoor-1+ndim*(i-1)+j)=zr(iacoo1-1+(ndim+1)*(i-1)+j)
22      continue
20  end do
    call jedetr('&&GILIRE.COORD1')
!
    1000 format( 3(1x,d21.14) )
!
    call jedema()
!
end subroutine
