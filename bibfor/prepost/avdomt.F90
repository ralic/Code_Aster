subroutine avdomt(nbvec, nbordr, ncycl, jdomel, domtot)
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
! person_in_charge: van-xuan.tran at edf.fr
    implicit   none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbvec, nbordr, ncycl(nbvec), jdomel
    real(kind=8) ::  domtot(nbvec)
!   real(kind=8) ::domel(nbvec*nbordr),
! ----------------------------------------------------------------------
! BUT: CALCULER LE DOMMAGE TOTAL (CUMUL) POUR TOUS LES VECTEURS NORMAUX.
! ----------------------------------------------------------------------
! ARGUMENTS :
!  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
!  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
!  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
!  JDOMEL    IN   I  : ADDRESSE VECTEUR CONTENANT LES VALEURS DES DOMMAGES
!                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
!                     DE CHAQUE VECTEUR NORMAL.
!  DOMTOT   OUT  R  : VECTEUR CONTENANT LES DOMMAGES TOTAUX (CUMUL)
!                     DE CHAQUE VECTEUR NORMAL.
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ivect, icycl, adrs, i
!     ------------------------------------------------------------------
!234567                                                              012
!
    call jemarq()
!
! INITIALISATION
!
    do 100 i = 1, nbvec
        domtot(i) = 0
100  end do
!
    do 10 ivect = 1, nbvec
        do 20 icycl = 1, ncycl(ivect)
            adrs = (ivect-1)*nbordr + icycl
            domtot(ivect) = domtot(ivect) + zr(jdomel+adrs)
20      continue
10  end do
!
    call jedema()
!
end subroutine
