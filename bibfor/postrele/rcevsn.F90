subroutine rcevsn(csigm, cinst, csno, csne)
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rctres.h"
#include "asterfort/wkvect.h"
    character(len=24) :: csigm, cinst, csno, csne
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
!     CALCUL DU SN
!
!     ------------------------------------------------------------------
!
    integer :: ncmp, jsigm, jinst, nbinst, nbordr, jsno, jsne, ind, i1, i2, icmp
    integer :: l1, l2
    parameter  ( ncmp = 6 )
    real(kind=8) :: sn1o(ncmp), sn1e(ncmp), sn2o(ncmp), sn2e(ncmp), sn12o(ncmp)
    real(kind=8) :: sn12e(ncmp), tresca
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call jeveuo(csigm, 'L', jsigm)
    call jeveuo(cinst, 'L', jinst)
    call jelira(cinst, 'LONMAX', nbinst)
!
    nbordr = (nbinst*(nbinst+1)) / 2
    call wkvect(csno, 'V V R', nbordr, jsno)
    call wkvect(csne, 'V V R', nbordr, jsne)
    ind = 0
!
    do 100 i1 = 1, nbinst
!
        do 102 icmp = 1, ncmp
            l1 = ncmp*(i1-1) + icmp
            l2 = ncmp*nbinst + ncmp*(i1-1) + icmp
            sn1o(icmp) = zr(jsigm-1+l1) - zr(jsigm-1+l2)
            sn1e(icmp) = zr(jsigm-1+l1) + zr(jsigm-1+l2)
102      continue
        ind = ind + 1
        zr(jsno+ind-1) = 0.d0
        zr(jsne+ind-1) = 0.d0
!
        do 110 i2 = i1+1, nbinst
!
            do 112 icmp = 1, ncmp
                l1 = ncmp*(i2-1) + icmp
                l2 = ncmp*nbinst + ncmp*(i2-1) + icmp
                sn2o(icmp) = zr(jsigm-1+l1) - zr(jsigm-1+l2)
                sn2e(icmp) = zr(jsigm-1+l1) + zr(jsigm-1+l2)
112          continue
            ind = ind + 1
! ======================================================================
! ---       COMBINAISON DES CONTRAINTES AUX 2 INSTANTS TEMP1 ET TEMP2 :
! ======================================================================
            do 114 icmp = 1, ncmp
                sn12o(icmp) = sn1o(icmp) - sn2o(icmp)
                sn12e(icmp) = sn1e(icmp) - sn2e(icmp)
114          continue
! ======================================================================
! ---       CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS
! ---       DE CONTRAINTES LINEARISEES
! ---       SN12O = SNO(TEMP1)-SNO(TEMP2) A L'ORIGINE DU CHEMIN :
! ---  CALCUL DE LA DIFFERENCE SUP SNO DES VALEURS PROPRES ( LE TRESCA)
! ======================================================================
            call rctres(sn12o, tresca)
            zr(jsno+ind-1) = tresca
! ======================================================================
! ---      CALCUL DES VALEURS PROPRES DE LA DIFFERENCE DES TENSEURS
! ---      DE CONTRAINTES LINEARISEES
! ---      SN12E = SNE(TEMP1)-SNE(TEMP2) A L'AUTRE EXTREMITE DU CHEMIN :
! ---   CALCUL DE LA DIFFERENCE SUP SNE DES VALEURS PROPRES (LE TRESCA)
! ======================================================================
            call rctres(sn12e, tresca)
            zr(jsne+ind-1) = tresca
!
110      continue
!
100  end do
!
    call jedema()
end subroutine
