subroutine ef0415(nomte)
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
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterfort/cosiro.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/efcoq3d.h"

!
    character(len=16) :: nomte
!
!-----------------------------------------------------------------------
    integer ::  ichg,  icompo
    integer ::  iret
    integer ::  jcara, jeffg, jgeom
    integer :: lzi, lzr, nbcou
    integer :: npge, npgt
    integer :: nso

!-----------------------------------------------------------------------
    parameter(npge=3)
    parameter(npgt=10)
    integer ::  jmat, jnbspi
    integer :: nb1, nb2, npgsr, npgsn




!

    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1=zi(lzi-1+1)
    nb2=zi(lzi-1+2)
    npgsr=zi(lzi-1+3)
    npgsn=zi(lzi-1+4)
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
    if (nomte .eq. 'MEC3QU9H') then
        nso=4
    else if (nomte.eq.'MEC3TR7H') then
        nso=3
    endif
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PCACOQU', 'L', jcara)
!
!
    call cosiro(nomte, 'PCONTRR', 'L', 'UI', 'G',&
                ichg, 'S')
!
    call tecach('ONN', 'PCOMPOR', 'L', iret, iad=icompo)
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou=zi(jnbspi-1+1)
!
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_12')
    endif

    call jevete('&INEL.'//nomte//'.B', ' ', jmat)

    call jevech('PEFFORR', 'E', jeffg)

   call efcoq3d(nomte, nb1, nb2, zr(jcara), zr(jgeom), zr(lzr),& 
                zr(ichg),zr(jmat),zr(jeffg),                   &
                nbcou,npgsn,npgsr,npge,nso,npgt)

!
end subroutine
