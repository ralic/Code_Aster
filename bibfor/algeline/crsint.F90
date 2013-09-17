subroutine crsint(solveu)
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
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/sdsolv.h"
#include "asterfort/wkvect.h"
    character(len=19) :: solveu
!-----------------------------------------------------------------------
!     CREATION D'UNE SD SOLVEUR MUMPS POUR LA ROUTINE CONINT (OPERATEUR
!     MODE_STATIQUE
!     ATTENTION A LA COHERENCE AVEC CRSVMU ET CRSMSP
!-----------------------------------------------------------------------
! IN  K19  SOLVEUR    : NOM DE LA SD SOLVEUR MUMPS BIDON
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    integer :: zslvk, zslvr, zslvi
    integer :: islvk, islvr, islvi
!----------------------------------------------------------------------
    call jemarq()
    zslvk = sdsolv('ZSLVK')
    zslvr = sdsolv('ZSLVR')
    zslvi = sdsolv('ZSLVI')
    call wkvect(solveu//'.SLVK', 'V V K24', zslvk, islvk)
    call wkvect(solveu//'.SLVR', 'V V R', zslvr, islvr)
    call wkvect(solveu//'.SLVI', 'V V I', zslvi, islvi)
!
    zk24(islvk-1+1) = 'MUMPS                   '
    zk24(islvk-1+2) = 'AUTO                    '
    zk24(islvk-1+3) = 'AUTO                    '
    zk24(islvk-1+4) = 'AUTO                    '
    zk24(islvk-1+5) = 'NON                     '
    zk24(islvk-1+6) = 'OUI                     '
    zk24(islvk-1+7) = 'NON                     '
    zk24(islvk-1+8) = 'NON                     '
    zk24(islvk-1+9) = 'IN_CORE                 '
    zk24(islvk-1+10) = 'NON                     '
    zk24(islvk-1+11) = 'AUTO                    '
    zk24(islvk-1+12) = 'XXXX                    '
!
    zr(islvr-1+1) = -1.d0
    zr(islvr-1+2) = -1.d0
    zr(islvr-1+3) = 0.d0
    zr(islvr-1+4) = 0.d0
!
    zi(islvi-1+1) = 9
    zi(islvi-1+2) = 50
    zi(islvi-1+3) = 0
    zi(islvi-1+4) = -9999
    zi(islvi-1+5) = -9999
    zi(islvi-1+6) = 1
    zi(islvi-1+7) = -9999
    zi(islvi-1+8) = 0
    call jedema()
end subroutine
