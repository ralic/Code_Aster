subroutine cftyli(resoco, iliac, type0)
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
!
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: iliac, type0
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - UTILITAIRE)
!
! RETOURNE LE TYPE DE LA LIAISON SELON UN ENTIER
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  ILIAC  : NUMERO DE LA LIAISON ACTIVE
! OUT TYPE0  : TYPE DE LA LIAISON
!               1  CONTACT
!               2  FROTTEMENT (2D)
!               3  FROTTEMENT - DIRECTION 1 (3D)
!               4  FROTTEMENT - DIRECTION 2 (3D)
!
!
!
!
    integer :: jtypl
    character(len=2) :: typec0, typef0, typef1, typef2
    character(len=19) :: typl
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    typl = resoco(1:14)//'.TYPL'
    call jeveuo(typl, 'L', jtypl)
!
! --- INITIALISATIONS
!
    typec0 = 'C0'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
!
! --- IDENTIFICATION DE LA LIAISON
!
    if (zk8(jtypl-1+iliac) .eq. typec0) then
        type0 = 1
    else if (zk8(jtypl-1+iliac).eq.typef0) then
        type0 = 2
    else if (zk8(jtypl-1+iliac).eq.typef1) then
        type0 = 3
    else if (zk8(jtypl-1+iliac).eq.typef2) then
        type0 = 4
    else
        call assert(.false.)
    endif
    call jedema()
!
end subroutine
