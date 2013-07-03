subroutine caldbg(inout, ncham, lcham, lparam)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/dbgobj.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: ncham, i, iret
    character(len=*) :: inout
    character(len=19) :: lcham(*)
    character(len=8) :: lparam(*)
! ----------------------------------------------------------------------
!     BUT : IMPRIMER SUR UNE LIGNE LA VALEUR
!           D'UNE LISTE DE CHAMPS POUR COMPARER 2 VERSIONS
! ----------------------------------------------------------------------
    character(len=19) :: champ
    character(len=24) :: ojb
    character(len=4) :: inou2
! DEB-------------------------------------------------------------------
!
    call jemarq()
    inou2=inout
!
!     1- POUR FAIRE DU DEBUG PAR COMPARAISON DE 2 VERSIONS:
!     -----------------------------------------------------
    do 10,i = 1,ncham
    champ = lcham(i)
    call exisd('CARTE', champ, iret)
    if (iret .gt. 0) ojb = champ//'.VALE'
    call exisd('CHAM_NO', champ, iret)
    if (iret .gt. 0) ojb = champ//'.VALE'
    call exisd('CHAM_ELEM', champ, iret)
    if (iret .gt. 0) ojb = champ//'.CELV'
    call exisd('RESUELEM', champ, iret)
    if (iret .gt. 0) ojb = champ//'.RESL'
!
    call dbgobj(ojb, 'OUI', 6, '&&CALCUL|'//inou2//'|'//lparam(i))
    10 end do
!
    call jedema()
!
end subroutine
