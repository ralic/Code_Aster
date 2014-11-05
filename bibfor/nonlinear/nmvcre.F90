subroutine nmvcre(modelz, matz, carelz, comrez)
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
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmvcd2.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcref.h"
    character(len=*) :: modelz, matz, carelz, comrez
! ----------------------------------------------------------------------
!  CREATION DES VALEURS DE REFERENCE DES VARIABLES DE COMMANDE
! ----------------------------------------------------------------------
! IN/JXIN   MODELZ  K8  SD MODELE
! IN/JXIN   MATZ    K8  SD MATERIAU
! IN        CARELE  K8  SD CARAELEM
! IN/JXOUT  COMREZ  K14 SD VARI_COM
! ----------------------------------------------------------------------
!
!
!
!
    character(len=8) :: modele, mate, carele
    character(len=14) :: comref
    character(len=19) :: champ
    integer :: iret
!
!
    call jemarq()
    modele = modelz
    mate = matz
    carele = carelz
    comref = comrez
    champ=comref//'.TOUT'
!
    call detrsd('VARI_COM', comref)
    call vrcref(modele, mate, carele, champ)
    call exisd('CHAMP_GD', champ, iret)
!
    call jedema()
!
end subroutine
