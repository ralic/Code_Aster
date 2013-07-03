subroutine caveas(chargz)
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
!.======================================================================
    implicit none
!
!       CAVEAS -- TRAITEMENT DU MOT CLE VECT_ASSE
!
!      TRAITEMENT DU MOT CLE VECT_ASSE DE AFFE_CHAR_MECA
!      CE MOT CLE PERMET DE SPECIFIER UN VECTEUR ASSEMBLE (UN CHAM_NO)
!      QUI SERVIRA DE SECOND MEMBRE DANS STAT_NON_LINE
!                                   OU   DYNA_NON_LINE
!
! -------------------------------------------------------
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -   LA  CHARGE EST ENRICHIE
!                                   DU VECTEUR ASSEMBLE DONT LE NOM
!                                   EST STOCKE DANS L'OBJET
!                                   CHAR//'CHME.VEASS'
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
!
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterfort/chpver.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=*) :: chargz
! ------ VARIABLES LOCALES
    character(len=8) :: charge, vecass
    character(len=24) :: obj
    integer :: iarg
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: idveas, ier, nvecas
!-----------------------------------------------------------------------
    call jemarq()
!
    call getvid(' ', 'VECT_ASSE', 0, iarg, 1,&
                vecass, nvecas)
    if (nvecas .eq. 0) goto 9999
!
    call chpver('F', vecass, 'NOEU', 'DEPL_R', ier)
!
    charge = chargz
    obj = charge//'.CHME.VEASS'
!
    call wkvect(obj, 'G V K8', 1, idveas)
    zk8(idveas) = vecass
!
9999  continue
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
