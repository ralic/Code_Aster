subroutine op0088()
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
    implicit none
!     COMMANDE:  DEFI_MAILLAGE
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/cargeo.h"
#include "asterfort/infmaj.h"
#include "asterfort/ssdmdm.h"
#include "asterfort/ssdmdn.h"
#include "asterfort/ssdmgn.h"
#include "asterfort/ssdmrc.h"
#include "asterfort/ssdmrg.h"
#include "asterfort/ssdmrm.h"
#include "asterfort/ssdmte.h"
    character(len=8) :: nomu
    character(len=16) :: kbi1, kbi2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call infmaj()
    call getres(nomu, kbi1, kbi2)
!
!     --TRAITEMENT DU MOT CLEF 'DEFI_SUPER_MAILLE'
    call ssdmdm(nomu)
!
!     --TRAITEMENT DES MOTS CLEF 'RECO_GLOBAL' ET 'RECO_SUPER_MAILLE'
    call ssdmrc(nomu)
    call ssdmrg(nomu)
    call ssdmrm(nomu)
!
!     --TRAITEMENT DU MOT CLEF 'DEFI_NOEUD'
    call ssdmdn(nomu)
!
!     --TRAITEMENT DU MOT CLEF 'DEFI_GROUP_NO'
    call ssdmgn(nomu)
!
!     --ON TERMINE LE MAILLAGE :
    call ssdmte(nomu)
!
    call cargeo(nomu)
!
end subroutine
