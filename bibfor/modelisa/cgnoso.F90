subroutine cgnoso(mofaz, iocc, nomaz, lisnoz, nbno)
    implicit  none
#include "jeveux.h"
!
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/oreino.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mess.h"
#include "asterfort/utreno.h"
    integer :: iocc, nbno
    character(len=*) :: mofaz, nomaz, lisnoz
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
! -------------------------------------------------------
!
!       CGNOSO -- TRAITEMENT DE L'OPTION "SEGM_DROI_ORDO"
!                 DU MOT FACTEUR CREA_GROUP_NO DE
!                 LA COMMANDE DEFI_GROUP
!
! ----------------------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
!                                   APPARTENANT A L'ENVELOPPE
!                                   DU CYLINDRE.
!  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! ----------------------------------------------------------------------
!
    integer :: iret, idcoor, jnoeu, numori, numext, n1, iera
    real(kind=8) :: tole
    character(len=8) :: noma, crit, nom1
    character(len=16) :: motfac, motcle(2), typmcl(2)
    character(len=24) :: lisnoe, nomnoe
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    motfac = mofaz
    noma = nomaz
    lisnoe = lisnoz
    iera = 0
!
    nomnoe = noma//'.NOMNOE'
!
! --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
!     --------------------------------------------------
    call jeveuo(noma//'.COORDO    .VALE', 'L', idcoor)
!
! --- RECUPERATION DES NOEUDS A ORDONNER :
!     ----------------------------------
    motcle(1) = 'GROUP_NO'
    motcle(2) = 'NOEUD'
    typmcl(1) = 'GROUP_NO'
    typmcl(2) = 'NOEUD'
    call reliem(' ', noma, 'NU_NOEUD', motfac, iocc,&
                2, motcle, typmcl, lisnoe, nbno)
    if (nbno .le. 0) then
        call u2mess('F', 'MODELISA3_99')
    endif
    call jeveuo(lisnoe, 'E', jnoeu)
!
! --- RECUPERATION DES NOEUDS EXTREMITES :
!     ----------------------------------
    call utreno(motfac, 'ORIG', iocc, noma, nom1)
    call jenonu(jexnom(nomnoe, nom1), numori)
!
    call utreno(motfac, 'EXTR', iocc, noma, nom1)
    call jenonu(jexnom(nomnoe, nom1), numext)
!
! --- RECUPERATION DE LA PRECISION ET DU CRITERE :
!     ------------------------------------------
    call getvr8(motfac, 'PRECISION', iocc, iarg, 1,&
                tole, n1)
    call getvtx(motfac, 'CRITERE', iocc, iarg, 1,&
                crit, n1)
!
! --- ON ORDONNE :
!     ----------
    call oreino(noma, zi(jnoeu), nbno, numori, numext,&
                zr(idcoor), crit, tole, iera, iret)
    ASSERT(iret.eq.0)
!
    call jedema()
!
end subroutine
