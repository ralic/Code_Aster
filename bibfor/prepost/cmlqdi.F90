function cmlqdi(nbma, nbno, lima, connez)
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
    implicit none
    integer :: cmlqdi
!
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: nbma, nbno, lima(*)
    character(len=*) :: connez
    character(len=24) :: connex
!
!
! ----------------------------------------------------------------------
!           DECOMPTE DU NOMBRE MAXIMUM D'ARETES PAR NOEUD
! ----------------------------------------------------------------------
! IN  NBMA    NOMBRE DE MAILLES A TRAITER
! IN  NBNO    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! IN  LIMA    LISTE DES MAILLES A TRAITER
! IN  CONNEX  CONNECTIVITE DES MAILLES (COLLECTION JEVEUX)
! RET         NOMBRE MAXIMAL D'ARETE PAR NOEUD
! ----------------------------------------------------------------------
!
!
    integer :: m, ma, n, no, jnbma, nbnoma, jnoma, mxar
! ----------------------------------------------------------------------
!
!
    call jemarq()
    connex = connez
!
!    INITIALISATION
!
    call wkvect('&&CMLQDI.NB_MAILLES', 'V V I', nbno, jnbma)
    do 5 no = 1, nbno
        zi(jnbma-1 + no) = 0
 5  end do
!
!
!    NOMBRE DE MAILLES AUXQUELLES APPARTIENT CHAQUE NOEUD
!
    do 10 m = 1, nbma
        ma = lima(m)
!
!      NOEUDS DE LA MAILLE
        call jelira(jexnum(connex, ma), 'LONMAX', nbnoma)
        call jeveuo(jexnum(connex, ma), 'L', jnoma)
!
!      COMPTABILISATION DES MAILLES PAR NOEUD
        do 20 n = 1, nbnoma
            no = zi(jnoma-1 + n)
            zi(jnbma-1+no) = zi(jnbma-1+no) + 1
20      continue
10  end do
!
!
!    MAJORANT DU NOMBRE D'ARETES PAR NOEUD
!
    mxar = 0
    do 30 no = 1, nbno
        mxar = max(mxar, 4*zi(jnbma-1+no))
30  end do
!
!
    call jedetr('&&CMLQDI.NB_MAILLES')
    call jedema()
!
    cmlqdi = mxar
end function
