subroutine xpogma(nbgma, nb, listgr, ima, jlogma)
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
!
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: nbgma, nb, ima, jlogma
    character(len=24) :: listgr
!
!   AUGMENTATION DE LA TAILLE DES GROUP_MA A CREER
!
!   IN
!       NBGMA   : NOMBRE DE GROUP_MA EN TOUT
!       NB      : NOMBRE D'ELEMENTS A RAJOUTER
!       LISTGR  : LISTE DES GROUPES DE MAILLES
!       IMA     : NUMERO DE LA MAILLE
!
!   IN/OUT
!       JLOGMA  : ADRESSE DES VECTEUR DE TAILLE
!
    integer :: ngrm, iagma, i, ig
    character(len=8) :: k8b
!
!
    call jemarq()
!
    if (nbgma .eq. 0) goto 999
!
    call jelira(jexnum(listgr, ima), 'LONMAX', ngrm, k8b)
!
!     NGRM = 0 : MAILLE N'APPARTENANT A AUCUN GROUPE, ON SORT
    if (ngrm .eq. 0) goto 999
!
    call jeveuo(jexnum(listgr, ima), 'L', iagma)
!
!     BOUCLE SUR LES GROUPES CONTENANT LA MAILLE IMA
    do 10 i = 1, ngrm
!       NUMERO DU GROUPE
        ig = zi(iagma-1+i)
!       ON AUGMENTE DE NB
        zi(jlogma-1+ig)=zi(jlogma-1+ig)+nb
10  end do
!
999  continue
    call jedema()
end subroutine
