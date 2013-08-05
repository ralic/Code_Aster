subroutine gfmama(noma, numagl, nutyma, jcnx, nttri3,&
                  ntqua4, nbnoeu, nomgrf, numagr)
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
!-----------------------------------------------------------------------
!
    implicit none
!     IN
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
    integer :: numagl, jcnx, nutyma, nttri3, ntqua4, nbnoeu, numagr
    character(len=8) :: noma
    character(len=24) :: nomgrf
!     ------------------------------------------------------------------
!     NOMA   : NOM DU MAILLAGE GLOBAL DE LA SD G_FIBRE
!     NUMAGL : NUMERO DE LA MAILLE DANS LE MAILLAGE GLOBAL
!     NUTYMA : NUMERO DE TYPE DE MAILLE
!     JCNX   : ADRESSE DE LA CONNECTIVITE DE LA MAILLE DANS LE MAILLAGE
!              SECTION FOURNI DANS LE MOT CLE FACTEUR SECTION
!     NTTRI3 : NUMERO DE TYPE DE MAILLE DES TRIA3
!     NTQUA4 : NUMERO DE TYPE DE MAILLE DES QUAD4
!     NBNOEU : NB DE NOEUDS DEJA ENTRES DANS .COORDO  .VALE
!     NOMGRF : NOM DU GROUPE DE FIBRES/MAILLES
!     NUMAGR : NUMERO DE LA MAILLE DANS LE GROUPE DE FIBRES/MAILLES
!     ------------------------------------------------------------------
!
! ----- DECLARATIONS
!
    integer :: i, iadt, nno, jconnx, jgg
    character(len=8) :: knma
    character(len=24) :: nommai, grpmai, connex, typmai
!
    call jemarq()
!
!
!     CONSTRUCTION DES NOMS JEVEUX POUR L OBJET-MAILLAGE
!     --------------------------------------------------
!
!               123456789012345678901234
    nommai = noma// '.NOMMAI         '
    grpmai = noma// '.GROUPEMA       '
    connex = noma// '.CONNEX         '
    typmai = noma// '.TYPMAIL        '
!
!
! - RECUPERATION DES ADRESSES DES CHAMPS
!
    call jeveuo(typmai, 'E', iadt)
!
! -   REMPLISSAGE DES OBJETS .NOMMAI ET . TYPMAI
!
    zi(iadt-1+numagl) = nutyma
    knma='M0000000'
    write(knma(2:8),'(I7.7)')numagl
    call jecroc(jexnom(nommai, knma))
!
! - STOCKAGE DES NUMERO DES NOEUDS DE LA MAILLE (CONNECTIVITE)
!
    if (nutyma .eq. nttri3) then
        nno=3
    else if (nutyma.eq.ntqua4) then
        nno=4
    else
        ASSERT(.false.)
    endif
    call jeecra(jexnum(connex, numagl), 'LONMAX', nno, ' ')
    call jeveuo(jexnum(connex, numagl), 'E', jconnx)
    do 15 i = 1, nno
        zi(jconnx+i-1) = zi(jcnx+i-1)+nbnoeu
15  end do
!
!  -  AJOUT DE LA MAILLE DANS SON GROUPE DE MAILLE
!
    call jeveuo(jexnom(grpmai, nomgrf), 'E', jgg)
    zi(jgg+numagr-1) = numagl
!
    call jedema()
end subroutine
