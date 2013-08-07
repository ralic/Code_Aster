subroutine gfmafi(noma, numagl, centre, nunoeu, nomgrf,&
                  numagr)
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
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
    integer :: nunoeu, numagl, numagr
    character(len=8) :: noma
    character(len=24) :: nomgrf
    real(kind=8) :: centre(2)
!
!     NOMA   : NOM DU MAILLAGE GLOBAL DE LA SD G_FIBRE
!     NUMAGL : NUMERO DE MAILLE DANS LE MAILLAGE GLOBAL
!     CENTRE : COORDONNEES DU CENTRE DE LA FIBRE
!     NUNOEU : NUMERO DU NOEUD DANS LE MAILLAGE GLOBAL
!     NOMGRF : NOM DU GROUPE DE FIBRES/MAILLES
!     NUMAGR : NUMERO DE LA MAILLE DANS LE GROUPE DE FIBRES/MAILLES
!
!     ------------------------------------------------------------------
!     REMPLISSAGE DES ATTRIBUTS DE MAILLAGE POUR LES GROUPES DE FIBRES
!     ISSUS DU MOT CLE FIBRE (.NOMMAI ,.TYPMAIL,COORDO.VALE,CONNEX)
!
!     ----- DECLARATIONS
!
    integer :: icoval, iadt, jconnx, jgg
    character(len=8) :: knma
    character(len=24) :: nommai, cooval, grpmai, connex, typmai
!
    call jemarq()
!
!
!     CONSTRUCTION DES NOMS JEVEUX POUR L OBJET-MAILLAGE
!     --------------------------------------------------
!
!               123456789012345678901234
    nommai = noma// '.NOMMAI         '
    cooval = noma// '.COORDO    .VALE'
    grpmai = noma// '.GROUPEMA       '
    connex = noma// '.CONNEX         '
    typmai = noma// '.TYPMAIL        '
!
!
! - RECUPERATION DES ADRESSES DES CHAMPS
!
    call jeveuo(typmai, 'E', iadt)
!
! -   REMPLISSAGE DES OBJETS .NOMMAI ET .TYPMAI
!
    zi(iadt-1+numagl) = 1
    knma='M0000000'
    write(knma(2:8),'(I7.7)')numagl
    call jecroc(jexnom(nommai, knma))
!
! - STOCKAGE DES NUMERO DES NOEUDS DE LA MAILLE
!
    call jeecra(jexnum(connex, numagl), 'LONMAX', 1)
    call jeveuo(jexnum(connex, numagl), 'E', jconnx)
    zi(jconnx) = nunoeu
!
! --- REMPLISSAGE DE L'OBJET .COORDO    .VALE :
!     -------------------------
    call jeveuo(cooval, 'E', icoval)
!
    zr(icoval+(nunoeu-1)*3-1+1)=centre(1)
    zr(icoval+(nunoeu-1)*3-1+2)=centre(2)
!
!
! --- AJOUT DE LA MAILLE DANS SON GROUPE DE MAILLE
!
    call jeveuo(jexnom(grpmai, nomgrf), 'E', jgg)
    zi(jgg+numagr-1) = numagl
!
    call jedema()
end subroutine
