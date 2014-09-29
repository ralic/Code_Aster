subroutine xajpin(ndim, list, long, ipt, cpt,&
                  newpt, longar, ainter, ia, in,&
                  al, ajout)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/padist.h"
#include "asterfort/xxmmvd.h"
    aster_logical :: ajout
    integer :: ndim, long, ipt, cpt, ia, in
    real(kind=8) :: newpt(3), longar, al, list(*), ainter(*)
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
! person_in_charge: samuel.geniaut at edf.fr
!         AJOUTER UN POINT D'INTERSECTION DANS UNE LISTE
!              ET INFORMATIONS COMPLÉMENTAIRES SUR LES ARETES
!
!     ENTREE
!       NDIM   : DIMENSION DU MAILLAGE
!       LIST   : LA LISTE
!       LONG   : LONGUEUR MAX DE LA LISTE
!       IPT    : LONGUEUR DE LA LISTE AVANT AJOUT
!       CPT    : COMPTEUR SPÉCIFIQUE
!       NEWPT  : COORDONNES DU POINT A AJOUTER
!       LONGAR : LONGUEUR DE L'ARETE
!       AINTER : LISTE DES ARETES
!       IA     : NUMERO DE L'ARETE (0 SI NOEUD SOMMET)
!       IN     : NUMÉRO NOEUD SI NOEUD SOMMET        (0 SINON)
!       AL     : POSITION DU PT SUR L'ARETE (0.D0 SI NOEUD SOMMET)
!     SORTIE
!       LIST,AINTER
!     ------------------------------------------------------------------
    real(kind=8) :: p(3), cridist
    parameter(cridist=1.d-9)
    integer :: i, j
    aster_logical :: deja
    integer :: zxain
! ----------------------------------------------------------------------
!
    call jemarq()
!
    zxain = xxmmvd('ZXAIN')
!
!     VERIFICATION SI CE POINT EST DEJA DANS LA LISTE
    deja = .false.
!
    do 100 i = 1, ipt
        do 99 j = 1, ndim
            p(j) = list(ndim*(i-1)+j)
 99     continue
        if (padist(ndim,p,newpt) .lt. (longar*cridist)) deja = .true.
100 end do
!
    if (.not. deja) then
!       CE POINT N'A PAS DEJA ETE TROUVE, ON LE GARDE
        ipt = ipt + 1
        cpt = cpt + 1
!       TROP DE POINTS DANS LA LISTE
        ASSERT(ipt .le. long)
        do 101 j = 1, ndim
            list(ndim*(ipt-1)+j) = newpt(j)
101     continue
        ainter(zxain*(ipt-1)+1)=ia
        ainter(zxain*(ipt-1)+2)=in
        ainter(zxain*(ipt-1)+3)=longar
        ainter(zxain*(ipt-1)+4)=al
    endif
!
    ajout=.not. deja
    call jedema()
end subroutine
