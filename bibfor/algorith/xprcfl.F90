subroutine xprcfl(model, lcmin)
    implicit none
!
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/megeom.h"
#include "asterfort/memaxm.h"
    real(kind=8) :: lcmin
    character(len=8) :: model
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPRCFL   : X-FEM PROPAGATION : CALCUL DES CONDITIONS CFL
!       ------     -     --                                  ---
!    CALCUL DE LA CONDITIONS CFL POUR LES PHASES DE REORTHOGONALISATION
!    ET REINITIALISATION DES LEVEL SETS, C'EST A DIRE LA LONGUEUR
!    MINIMALE DES ARETES DU MAILLAGE
!
!    ENTREE
!        MODEL   : NOM DU CONCEPT MODELE
!
!    SORTIE
!        LCMIN   : LONGUEUR CARACTERISTIQUE MINIMALE DU MAILLAGE
!
!     ------------------------------------------------------------------
!
!
    integer :: ibid, ifm, niv
    character(len=8) :: lpain(1), lpaout(1)
    character(len=19) :: cellc
    character(len=24) :: ligrel, chgeom, lchin(1), lchout(1)
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    cellc='&&XPRCFL.CELLC'
!
    call megeom(model, chgeom)
    ligrel=model//'.MODELE'
    lpain(1)='PGEOMER'
    lchin(1)=chgeom
    lpaout(1)='PLONCAR'
    lchout(1)=cellc
!
    call calcul('S', 'CFL_XFEM', ligrel, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
!   ON VA CHERCHER LE MINIMUM DE CELLC SUR LES ELEMENTS -->  LCMIN
    call memaxm('MIN', cellc, 'X1', 1, 'X1',&
                lcmin, 0, ibid)
    call jedetr(cellc)
!
    write(ifm,*)'   LONGUEUR DE LA PLUS PETITE ARETE DU MAILLAGE: ',&
     &            lcmin
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
