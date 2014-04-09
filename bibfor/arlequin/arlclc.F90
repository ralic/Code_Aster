subroutine arlclc(modarl,nbchel,chamel,marlel)

! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================


    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/inical.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedetr.h"
#include "asterfort/megeom.h"
#include "asterfort/wkvect.h"
#include "asterfort/calcul.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------
    character(len=8) ::  modarl
    integer ::      nbchel
    character(len=19) :: chamel(nbchel)
    character(len=8) ::  marlel

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! APPEL A CALCUL - ARLQ_MATR

! ----------------------------------------------------------------------


! IN  MODARL : NOM DU PSEUDO-MODELE
! IN  NBCHEL : NOMBRE DE CHAM_ELEM CREES
! IN  CHAMEL : LISTE DES CHAM_ELEM D'ENTREES
! I/O MARLEL : IN  -> NOM DES MATR_ELEM A CREER
!              OUT -> MATR_ELEM


! ----------------------------------------------------------------------

    integer ::      nbout,nbin
    parameter    (nbout=2, nbin=7)
    character(len=8) ::  lpaout(nbout),lpain(nbin)
    character(len=19) :: lchout(nbout),lchin(nbin)

    character(len=19) :: ligrmo
    character(len=16) :: option
    character(len=19) :: ctfami,ctinfo,ctref1,ctcoo1,ctref2,ctcoo2,chgeom
    integer ::      jarlm1, jarlm2
    parameter   (option = 'ARLQ_MATR')

! ----------------------------------------------------------------------
    call jemarq()

! --- INITIALISATIONS

    ligrmo = modarl(1:8)//'.MODELE'

    ctfami = chamel(1)
    ctinfo = chamel(2)
    ctref1 = chamel(3)
    ctcoo1 = chamel(4)
    ctref2 = chamel(5)
    ctcoo2 = chamel(6)

! --- INITIALISATION DES CHAMPS POUR CALCUL

    call inical(nbin  ,lpain ,lchin , &
                nbout ,lpaout,lchout)

! --- DESTRUCTION DU MARLEL

    call jedetr(marlel(1:8)//'.ARLMT1')
    call jedetr(marlel(1:8)//'.ARLMT2')

! --- CREATION DES LISTES DES CHAMPS IN

    call megeom(modarl,chgeom)

    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PFAMILK'
    lchin(2) = ctfami
    lpain(3) = 'PINFORR'
    lchin(3) = ctinfo
    lpain(4) = 'PREFE1K'
    lchin(4) = ctref1
    lpain(5) = 'PCOOR1R'
    lchin(5) = ctcoo1
    lpain(6) = 'PREFE2K'
    lchin(6) = ctref2
    lpain(7) = 'PCOOR2R'
    lchin(7) = ctcoo2

! --- CALCUL DE MARLEL : MATRICES ELEMENTAIRES

    call wkvect(marlel(1:8)//'.ARLMT1','V V K24',1,jarlm1)
    call wkvect(marlel(1:8)//'.ARLMT2','V V K24',1,jarlm2)

! --- CREATION DES LISTES DES CHAMPS OUT

    lpaout(1) = 'PMATUN1'
    lchout(1) = marlel(1:8)//'.ARLMT1'
    lpaout(2) = 'PMATUN2'
    lchout(2) = marlel(1:8)//'.ARLMT2'

! --- APPEL A CALCUL

    call calcul('S',option,ligrmo,nbin  ,lchin ,lpain, &
                nbout ,lchout,lpaout,'V','OUI')

    call jedema()

end subroutine
