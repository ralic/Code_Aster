subroutine xgrals(mode, noma, ln, lt, grlt,&
                  grln)
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: mode, noma
    character(len=19) :: ln, lt, grlt, grln
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
! person_in_charge: samuel.geniaut at edf.fr
!
!                       CALCUL DES GRADIENTS DES LEVEL-SETS
!
!
!    ENTREE :
!              IFM    :   FICHIER D'IMPRESSION
!              MODE   :   OBJET MODELE
!              NOMA   :   OBJET MAILLAGE
!              LN     :   LEVEL SET NORMALE
!              LT     :   LEVEL SET TANGENTE
!
!    SORTIE :
!              GRLN  :   GRADIENT DE LA LEVEL-SET NORMALE
!              GRLT  :   GRADIENT DE LA LEVEL-SET TANGENTE
!
!
    integer :: nchin, ier
    character(len=8) :: lpain(2), lpaout(1)
    character(len=19) :: chgrlt, chgrln, chams
    character(len=24) :: lchin(2), lchout(1), ligrmo
!     ------------------------------------------------------------------
    call jemarq()
!
    chgrlt = '&&OP0112.CHGRLT'
    chgrln = '&&OP0112.CHGRLN'
    chams = '&&OP0112.CHAMS'
!
!     GRADIENT DE LST
!     ---------------
!
    lpain(1)='PGEOMER'
    lchin(1)=noma//'.COORDO'
    lpain(2)='PNEUTER'
    lchin(2)=lt
    lpaout(1)='PGNEUTR'
    lchout(1)=chgrlt
    ligrmo=mode//'.MODELE'
    nchin=2
    call calcul('S', 'GRAD_NEUT_R', ligrmo, nchin, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
!     PASSAGE D'UN CHAM_ELNO EN UN CHAM_NO
    call celces(lchout(1), 'V', chams)
    call cescns(chams, ' ', 'V', grlt, ' ',&
                ier)
!
    call detrsd('CHAM_ELEM_S', chams)
    call detrsd('CHAM_ELEM'  , chgrlt)
!
!     GRADIENT DE LSN
!     ---------------
!
    lpain(1)='PGEOMER'
    lchin(1)=noma//'.COORDO'
    lpain(2)='PNEUTER'
    lchin(2)=ln
    lpaout(1)='PGNEUTR'
    lchout(1)=chgrln
    ligrmo=mode//'.MODELE'
    nchin=2
    call calcul('S', 'GRAD_NEUT_R', ligrmo, nchin, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
!     PASSAGE D'UN CHAM_ELNO EN UN CHAM_NO
    call celces(lchout(1), 'V', chams)
    call cescns(chams, ' ', 'V', grln, ' ',&
                ier)
!
    call detrsd('CHAM_ELEM_S', chams)
    call detrsd('CHAM_ELEM'  , chgrln)
!
    call jedema()
end subroutine
