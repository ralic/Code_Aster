subroutine vecgcy(nomres, numeg)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
!***********************************************************************
!    O. NICOLAS      DATE 12/05/05
!-----------------------------------------------------------------------
!  BUT: INITIALISER UN VECTEUR GENERALISE A ZERO
!
!     CONCEPT CREE: VECT_ASSE_GENE
!
!-----------------------------------------------------------------------
!
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomres, numeg, modgen
    character(len=19) :: nomnum, nomsto
    integer ::  iavale, iarefe, iadesc, j, neq
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    character(len=24), pointer :: refn(:) => null()
    integer, pointer :: scde(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
!     1/ LECTURE ET STOCKAGE DES INFORMATIONS
!     =======================================
    nomnum = numeg//'      .NUME'
    nomsto = numeg//'      .SLCS'
    call jeveuo(nomnum//'.REFN', 'L', vk24=refn)
    modgen=refn(1)(1:8)
!
    call jeveuo(nomsto//'.SCDE', 'L', vi=scde)
    neq=scde(1)
!
    call wkvect(nomres//'           .VALE', 'G V R', neq, iavale)
    call wkvect(nomres//'           .REFE', 'G V K24', 2, iarefe)
    call wkvect(nomres//'           .DESC', 'G V I', 3, iadesc)
    zk24(iarefe) = modgen
    zk24(iarefe+1) = nomnum
    zi(iadesc) = 1
    zi(iadesc+1) = neq
!
    do 60 j = 1, neq
        zr(iavale+j-1) = 0.d0
60  continue
!
!
!
    call jedema()
end subroutine
