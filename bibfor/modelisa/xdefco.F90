subroutine xdefco(noma, nomo, fiss, algola, ndim,&
                  nliseq)
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
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/xlagsp.h"
    integer :: ndim, i, ier
    character(len=8) :: fiss, noma, nomo
    integer :: algola
    character(len=19) :: nliseq
    character(len=24) :: grma(3)
    aster_logical :: lxfem
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (MODIF. DU MODELE)
!
! CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NOMA   : NOM DE L'OBJET MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  ALGOLA : TYPE DE CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
! IN  FISS   : SD FISS_XFEM
! OUT NLISRL : LISTE REL. LIN. POUR V1 ET V2
! OUT NLISCO : LISTE REL. LIN. POUR V1 ET V2
! OUT NLISEQ : LISTE REL. LIN. POUR V2 SEULEMENT
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES AUX OBJETS
!
    grma(1)=fiss//'.MAILFISS.HEAV'
    grma(2)=fiss//'.MAILFISS.CTIP'
    grma(3)=fiss//'.MAILFISS.HECT'
!
    lxfem=.false.
    do 10 i = 1, 3
        call jeexin(grma(i), ier)
        if (ier .ne. 0) lxfem=.true.
10  continue
    ASSERT(lxfem)
!
! --- CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT
!
    call xlagsp(noma, nomo, fiss, algola, ndim,&
                nliseq)
!
    call jedema()
end subroutine
