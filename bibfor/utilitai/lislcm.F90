subroutine lislcm(lischa, ichar, motclc)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lisnnb.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=19) :: lischa
    integer :: ichar
    integer :: motclc(2)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! RETOURNE LE CODE DES MOT-CLEFS DE LA CHARGE
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  ICHAR  : INDICE DE LA CHARGE
! OUT MOTCLC : CODE (ENTIER CODE) CONTENANT LES MOTS-CLEFS
!
! ----------------------------------------------------------------------
!
    character(len=24) :: mocfch
    integer :: jmcfc
    integer :: nbchar
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    motclc(1) = 0
    motclc(2) = 0
    call lisnnb(lischa, nbchar)
!
    if (nbchar .ne. 0) then
        mocfch = lischa(1:19)//'.MCFC'
        call jeveuo(mocfch, 'L', jmcfc)
        motclc(1) = zi(jmcfc-1+2*(ichar-1)+1)
        motclc(2) = zi(jmcfc-1+2*(ichar-1)+2)
    endif
!
    call jedema()
end subroutine
