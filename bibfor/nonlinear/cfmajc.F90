subroutine cfmajc(resoco, neq, nbliac)
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
    implicit     none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "blas/daxpy.h"
    character(len=24) :: resoco
    integer :: neq
    integer :: nbliac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! MISE A JOUR DU VECTEUR SOLUTION ITERATION DE CONTACT
!
! ----------------------------------------------------------------------
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
!
!
!
!
    integer :: iliai, iliac
    character(len=19) :: mu, cm1a, liac
    integer :: jmu, jcm1a, jliac
    character(len=19) :: ddelt
    integer :: jddelt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES STRUCTURES DE DONNEES DE CONTACT
!
    cm1a = resoco(1:14)//'.CM1A'
    mu = resoco(1:14)//'.MU'
    liac = resoco(1:14)//'.LIAC'
    call jeveuo(mu, 'L', jmu)
    call jeveuo(liac, 'L', jliac)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
!
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(ddelt(1:19)//'.VALE', 'E', jddelt)
!
! --- AJOUT DES LIAISONS DE CONTACT ACTIVES
!
    do 10 iliac = 1, nbliac
        iliai = zi(jliac-1+iliac)
        call jeveuo(jexnum(cm1a, iliai), 'L', jcm1a)
        call daxpy(neq, -zr(jmu-1+iliac), zr(jcm1a), 1, zr(jddelt),&
                   1)
        call jelibe(jexnum(cm1a, iliai))
10  end do
!
    call jedema()
!
end subroutine
