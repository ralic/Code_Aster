subroutine cuadu(deficu, resocu, neq, nbliac)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/caladu.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: neq
    integer :: nbliac
    character(len=24) :: deficu
    character(len=24) :: resocu
!
! ----------------------------------------------------------------------
! ROUTINE APPELEE PAR : ALGOCU
! ----------------------------------------------------------------------
!
!  ROUTINE MERE POUR LE CALCUL DU SECOND MEMBRE
!
! IN  DEFICU : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCU : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!                'E': RESOCU(1:14)//'.MU'
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
!
!
!
!
    integer :: iliac, nbddl
    integer :: lliac, jdecal
    real(kind=8) :: val
    character(len=19) :: liac, mu, delt0
    integer :: jliac, jmu, jdelt0
    character(len=24) :: apddl, apcoef, apjeu, poinoe
    integer :: japddl, japcoe, japjeu, jpoi
! ======================================================================
    call jemarq()
! ======================================================================
    apddl = resocu(1:14)//'.APDDL'
    liac = resocu(1:14)//'.LIAC'
    apcoef = resocu(1:14)//'.APCOEF'
    apjeu = resocu(1:14)//'.APJEU'
    mu = resocu(1:14)//'.MU'
    delt0 = resocu(1:14)//'.DEL0'
    poinoe = deficu(1:16)//'.POINOE'
! ======================================================================
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apjeu, 'L', japjeu)
    call jeveuo(delt0, 'L', jdelt0)
    call jeveuo(mu, 'E', jmu)
    call jeveuo(poinoe, 'L', jpoi)
! ======================================================================
    do 10 iliac = 1, nbliac
        lliac = zi(jliac-1+iliac)
        jdecal = zi(jpoi+lliac-1)
        nbddl = zi(jpoi+lliac) - zi(jpoi+lliac-1)
        call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), zr( jdelt0),&
                    val)
        zr(jmu+iliac-1) = zr(japjeu+lliac-1) - val
10  continue
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
