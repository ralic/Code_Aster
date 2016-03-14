subroutine cfatmu(neq, nbliac, sdcont_solv)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calatm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: neq
    integer :: nbliac
    character(len=24) :: sdcont_solv
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! CALCUL DE ATMU - VECTEUR DES FORCES DE CONTACT
!
!
! ----------------------------------------------------------------------
!
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!                'E': RESOCO(1:14)//'.ATMU'
!
!
!
    integer :: iliac, lliac, jdecal, compts
    integer :: nbddl, kk
    character(len=19) :: liac, mu, atmu
    integer :: jliac, jmu, jatmu
    character(len=24) :: appoin, apddl, apcoef
    integer :: japptr, japddl, japcoe
!
    call jemarq()
!
    liac = sdcont_solv(1:14)//'.LIAC'
    mu = sdcont_solv(1:14)//'.MU'
    atmu = sdcont_solv(1:14)//'.ATMU'
    appoin = sdcont_solv(1:14)//'.APPOIN'
    apddl = sdcont_solv(1:14)//'.APDDL'
    apcoef = sdcont_solv(1:14)//'.APCOEF'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(mu, 'L', jmu)
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(atmu, 'E', jatmu)
    compts = 0
!
    do kk = 1, neq
        zr(jatmu+kk-1) = 0.0d0
    end do
    do iliac = 1, nbliac 
        lliac = zi(jliac +iliac-1)
        jdecal = zi(japptr+lliac-1)
        nbddl = zi(japptr+lliac ) - zi(japptr+lliac-1)
        compts = compts + 1
        call calatm(neq, nbddl, zr(jmu-1+compts), zr(japcoe+jdecal), zi( japddl+jdecal),&
                    zr(jatmu))
    end do
!
    call jedema()
!
end subroutine
