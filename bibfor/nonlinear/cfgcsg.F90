subroutine cfgcsg(resoco, neq, nbliai, tole, ninf)
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
#include "asterfort/caladu.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: neq, nbliai
    real(kind=8) :: tole
    real(kind=8) :: ninf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - GCP)
!
! CALCUL DU SOUS-GRADIENT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  TOLE   : TOLERANCE POUR DETECTER PRESSION NULLE
! OUT NINF   : NORME INFINIE DU RESIDU
!
!
!
!
    integer :: iliai, nbddl, jdecal
    real(kind=8) :: jeuinc, jeuold, jeunew, ssgrad
    character(len=19) :: mu
    integer :: jmu
    character(len=19) :: sgradp
    integer :: jsgrap
    character(len=24) :: apcoef, apddl, appoin
    integer :: japcoe, japddl, japptr
    character(len=24) :: jeuite
    integer :: jjeuit
    character(len=19) :: ddeplc
    integer :: jddepc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES STRUCTURES DE DONNEES DE CONTACT
!
    mu = resoco(1:14)//'.MU'
    appoin = resoco(1:14)//'.APPOIN'
    apcoef = resoco(1:14)//'.APCOEF'
    apddl = resoco(1:14)//'.APDDL'
    jeuite = resoco(1:14)//'.JEUITE'
    sgradp = resoco(1:14)//'.SGDP'
    call jeveuo(mu, 'L', jmu)
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(sgradp, 'E', jsgrap)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
!
    ddeplc = resoco(1:14)//'.DELC'
    call jeveuo(ddeplc(1:19)//'.VALE', 'L', jddepc)
!
! --- CALCUL DU SOUS-GRADIENT
!
    do 10 iliai = 1, nbliai
        jdecal = zi(japptr+iliai-1)
        nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
        jeuold = zr(jjeuit+3*(iliai-1)+1-1)
        call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), zr(jddepc),&
                    jeuinc)
        jeunew = jeuold - jeuinc
        ssgrad = -jeunew
        zr(jsgrap-1+iliai) = ssgrad
10  end do
!
! --- PROJECTION DU SOUS-GRADIENT
!
    do 15 iliai = 1, nbliai
        ssgrad = zr(jsgrap-1+iliai)
        if (zr(jmu+iliai-1) .le. tole) then
            zr(jsgrap-1+iliai) = max(ssgrad,0.d0)
        endif
15  end do
!
! --- NORME INFINIE DU RESIDU
!
    ninf = 0.d0
    do 20 iliai = 1, nbliai
        ninf = max(abs(zr(jsgrap-1+iliai)),ninf)
20  end do
!
    call jedema()
!
end subroutine
