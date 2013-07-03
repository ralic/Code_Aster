subroutine nmdcrg(depart, iterat, vresi, xa0, xa1,&
                  xdet)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: depart, iterat
    real(kind=8) :: vresi(*)
    real(kind=8) :: xa0, xa1, xdet
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (EVENEMENTS)
!
! CALCUL DE L'EXTRAPOLATION SUR LES RESIDUS
!
! ----------------------------------------------------------------------
!
!
! EXTRAPOLATION LINEAIRE (XA0 + ITER*XA1) / XDET
!
! ON DONNE UN POIDS DOUBLE AU 3 DERNIERS POINTS CELA REVIENT
! A AJOUTER DES POINTS => MEILLEURE EXTRAPOLATION
!
! IN  DEPART : DEBUT DE L'EXTRAPOLATION
! IN  ITERAT : FIN DE L'EXTRAPOLATION
! IN  VRESI  : LISTE DES VALEURS DU RESIDU ACTUEL [0,ITERAT]
! OUT XA0    : VALEUR DE L'EXTRAPOLATION
! OUT XA1    : VALEUR DE L'EXTRAPOLATION
! OUT XDET   : VALEUR DE L'EXTRAPOLATION
!
!
!
!
    real(kind=8) :: zero, un, deux
    parameter   (zero=0.0d0, un=1.0d0, deux=2.0d0)
!
    integer :: i
    real(kind=8) :: sx, sy, sx2, syx
    real(kind=8) :: xn, xx
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    sx = zero
    sy = zero
    sx2 = zero
    syx = zero
    xn = zero
    xa0 = zero
    xa1 = zero
    xdet = zero
!
! --- CALCUL DE L'EXTRAPOLATION
!
    do 110 i = depart, iterat
        xx = log(vresi(i+1))
        if (i .gt. (iterat - 3)) then
            xn = xn + deux
            sx = sx + deux*xx
            sy = sy + deux*i
            sx2 = sx2 + deux*(xx**2)
            syx = syx + deux*xx*i
        else
            xn = xn + un
            sx = sx + xx
            sy = sy + i
            sx2 = sx2 + xx**2
            syx = syx + xx*i
        endif
110  end do
    xdet = -sx**2 + sx2*xn
    xa0 = sx2*sy - sx*syx
    xa1 = -(sx*sy) + syx*xn
!
    call jedema()
end subroutine
