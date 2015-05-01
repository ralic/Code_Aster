subroutine cusign(jcmpg, icmp, sign)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: jcmpg
    integer :: icmp
    real(kind=8) :: sign
!
! ----------------------------------------------------------------------
! ROUTINE APPELEE PAR : CUPREP
! ----------------------------------------------------------------------
!
! CETTE ROUTINE DONNE LE SIGNE A PLACER DEVANT LA COMPOSANTE DDL
! POUR LA THM, LE SIGNE EST POSITIF A CAUSE DE L'ECRITURE DE L'EQUATION
! HYDRAULIQUE.
! POUR LES AUTRES DDLS, IL EST NEGATIF
!
! IN  JCOEF  : ADRESSE JEVEUX DES COMPOSANTES
! IN  ICMP   : INDICE DE LA COMPOSANTE DU COEFFICIENT
! OUT COEF   : VALEUR DU SIGNE
!
!
!
!
!
    character(len=8) :: cmp
!
! ----------------------------------------------------------------------
!
    call jemarq()
! ----------------------------------------------------------------------
    cmp = zk8(jcmpg-1+icmp)
!
    if (cmp(1:4) .eq. 'PRE1') then
        sign = +1.d0
    else if (cmp(1:4).eq.'PRE2') then
        sign = +1.d0
    else
        sign = +1.d0
    endif
!
! ----------------------------------------------------------------------
    call jedema()
end subroutine
