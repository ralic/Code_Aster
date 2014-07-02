subroutine chsfus(nbchs, lichs, lcumul, lcoefr, lcoefc,&
                  lcoc, base, chs3)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
    implicit none
#include "asterf_types.h"
#include "asterfort/cesfus.h"
#include "asterfort/cnsfus.h"
#include "asterfort/exisd.h"
#include "asterfort/utmess.h"
    integer :: nbchs
    character(len=*) :: lichs(nbchs), chs3, base
    aster_logical :: lcumul(nbchs), lcoc
    real(kind=8) :: lcoefr(nbchs)
    complex(kind=8) :: lcoefc(nbchs)
! ---------------------------------------------------------------------
! BUT: FUSIONNER UNE LISTE DE CHAMP_S (CHAM_ELEM_S OU CHAM_NO_S)
!      POUR EN FORMER 1 CHAMP_S
! ---------------------------------------------------------------------
!     ARGUMENTS:
! NBCHS   IN       I      : NOMBRE DE CHAMP_S A FUSIONNER
! LICHS   IN/JXIN  V(K19) : LISTE DES SD CHAMP_S A FUSIONNER
! LCUMUL  IN       V(L)   : V(I) =.TRUE. => ON ADDITIONNE LE CHAMP I
!                         : V(I) =.FALSE. => ON SURCHARGE LE CHAMP I
! LCOEFR  IN       V(R)   : LISTE DES COEF. MULT. DES VALEURS DES CHAMPS
! LCOEFC  IN       V(C)   : LISTE DES COEF. MULT. DES VALEURS DES CHAMPS
! LCOC    IN       L      : =TRUE SI COEF COMPLEXE
! CHS3    IN/JXOUT K19    : SD CHAMP_S RESULTAT
! BASE    IN       K1     : BASE DE CREATION POUR CHS3 : G/V/L
!
! REMARQUES :
!
!- LES CHAMP_S DE LICHS DOIVENT ETRE DE LA MEME GRANDEUR,S'APPUYER
!  SUR LE MEME MAILLAGE ET ETRE DE MEME TYPE (NOEU/CART/ELGA/ELNO).
!  POUR LES CHAM_ELEM_S :
!  DANS TOUS LES CHAM_ELEM_S, CHAQUE MAILLE DOIT AVOIR LE MEME
!  NOMBRE DE POINTS (NOEUD OU GAUSS) ET LE MEME NOMBRE DE SOUS-POINTS.
!
!- L'ORDRE DES CHAMP_S DANS LICHS EST IMPORTANT :
!  LES CHAMP_S SE SURCHARGENT LES UNS LES AUTRES
!
!- ON PEUT APPELER CETTE ROUTINE MEME SI CHS3 APPARTIENT
!  A LA LISTE LICHS (CHAMP_S IN/OUT)
! ---------------------------------------------------------------------
!
    character(len=19) :: chs
    integer :: i1, i2, k, j1, j2
!
    j1 = 0
    j2 = 0
    do 10 k = 1, nbchs
        chs = lichs(k)
        call exisd('CHAM_NO_S', chs, i1)
        call exisd('CHAM_ELEM_S', chs, i2)
        j1 = max(j1,i1)
        j2 = max(j2,i2)
 10 end do
    if (j1*j2 .ne. 0) then
        call utmess('F', 'CALCULEL_99')
    endif
!
    if (j1 .gt. 0) call cnsfus(nbchs, lichs, lcumul, lcoefr, lcoefc,&
                               lcoc, base, chs3)
    if (j2 .gt. 0) call cesfus(nbchs, lichs, lcumul, lcoefr, lcoefc,&
                               lcoc, base, chs3)
end subroutine
