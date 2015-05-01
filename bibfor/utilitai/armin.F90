function armin(nomaz)
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
    real(kind=8) :: armin
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
    character(len=8) :: nomaz
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE - MAILLAGE
!
! CETTE FONCTION PERMET DE RECUPERER LA PLUS PETITE ARETE DU MAILLAGE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA  : NOM DU MODELE
! OUT ARMIN : TAILLE DE LA PLUS PETITE ARETE DU MAILLAGE
!
!
!
!
    character(len=8) :: noma, k8b
    character(len=19) :: nomt19
    character(len=24) :: para
    integer :: ibid, ier
    integer :: nbpar
    real(kind=8) :: r8b, arete
    complex(kind=8) :: cbid
!
! ----------------------------------------------------------------------
!
    call jemarq()
    r8b=0.d0
!
! --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE
!
    noma = nomaz
    call jeexin(noma//'           .LTNT', ier)
    if (ier .ne. 0) then
        call ltnotb(noma, 'CARA_GEOM', nomt19)
        nbpar = 0
        para = 'AR_MIN                  '
        call tbliva(nomt19, nbpar, ' ', [ibid], [r8b],&
                    [cbid], k8b, k8b, [r8b], para,&
                    k8b, ibid, arete, cbid, k8b,&
                    ier)
        if (ier .eq. 0) then
            armin = arete
        else
            call utmess('F', 'MODELISA2_13')
        endif
    else
        call utmess('F', 'MODELISA3_18')
    endif
!
    call jedema()
!
end function
