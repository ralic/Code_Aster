subroutine te0420(option, nomte)
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
!.......................................................................
!
!     BUT: CALCUL DES NIVEAUX DE PRESSION EN DB
!          ELEMENTS ISOPARAMETRIQUES 3D ET 3D_MIXTE
!
!          OPTION : 'PRME_ELNO'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/jevech.h"
#include "asterfort/tecael.h"
    character(len=16) :: nomte, option
!
!
!
    integer :: iadzi, iazk24
    integer :: nno, ino, ipdeb, ipres, idino, ipino
!
!
    call jevech('PPRME_R', 'E', ipdeb)
    call jevech('PDEPLAC', 'L', ipres)
!
    call tecael(iadzi, iazk24)
    nno = zi(iadzi+1)
!
    do 10 ino = 1, nno
        idino = ipdeb + ino - 1
        ipino = ipres + 2* (ino-1)
        zr(idino) = 20.d0*log10(abs(zc(ipino))/2.d-5)
10  end do
!
end subroutine
