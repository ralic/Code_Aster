subroutine te0487(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT EN PRESSION SUIVEUSE
!          POUR LES PLAQUES ET COQUES
!
!          OPTION : 'CHAR_MECA_PRSU_F '
!                   'CHAR_MECA_SFCO3D '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
    character(len=24) :: valk
! ----------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: igeom, idepm, idepp, ipres, itemp, ires, icaco
    integer :: ino, iadzi, iazk24, ier, iret, jin
    real(kind=8) :: valpar(4), pr
    character(len=8) :: nomail, nompar(4)
!     ------------------------------------------------------------------
!
    if (nomte .eq. 'MEC3QU9H' .or. nomte .eq. 'MEC3TR7H') then
        call jevete('&INEL.'//nomte(1:8)//'.DESI', 'L', jin)
        nno = zi(jin)
    else
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfdx, jgano)
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLMR', 'L', idepm)
    call jevech('PDEPLPR', 'L', idepp)
    call jevech('PTEMPSR', 'L', itemp)
    call tecach('NNN', 'PPRESSF', 'L', iret, iad=ipres)
    if (ipres .eq. 0) then
        call jevech('PFFCO3D', 'L', ipres)
        call jevech('PCACOQU', 'L', icaco)
    endif
    call jevech('PVECTUR', 'E', ires)
!
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(4) = 'INST'
    valpar(4) = zr(itemp)
!
    do 10 ino = 0, nno-1
        valpar(1) = zr(igeom+3*ino )
        valpar(2) = zr(igeom+3*ino+1)
        valpar(3) = zr(igeom+3*ino+2)
        call fointe('FM', zk8(ipres), 4, nompar, valpar,&
                    pr, ier)
        if (pr .ne. 0.d0) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3)(1:8)
            valk = nomail
            call utmess('F', 'ELEMENTS4_92', sk=valk)
        endif
10  end do
!
end subroutine
