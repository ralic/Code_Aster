subroutine te0503(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/connec.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/matini.h"
#include "asterfort/teattr.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'RIGI_THER_COET_R'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: poids, r, nx, ny, mrigt(9, 9), coorse(18)
    integer :: nno, nnos, ndim, kp, npg, ipoids, ivf, idfde, jgano, igeom
    integer :: imattt, i, j, ij, l, li, lj, icoefh, itemps
    integer :: c(6, 9), ise, nse, nnop2, ibid
    aster_logical :: laxi
    character(len=8) :: elrefe, alias8
!
!
    call elref1(elrefe)
!
    if (lteatt('LUMPE','OUI')) then
        call teattr('S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'SE3') elrefe='SE2'
    endif
!
    call elrefe_info(elrefe=elrefe, fami='RIGI', ndim=ndim, nno=nno, nnos=nnos,&
                     npg=npg, jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOEFHR', 'L', icoefh)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PMATTTR', 'E', imattt)
!
    call connec(nomte, nse, nnop2, c)
!
    call matini(9, 9, 0.d0, mrigt)
!
! --- CALCUL ISO-P2 : BOUCLE SUR LES SOUS-ELEMENTS -------
!
    do 90 ise = 1, nse
!
        do 40 i = 1, nno
            do 30 j = 1, 2
                coorse(2* (i-1)+j) = zr(igeom-1+2* (c(ise,i)-1)+j)
 30         continue
 40     continue
!
        do 80 kp = 1, npg
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        coorse, nx, ny, poids)
            if (laxi) then
                r = 0.d0
                do 50 i = 1, nno
                    l = (kp-1)*nno + i
                    r = r + coorse(2* (i-1)+1)*zr(ivf+l-1)
 50             continue
                poids = poids*r
            endif
            ij = imattt - 1
            do 70 i = 1, nno
                li = ivf + (kp-1)*nno + i - 1
!
                do 60 j = 1, i
                    lj = ivf + (kp-1)*nno + j - 1
                    ij = ij + 1
                    mrigt(c(ise,i),c(ise,j)) = mrigt(&
                                               c(ise, i),&
                                               c(ise, j) ) + poids*zr(li)*zr(lj)*zr(icoefh&
                                               )
 60             continue
 70         continue
 80     continue
 90 end do
!
! MISE SOUS FORME DE VECTEUR
!
    ij = imattt - 1
    do 110 i = 1, nnop2
        do 100 j = 1, i
            ij = ij + 1
            zr(ij) = mrigt(i,j)
100     continue
110 end do
!
end subroutine
