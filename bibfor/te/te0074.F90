subroutine te0074(option, nomte)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/connec.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/teattr.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_TH_FLUXNR  '
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: elrefe, alias8
    real(kind=8) :: poids, r, nx, ny
    real(kind=8) :: coorse(18), vectt(9)
    integer :: nno, nnos, ndim, kp, npg, ipoids, ivf, idfde, jgano, igeom
    integer :: ivectt, i, j, l, li, iflu, nnop2, c(6, 9), ise, nse, ibid
    aster_logical :: laxi
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
    call jevech('PFLUXNR', 'L', iflu)
    call jevech('PVECTTR', 'E', ivectt)
!
    call connec(nomte, nse, nnop2, c)
!
    do 10 i = 1, nnop2
        vectt(i) = 0.d0
 10 end do
!
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do 70 ise = 1, nse
!
        do 30 i = 1, nno
            do 20 j = 1, 2
                coorse(2* (i-1)+j) = zr(igeom-1+2* (c(ise,i)-1)+j)
 20         continue
 30     continue
!
        do 60 kp = 1, npg
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        coorse, nx, ny, poids)
            if (laxi) then
                r = 0.d0
                do 40 i = 1, nno
                    l = (kp-1)*nno + i
                    r = r + coorse(2* (i-1)+1)*zr(ivf+l-1)
 40             continue
                poids = poids*r
            endif
            do 50 i = 1, nno
                li = ivf + (kp-1)*nno + i - 1
                vectt(c(ise,i)) = vectt(c(ise,i)) + poids*zr(li)*zr( iflu)
 50         continue
 60     continue
!
 70 end do
!
    do 80 i = 1, nnop2
        zr(ivectt-1+i) = vectt(i)
 80 end do
!
end subroutine
