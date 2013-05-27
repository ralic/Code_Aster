subroutine te0275(option, nomte)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref2.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/vff2dn.h'
    character(len=16) :: option, nomte
! ......................................................................
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'RESI_THER_PARO_R'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nno, nnos, jgano, ndim, kp, npg, ipoids, ivf, idfde, igeom
    integer :: iveres, i, l, li, ihechp, itemps, itemp, nbelr
    real(kind=8) :: poids, poids1, poids2, coefh
    real(kind=8) :: r1, r2, nx, ny, tpg, theta
    character(len=8) :: lirefe(2)
    logical :: laxi
!     ------------------------------------------------------------------
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    call elref2(nomte, 2, lirefe, nbelr)
    call assert(nbelr.eq.2)
    call elref4(lirefe(2), 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PHECHPR', 'L', ihechp)
    coefh = zr(ihechp)
    call jevech('PTEMPEI', 'L', itemp)
    call jevech('PRESIDU', 'E', iveres)
!
    theta = zr(itemps+2)
!
    do 30 kp = 1, npg
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids1)
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom+2*nno), nx, ny, poids2)
        r1 = 0.d0
        r2 = 0.d0
        tpg = 0.d0
        do 10 i = 1, nno
            l = (kp-1)*nno + i
            r1 = r1 + zr(igeom+2*i-2)*zr(ivf+l-1)
            r2 = r2 + zr(igeom+2* (nno+i)-2)*zr(ivf+l-1)
            tpg = tpg + (zr(itemp+nno+i-1)-zr(itemp+i-1))*zr(ivf+l-1)
10      continue
        if (laxi) then
            poids1 = poids1*r1
            poids2 = poids2*r2
        endif
        poids = (poids1+poids2)/2
!CDIR$ IVDEP
        do 20 i = 1, nno
            li = ivf + (kp-1)*nno + i - 1
            zr(iveres+i-1) = zr(iveres+i-1) - poids*zr(li)*coefh* theta*tpg
            zr(iveres+i-1+nno) = zr(iveres+i-1+nno) + poids*zr(li)* coefh*theta*tpg
20      continue
30  end do
end subroutine
