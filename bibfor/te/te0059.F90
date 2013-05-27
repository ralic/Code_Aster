subroutine te0059(option, nomte)
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
!.......................................................................
    implicit none
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU TERME D'ECHANGE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_THER_TEXT_R'
!          OPTION : 'CHAR_THER_RAYO_R'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    include 'jeveux.h'
!
    include 'asterc/r8t0.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, tem, theta
    integer :: ipoids, ivf, idfdx, idfdy, igeom
    integer :: ndim, nno, ipg, npg1, ivectt, itext, iech, iray
    integer :: idec, jdec, kdec, ldec, nnos, jgano
    real(kind=8) :: sigma, epsil, tpinf, tz0
!
!
!-----------------------------------------------------------------------
    integer :: i, ino, itemp, itemps, j, jno
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    tz0 = r8t0()
!
    if (option(11:14) .eq. 'TEXT') then
        call jevech('PCOEFHR', 'L', iech)
        call jevech('PT_EXTR', 'L', itext)
    else if (option(11:14).eq.'RAYO') then
        call jevech('PRAYONR', 'L', iray)
        sigma = zr(iray)
        epsil = zr(iray+1)
        tpinf = zr(iray+2)
    endif
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PVECTTR', 'E', ivectt)
!
    theta = zr(itemps+2)
!
    do 10 i = 1, nno
        zr(ivectt+i-1) = 0.0d0
10  end do
!
!    CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do 1 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 2 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
 2      continue
 1  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!   CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
!
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
!
102          continue
!
!   CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
        tem = 0.d0
        do 103 i = 1, nno
            ldec = (ipg-1)*nno
            tem = tem + zr(itemp+i-1) * zr(ivf+ldec+i-1)
103      continue
        if (option(11:14) .eq. 'TEXT') then
            do 104 i = 1, nno
                zr(ivectt+i-1) = zr(ivectt+i-1) + jac * zr(ipoids+ipg- 1) * zr(ivf+ldec+i-1) * zr&
                                 &(iech) * ( zr(itext) - ( 1.0d0-theta)*tem )
104          continue
        else if (option(11:14).eq.'RAYO') then
            do 105 i = 1, nno
                zr(ivectt+i-1) = zr(ivectt+i-1) + jac * zr(ipoids+ipg- 1) * zr(ivf+ldec+i-1) * si&
                                 &gma * epsil * ( (tpinf+tz0) **4 - (1.0d0-theta) * (tem+tz0)**4 &
                                 &)
105          continue
        endif
!
101  continue
end subroutine
