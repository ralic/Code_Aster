subroutine te0397(option, nomte)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/dfdm1d.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    character(len=16) :: option, nomte
! ......................................................................
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          COQUE 1D
!                          OPTION : 'CHAR_MECA_PRES_R  '
!                          ELEMENT: MECXSE3,METCSE3,METDSE3
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: nno, nnos, jgano, ndim, nddl, kp, npg, ipoids, ivf, idfdk, igeom
    integer :: ivectu, k, i, l, ipres, ier, iadzi, iazk24, itemps
    real(kind=8) :: valpar(4), poids, r, fx, fy, f3, nx, ny, cour, dfdx(3), pr
    character(len=8) :: nompar(4), nomail, elrefe
    character(len=24) :: valk
! DEB ------------------------------------------------------------------
!
    call elref1(elrefe)
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdk, jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
    nddl = 3
!
    if (option .eq. 'CHAR_MECA_PRES_R') then
!          ------------------------------
        call jevech('PPRESSR', 'L', ipres)
!
        do 30 kp = 1, npg
            k = (kp-1)*nno
            call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                        cour, poids, nx, ny)
            r = 0.d0
            fx = 0.d0
            fy = 0.d0
            do 10 i = 1, nno
                l = (kp-1)*nno + i
!-----------------------------------------------------
!              LE SIGNE MOINS CORRESPOND A LA CONVENTION :
!                 UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
!-----------------------------------------------------
                f3 = -zr(ipres+i-1)
                fx = fx + nx*f3*zr(ivf+l-1)
                fy = fy + ny*f3*zr(ivf+l-1)
                r = r + zr(igeom+2* (i-1))*zr(ivf+l-1)
10          continue
            if (nomte .eq. 'MECXSE3') poids = poids*r
            do 20 i = 1, nno
                l = (kp-1)*nno + i
                zr(ivectu+nddl* (i-1)) = zr(ivectu+nddl* (i-1)) + fx*zr(ivf+l-1 )*poids
                zr(ivectu+nddl* (i-1)+1) = zr( ivectu+nddl* (i-1)+1 ) + fy*zr(ivf+l-1 )*poids
20          continue
30      continue
!
    else if (option.eq.'CHAR_MECA_PRES_F') then
!              ------------------------------
        call jevech('PPRESSF', 'L', ipres)
        call jevech('PTEMPSR', 'L', itemps)
        valpar(4) = zr(itemps)
        nompar(4) = 'INST'
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        do 40 i = 0, nno - 1
            valpar(1) = zr(igeom+3*i)
            valpar(2) = zr(igeom+3*i+1)
            valpar(3) = zr(igeom+3*i+2)
            call fointe('FM', zk8(ipres), 4, nompar, valpar,&
                        pr, ier)
            if (pr .ne. 0.d0) then
                call tecael(iadzi, iazk24)
                nomail = zk24(iazk24-1+3) (1:8)
                valk = nomail
                call u2mesg('F', 'ELEMENTS4_92', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
40      continue
!
    endif
!
end subroutine
