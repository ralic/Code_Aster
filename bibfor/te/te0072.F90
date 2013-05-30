subroutine te0072(option, nomte)
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
    include 'jeveux.h'
    include 'asterc/r8t0.h'
    include 'asterfort/connec.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/teattr.h'
    include 'asterfort/vff2dn.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_TEXT_R'
!                          OPTION : 'CHAR_THER_RAYO_R'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: elrefe, alias8
    real(kind=8) :: poids, r, nx, ny, tpg, theta
    real(kind=8) :: coorse(18), vectt(9)
    integer :: ndim, nnos, nno, kp, npg, ipoids, ivf, idfde, jgano, igeom
    integer :: ivectt, i, j, l, li, icoefh, itex, iray, itemp, itemps
    integer :: nnop2, c(6, 9), ise, nse, ibid
    real(kind=8) :: sigma, epsil, tpinf, tz0
    logical :: laxi
!
!
    call elref1(elrefe)
!
    if (lteatt(' ','LUMPE','OUI')) then
        call teattr(' ', 'S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'SE3') elrefe='SE2'
    endif
!
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    tz0 = r8t0()
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    if (option(11:14) .eq. 'TEXT') then
        call jevech('PCOEFHR', 'L', icoefh)
        call jevech('PT_EXTR', 'L', itex)
    else if (option(11:14).eq.'RAYO') then
        call jevech('PRAYONR', 'L', iray)
        sigma = zr(iray)
        epsil = zr(iray+1)
        tpinf = zr(iray+2)
    endif
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTTR', 'E', ivectt)
!
    theta = zr(itemps+2)
!
    call connec(nomte, nse, nnop2, c)
!
    do 10 i = 1, nnop2
        vectt(i) = 0.d0
10  end do
!
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do 80 ise = 1, nse
!
        do 30 i = 1, nno
            do 20 j = 1, 2
                coorse(2* (i-1)+j) = zr(igeom-1+2* (c(ise,i)-1)+j)
20          continue
30      continue
!
        do 70 kp = 1, npg
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        coorse, nx, ny, poids)
            r = 0.d0
            tpg = 0.d0
            do 40 i = 1, nno
                l = (kp-1)*nno + i
                r = r + coorse(2* (i-1)+1)*zr(ivf+l-1)
                tpg = tpg + zr(itemp-1+c(ise,i))*zr(ivf+l-1)
40          continue
            if (laxi) poids = poids*r
            if (option(11:14) .eq. 'TEXT') then
                do 50 i = 1, nno
                    li = ivf + (kp-1)*nno + i - 1
                    vectt(c(ise,i)) = vectt(&
                                      c(ise,i)) + poids*zr(li)* zr(icoefh)* (zr(itex)- (1.0d0-the&
                                      &ta)*tpg&
                                      )
50              continue
            else if (option(11:14).eq.'RAYO') then
                do 60 i = 1, nno
                    li = ivf + (kp-1)*nno + i - 1
                    vectt(c(ise,i)) = vectt(&
                                      c(ise,i)) + poids*zr(li)* sigma*epsil* ((tpinf+tz0)**4- (1.&
                                      &0d0-theta)* (tpg+tz0)**4&
                                      )
60              continue
            endif
70      continue
80  end do
!
    do 90 i = 1, nnop2
        zr(ivectt-1+i) = vectt(i)
90  end do
!
end subroutine
