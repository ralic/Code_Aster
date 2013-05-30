subroutine te0160(option, nomte)
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
!
    include 'asterfort/biline.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    include 'asterfort/matvec.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/verift.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - ELEMENT:  MECABL2
!      OPTION : 'FULL_MECA'   'RAPH_MECA'   'RIGI_MECA_TANG'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: nomres(2)
    integer :: icodre(2)
    real(kind=8) :: valres(2)
    integer :: nno, kp, i, j, imatuu, iret
    integer :: ipoids, ivf, igeom, imate, jcret
    real(kind=8) :: a, coef, coef1, coef2, demi
    real(kind=8) :: e, epsth
    real(kind=8) :: green, jacobi, nx, ytywpq(9), w(9)
!
!-----------------------------------------------------------------------
    integer :: icompo, idepla, ideplp, idfdk, imat, iyty, jefint, ivarip
    integer :: jgano, k, lsect, lsigma, ndim, nelyty, nnos
    integer :: nordre, npg
    real(kind=8) :: ec, preten, r8bid
!-----------------------------------------------------------------------
    demi = 0.5d0
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdk, jgano)
    call jevete('&INEL.CABPOU.YTY', 'L', iyty)
! --- 3 EFFORTS PAR NOEUD
    nordre = 3*nno
!
! --- PARAMETRES EN ENTREE
    call jevech('PCOMPOR', 'L', icompo)
    if (zk16(icompo+3) (1:9) .eq. 'COMP_INCR') then
        call u2mess('F', 'ELEMENTS3_36')
    endif
    if (zk16(icompo) (1:5) .ne. 'CABLE') then
        call u2mesk('F', 'ELEMENTS3_37', 1, zk16(icompo))
    endif
    if (zk16(icompo+1) .ne. 'GROT_GDEP') then
        call u2mesk('F', 'ELEMENTS3_38', 1, zk16(icompo+1))
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    nomres(1) = 'E'
    nomres(2) = 'EC_SUR_E'
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', 'ELAS', 0, '  ', r8bid,&
                1, nomres, valres, icodre, 1)
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', 'CABLE', 0, '  ', r8bid,&
                1, nomres(2), valres(2), icodre(2), 1)
    e = valres(1)
    ec = e*valres(2)
    call jevech('PCACABL', 'L', lsect)
    a = zr(lsect)
    preten = zr(lsect+1)
    call jevech('PDEPLMR', 'L', idepla)
    call jevech('PDEPLPR', 'L', ideplp)
! --- PARAMETRES EN SORTIE
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:14) .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imatuu)
    endif
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PVECTUR', 'E', jefint)
        call jevech('PCONTPR', 'E', lsigma)
        call jevech('PVARIPR', 'E', ivarip)
    endif
!
    do 20 i = 1, 3*nno
        w(i) = zr(idepla-1+i) + zr(ideplp-1+i)
20  end do
    do 70 kp = 1, npg
        call verift('RIGI', kp, 1, '+', zi(imate),&
                    'ELAS', 1, epsth, iret)
!
        k = (kp-1)*nordre*nordre
        jacobi = sqrt(biline(nordre,zr(igeom),zr(iyty+k),zr(igeom)))
!
        green = (biline(nordre, w, zr(iyty+k), zr(igeom))+ demi*biline( nordre, w, zr(iyty+k), w)&
                )/jacobi**2
!
        nx = e*a*green
        if (abs(nx) .lt. 1.d-6) then
            nx = preten
        else
            nx = nx - e*a*epsth
        endif
!
! ---    LE CABLE A UN MODULE PLUS FAIBLE EN COMPRESSION QU'EN TRACTION
!        LE MODULE DE COMPRESSION PEUT MEME ETRE NUL.
        if (nx .lt. 0.d0) then
            nx = nx*ec/e
            e = ec
        endif
!
        coef1 = e*a*zr(ipoids-1+kp)/jacobi**3
        coef2 = nx*zr(ipoids-1+kp)/jacobi
        call matvec(nordre, zr(iyty+k), 2, zr(igeom), w,&
                    ytywpq)
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:14) .eq. 'RIGI_MECA_TANG') then
            nelyty = iyty - 1 - nordre + k
            imat = imatuu - 1
            do 50 i = 1, nordre
                nelyty = nelyty + nordre
                do 40 j = 1, i
                    imat = imat + 1
                    zr(imat) = zr(imat) + coef1*ytywpq(i)*ytywpq(j) + coef2*zr(nelyty+j)
40              continue
50          continue
        endif
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
            coef = nx*zr(ipoids-1+kp)/jacobi
            do 60 i = 1, nordre
                zr(jefint-1+i) = zr(jefint-1+i) + coef*ytywpq(i)
60          continue
            zr(lsigma-1+kp) = nx
!
            zr(ivarip+kp-1) = 0.0d0
        endif
70  end do
!
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
end subroutine
