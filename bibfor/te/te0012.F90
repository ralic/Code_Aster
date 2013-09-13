subroutine te0012(option, nomte)
    implicit none
!.......................................................................
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
!     BUT: CALCUL DES MATRICES DE MASSE ELEMENTAIRES EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'MASS_MECA'
!          OPTION : 'MASS_MECA_DIAG'
!          OPTION : 'MASS_MECA_EXPLI'
!          OPTION : 'M_GAMMA'
!          OPTION : 'ECIN_ELEM'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/grdthm.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/vecma.h"
#include "blas/ddot.h"
!
    integer :: icodre(1)
!
    character(len=16) :: nomte, option, phenom
    character(len=4) :: fami
    character(len=1) :: stopz(3)
    real(kind=8) :: a(3, 3, 27, 27), matp(81, 81), matv(3321)
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, rho
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, nno, kp, i, j, k, imatuu, iacce, ivect
    integer :: ijkl, ik, l, ndim, npg, nddl, nvec
    integer :: n1, n2, i2, j2, k2
    integer :: idiag, nnos, iret
    integer :: idepl, ivite, iecin, ifreq
    real(kind=8) :: trace, alfa, wgt, masvit(81), masdep(81)
    real(kind=8) :: vect1(81), vect2(81)
    integer :: mecani(5), press1(7), press2(7), tempe(5), ibi, idec
!.......................................................................
!
    fami='MASS'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    nddl = 3*nno
    nvec = nddl* (nddl+1)/2
    press1(1) = 0
    press2(1) = 0
    tempe(1) = 0
    call grdthm(nomte, .false., .false., 3, mecani,&
                press1, press2, tempe, ibi, ibi,&
                ibi, ibi, ibi, ibi)
    idec = press1(1) + press2(1) + tempe(1)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
    do 50 k = 1, 3
        do 40 l = 1, 3
            do 30 i = 1, nno
                do 20 j = 1, i
                    a(k,l,i,j) = 0.0d0
20              continue
30          continue
40      continue
50  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 90 kp = 1, npg
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
        call rcvalb(fami, kp, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', 0.d0,&
                    1, 'RHO', rho, icodre(1), 1)
        do 80 i = 1, nno
            do 70 j = 1, i
                a(1,1,i,j) = a(1,1,i,j) + rho*poids*zr(ivf+l+i-1)* zr(ivf+l+j-1)
70          continue
80      continue
90  end do
!
    do 110 i = 1, nno
        do 100 j = 1, i
            a(2,2,i,j) = a(1,1,i,j)
            a(3,3,i,j) = a(1,1,i,j)
100      continue
110  end do
!
    if (option .eq. 'MASS_MECA') then
!
        call jevech('PMATUUR', 'E', imatuu)
        do 410 k = 1, nvec
            matv(k) = 0.0d0
410      continue
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
        do 150 k = 1, 3
            do 140 l = 1, 3
                do 130 i = 1, nno
                    ik = ((3*i+k-4)* (3*i+k-3))/2
                    do 120 j = 1, i
                        ijkl = ik + 3* (j-1) + l
                        matv(ijkl) = a(k,l,i,j)
120                  continue
130              continue
140          continue
150      continue
        if (idec .eq. 0) then
            do 400 i = 1, nvec
                zr(imatuu+i-1) = matv(i)
400          continue
        else
            do 401 k = 1, nno
                do 402 n1 = 1, 3
                    i = 3*k+n1-3
                    if (k .le. nnos) then
                        i2 = i+idec*(k-1)
                    else
                        i2 = i+idec*nnos
                    endif
                    do 403 l = 1, nno
                        do 404 n2 = 1, 3
                            j = 3*l+n2-3
                            if (j .gt. i) goto 405
                            if (l .le. nnos) then
                                j2 = j+idec*(l-1)
                            else
                                j2 = j+idec*nnos
                            endif
                            zr(imatuu+i2*(i2-1)/2+j2-1) = matv(i*(i-1) /2+j)
404                      continue
403                  continue
405                  continue
402              continue
401          continue
        endif
!
        else if (option.eq.'MASS_MECA_DIAG' .or.&
     &         option.eq.'MASS_MECA_EXPLI' ) then
!
        call jevech('PMATUUR', 'E', imatuu)
!
!-- CALCUL DE LA MASSE DE L'ELEMENT
!
        wgt = a(1,1,1,1)
        do 170 i = 2, nno
            do 160 j = 1, i - 1
                wgt = wgt + 2*a(1,1,i,j)
160          continue
            wgt = wgt + a(1,1,i,i)
170      continue
!
!-- CALCUL DE LA TRACE EN TRANSLATION SUIVANT X
!
        trace = 0.d0
        do 180 i = 1, nno
            trace = trace + a(1,1,i,i)
180      continue
!
!-- CALCUL DU FACTEUR DE DIAGONALISATION
!
        alfa = wgt/trace
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
        k = 0
        do 200 j = 1, nno
            do 190 i = 1, 3
                k = k + 1
                if (idec .eq. 0) then
                    idiag = k* (k+1)/2
                else
                    if (j .le. nnos) then
                        k2 = k+idec*(j-1)
                    else
                        k2 = k+idec*nnos
                    endif
                    idiag = k2* (k2+1)/2
                endif
                zr(imatuu+idiag-1) = a(i,i,j,j)*alfa
190          continue
200      continue
!
    else if (option.eq.'M_GAMMA') then
!
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
        do 210 k = 1, nvec
            matv(k) = 0.0d0
210      continue
        do 250 k = 1, 3
            do 240 l = 1, 3
                do 230 i = 1, nno
                    ik = ((3*i+k-4)* (3*i+k-3))/2
                    do 220 j = 1, i
                        ijkl = ik + 3* (j-1) + l
                        matv(ijkl) = a(k,l,i,j)
220                  continue
230              continue
240          continue
250      continue
        call vecma(matv, nvec, matp, nddl)
        if (idec .eq. 0) then
            call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
        else
            do 320 k = 1, nddl
                vect1(k) = 0.0d0
                vect2(k) = 0.0d0
320          continue
            do 311 k = 1, nno
                do 312 n1 = 1, 3
                    i = 3*k+n1-3
                    if (k .le. nnos) then
                        i2 = i+idec*(k-1)
                    else
                        i2 = i+idec*nnos
                    endif
                    vect1(i) = zr(iacce+i2-1)
312              continue
311          continue
            call pmavec('ZERO', nddl, matp, vect1, vect2)
            do 313 k = 1, nno
                do 314 n1 = 1, 3
                    i = 3*k+n1-3
                    if (k .le. nnos) then
                        i2 = i+idec*(k-1)
                    else
                        i2 = i+idec*nnos
                    endif
                    zr(ivect+i2-1) = vect2(i)
314              continue
313          continue
        endif
!
! OPTION ECIN_ELEM : CALCUL DE L'ENERGIE CINETIQUE
!
    else if (option.eq.'ECIN_ELEM') then
        stopz(1)='O'
        stopz(2)='N'
        stopz(3)='O'
        call tecach(stopz, 'PVITESR', 'L', 1, ivite,&
                    iret)
! IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        if (iret .eq. 0) then
            call jevech('PENERCR', 'E', iecin)
            do 260 k = 1, nvec
                matv(k) = 0.0d0
260          continue
            do 300 k = 1, 3
                do 290 l = 1, 3
                    do 280 i = 1, nno
                        ik = ((3*i+k-4)* (3*i+k-3))/2
                        do 270 j = 1, i
                            ijkl = ik + 3* (j-1) + l
                            matv(ijkl) = a(k,l,i,j)
270                      continue
280                  continue
290              continue
300          continue
            call vecma(matv, nvec, matp, nddl)
            call pmavec('ZERO', nddl, matp, zr(ivite), masvit)
            zr(iecin) = .5d0*ddot(nddl,zr(ivite),1,masvit,1)
        else
            call tecach(stopz, 'PDEPLAR', 'L', 1, idepl,&
                        iret)
            if (iret .eq. 0) then
                call jevech('POMEGA2', 'L', ifreq)
                call jevech('PENERCR', 'E', iecin)
                do 261 k = 1, nvec
                    matv(k) = 0.0d0
261              continue
                do 301 k = 1, 3
                    do 291 l = 1, 3
                        do 281 i = 1, nno
                            ik = ((3*i+k-4)* (3*i+k-3))/2
                            do 271 j = 1, i
                                ijkl = ik + 3* (j-1) + l
                                matv(ijkl) = a(k,l,i,j)
271                          continue
281                      continue
291                  continue
301              continue
                call vecma(matv, nvec, matp, nddl)
                call pmavec('ZERO', nddl, matp, zr(idepl), masdep)
                zr(iecin) = .5d0*ddot(nddl,zr(idepl),1,masdep,1)*zr( ifreq)
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
!
    else
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
!
end subroutine
