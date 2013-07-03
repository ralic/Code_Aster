subroutine te0060(option, nomte)
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
!----------------------------------------------------------------------
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU TERME D'ECHANGE (FONCTION)
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_THER_TEXT_F/R'
!                   'CHAR_THER_RAYO_F/R'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
#include "jeveux.h"
!
#include "asterc/r8t0.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
    character(len=8) :: nompar(4)
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, theta
    real(kind=8) :: valpar(4), xx, yy, zz, tem, echn, echnp1, texn, texnp1
    real(kind=8) :: sigm1, sigmn, eps1, epsn, tpf1, tpfn, tz0
    integer :: ipoids, ivf, idfdx, idfdy, igeom, itemps, i, ndim, nno, ipg, npg1
    integer :: ivectt, itext, iech, iray, ino, idec, jdec, kdec, ldec, itemp
    integer :: jno, j, ier, jgano, nnos
    logical :: ltext
!
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    tz0 = r8t0()
!
! INITS.
    if (option(11:14) .eq. 'TEXT') then
        ltext = .true.
    else if (option(11:14).eq.'RAYO') then
        ltext = .false.
    else
!C OPTION DE CALCUL INVALIDE
        call assert(.false.)
    endif
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
    if (ltext) then
! CHAR_.._TEXT : 2 TYPES DE CALCUL
!
        call jevech('PTEMPER', 'L', itemp)
        call jevech('PCOEFHF', 'L', iech)
        call jevech('PT_EXTF', 'L', itext)
!
    else
! CHAR_..._RAYO: 4 TYPES DE CALCUL
!
! CHAMP DE RAYONNEMENT
        call jevech('PRAYONF', 'L', iray)
        call jevech('PTEMPER', 'L', itemp)
! FIN DU IF LTEXT
    endif
!
! TRONC COMMUN
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTTR', 'E', ivectt)
!
!====
! 1.3 PREALABLES LIES AUX CALCULS
!====
    theta = zr(itemps+2)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(4) = 'INST'
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
!====
! 2. CALCULS TERMES DE MASSE
!====
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!    CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx=nx+zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(&
                i,j)
                ny=ny+zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(&
                i,j)
                nz=nz+zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(&
                i,j)
102          continue
        jac = sqrt(nx*nx + ny*ny + nz*nz)
        tem = 0.d0
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
!
        if (itemp .ne. 0) then
            do 104 i = 1, nno
! CALCUL DE T-
                tem = tem + zr(itemp+i-1) * zr(ivf+ldec+i-1)
104          continue
        endif
!
        do 106 i = 1, nno
! CALCUL DE LA POSITION GEOMETRIQUE DU PT DE GAUSS
            xx = xx + zr(igeom+3*i-3) * zr(ivf+ldec+i-1)
            yy = yy + zr(igeom+3*i-2) * zr(ivf+ldec+i-1)
            zz = zz + zr(igeom+3*i-1) * zr(ivf+ldec+i-1)
106      continue
!
        valpar(1) = xx
        valpar(2) = yy
        valpar(3) = zz
        valpar(4) = zr(itemps)
!
!====
! 2.1 OPTION CHAR_THER_TEXT_F/R
!====
        if (ltext) then
!
            call fointe('FM', zk8(itext), 4, nompar, valpar,&
                        texnp1, ier)
!
            if (theta .ne. 1.0d0) then
                valpar(4) = zr(itemps)-zr(itemps+1)
                call fointe('FM', zk8(itext), 4, nompar, valpar,&
                            texn, ier)
!
            else
                texn = 0.d0
            endif
!
            valpar(4) = zr(itemps)
            call fointe('FM', zk8(iech), 4, nompar, valpar,&
                        echnp1, ier)
!
            if (theta .ne. 1.0d0) then
                valpar(4) = zr(itemps)-zr(itemps+1)
                call fointe('FM', zk8(iech), 4, nompar, valpar,&
                            echn, ier)
!
            else
                echn = 0.d0
            endif
!
            do 120 i = 1, nno
                zr(ivectt+i-1) = zr(ivectt+i-1) + jac * zr(ipoids+ipg- 1) * zr(ivf+ldec+i-1) * ( &
                                 &theta*echnp1*texnp1+(1.0d0- theta)*echn*(texn-tem))
120          continue
!
!====
! 2.2 OPTION CHAR_THER_RAYO_F/R
!====
        else
!
            call fointe('FM', zk8(iray), 4, nompar, valpar,&
                        sigm1, ier)
            if (theta .ne. 1.0d0) then
                valpar(4) = zr(itemps)-zr(itemps+1)
                call fointe('FM', zk8(iray), 4, nompar, valpar,&
                            sigmn, ier)
            else
                sigmn = 0.d0
            endif
!
            valpar(4) = zr(itemps)
            call fointe('FM', zk8(iray+1), 4, nompar, valpar,&
                        eps1, ier)
            if (theta .ne. 1.0d0) then
                valpar(4) = zr(itemps)-zr(itemps+1)
                call fointe('FM', zk8(iray+1), 4, nompar, valpar,&
                            epsn, ier)
            else
                epsn = 0.d0
            endif
!
            valpar(4) = zr(itemps)
            call fointe('FM', zk8(iray+2), 4, nompar, valpar,&
                        tpf1, ier)
            if (theta .ne. 1.0d0) then
                valpar(4) = zr(itemps)-zr(itemps+1)
                call fointe('FM', zk8(iray+2), 4, nompar, valpar,&
                            tpfn, ier)
            else
                tpfn = 0.d0
            endif
!
            do 130 i = 1, nno
                zr(ivectt+i-1) = zr(ivectt+i-1) + jac * zr(ipoids+ipg- 1) * zr(ivf+ldec+i-1) * ( &
                                 &theta *sigm1*eps1* (tpf1+ tz0)**4+ (1.0d0-theta)*sigmn*epsn*((t&
                                 &pfn+tz0)**4-(tem+ tz0)**4))
130          continue
! FIN DU IF LTEXT
        endif
! FIN BOUCLE SUR LES PTS DE GAUSS
101  end do
end subroutine
