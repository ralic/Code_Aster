subroutine te0073(option, nomte)
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
!                          OPTION : 'CHAR_THER_TEXT_F/R'
!                                   'CHAR_THER_RAYO_F/R'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
#include "jeveux.h"
!
#include "asterc/r8t0.h"
#include "asterfort/assert.h"
#include "asterfort/connec.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/teattr.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
    integer :: nbres
    parameter (nbres=3)
    character(len=8) :: nompar(nbres), elrefe, alias8
    real(kind=8) :: valpar(nbres), poids, r, z, nx, ny, tpg, coen, coenp1, texn
    real(kind=8) :: texnp1, coorse(18), vectt(9), theta, sigm1, sigmn, eps1
    real(kind=8) :: epsn, tpf1, tpfn, tz0
    integer :: nno, nnos, jgano, ndim, kp, npg, ipoids, ivf, idfde, igeom
    integer :: itemps, ivectt, i, l, li, itex, icoefh, iray, itemp, nnop2
    integer :: c(6, 9), ise, nse, j, ier, icode, ibid
    logical :: laxi, ltext
!
!
    call elref1(elrefe)
!
    if (lteatt('LUMPE','OUI')) then
        call teattr('S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'SE3') elrefe='SE2'
    endif
!
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
    tz0 = r8t0()
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!
    if (option(11:14) .eq. 'TEXT') then
        ltext = .true.
    else if (option(11:14).eq.'RAYO') then
        ltext = .false.
    else
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
!
    if (ltext) then
! CHAR_.._TEXT
        call jevech('PTEMPER', 'L', itemp)
        call jevech('PCOEFHF', 'L', icoefh)
        call jevech('PT_EXTF', 'L', itex)
    else
! CHAR_..._RAYO
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
    call connec(nomte, nse, nnop2, c)
    do 10 i = 1, nnop2
        vectt(i) = 0.d0
10  end do
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'INST'
!
!====
! 2. CALCULS TERMES DE MASSE
!====
!
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do 160 ise = 1, nse
!
        do 30 i = 1, nno
            do 20 j = 1, 2
                coorse(2* (i-1)+j) = zr(igeom-1+2* (c(ise,i)-1)+j)
20          continue
30      continue
!
        do 150 kp = 1, npg
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        coorse, nx, ny, poids)
            r = 0.d0
            z = 0.d0
            tpg = 0.d0
            if (itemp .ne. 0) then
                do 40 i = 1, nno
! CALCUL DE T-
                    l = (kp-1)*nno + i
                    tpg = tpg + zr(itemp-1+c(ise,i))*zr(ivf+l-1)
40              continue
            endif
!
            do 50 i = 1, nno
                l = (kp-1)*nno + i
                r = r + coorse(2* (i-1)+1)*zr(ivf+l-1)
                z = z + coorse(2* (i-1)+2)*zr(ivf+l-1)
50          continue
            if (laxi) poids = poids*r
            valpar(1) = r
            valpar(2) = z
            valpar(3) = zr(itemps)
!
!====
! 2.1 OPTION CHAR_THER_TEXT_F/R
!====
            if (ltext) then
!
                call fointe('FM', zk8(icoefh), 3, nompar, valpar,&
                            coenp1, icode)
                ASSERT(icode.eq.0)
                if (theta .ne. 1.0d0) then
                    valpar(3) = zr(itemps) - zr(itemps+1)
                    call fointe('FM', zk8(icoefh), 3, nompar, valpar,&
                                coen, icode)
                    ASSERT(icode.eq.0)
                else
                    coen = 0.d0
                endif
!
                valpar(3) = zr(itemps)
                call fointe('FM', zk8(itex), 3, nompar, valpar,&
                            texnp1, icode)
                ASSERT(icode.eq.0)
                if (theta .ne. 1.0d0) then
                    valpar(3) = zr(itemps) - zr(itemps+1)
                    call fointe('FM', zk8(itex), 3, nompar, valpar,&
                                texn, icode)
                    ASSERT(icode.eq.0)
                else
                    texn = 0.d0
                endif
                do 80 i = 1, nno
                    li = ivf + (kp-1)*nno + i - 1
                    vectt(c(ise,i)) = vectt(&
                                      c(ise,i)) + poids*zr(li)* (theta*coenp1*texnp1+ (1.0d0-thet&
                                      &a)*coen* (texn- tpg)&
                                      )
80              continue
!====
! 2.2 OPTION CHAR_THER_RAYO_F/R
!====
            else
!
                call fointe('FM', zk8(iray), 3, nompar, valpar,&
                            sigm1, ier)
                ASSERT(ier.eq.0)
                if (theta .ne. 1.0d0) then
                    valpar(3) = zr(itemps) - zr(itemps+1)
                    call fointe('FM', zk8(iray), 3, nompar, valpar,&
                                sigmn, ier)
                    ASSERT(ier.eq.0)
                else
                    sigmn = 0.d0
                endif
!
                valpar(3) = zr(itemps)
                call fointe('FM', zk8(iray+1), 3, nompar, valpar,&
                            eps1, ier)
                ASSERT(ier.eq.0)
                if (theta .ne. 1.0d0) then
                    valpar(3) = zr(itemps) - zr(itemps+1)
                    call fointe('FM', zk8(iray+1), 3, nompar, valpar,&
                                epsn, ier)
                    ASSERT(ier.eq.0)
                else
                    epsn = 0.d0
                endif
!
                valpar(3) = zr(itemps)
                call fointe('FM', zk8(iray+2), 3, nompar, valpar,&
                            tpf1, ier)
                ASSERT(ier.eq.0)
                if (theta .ne. 1.0d0) then
                    valpar(3) = zr(itemps) - zr(itemps+1)
                    call fointe('FM', zk8(iray+2), 3, nompar, valpar,&
                                tpfn, ier)
                    ASSERT(ier.eq.0)
                else
                    tpfn = 0.d0
                endif
                do 100 i = 1, nno
                    li = ivf + (kp-1)*nno + i - 1
                    vectt(c(ise,i)) = vectt(&
                                      c(ise,i)) + poids*zr(li)* (theta*sigm1*eps1* (tpf1+tz0)**4+&
                                      & (1.0d0-theta)* sigmn* epsn* ((tpfn+tz0)**4- (tpg+tz0)**4)&
                                      )
100              continue
!
! FIN DU IF LTEXT
            endif
!
! FIN DE BOUCLE SUR LES PTS DE GAUSS
150      continue
! FIN DE BOUCLE SUR LES SOUS-ELEMENTS
160  end do
!
    do 170 i = 1, nnop2
        zr(ivectt-1+i) = vectt(i)
170  end do
end subroutine
