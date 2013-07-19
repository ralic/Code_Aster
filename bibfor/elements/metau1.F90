subroutine metau1(option, nomte, iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/verift.h"
    character(len=16) :: option, nomte
    integer :: iret
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_MECA_TEMP_Z  '
! ......................................................................
!  IN  OPTION K16 : NOM DE L OPTION (CHAR_MECA_TEMP_Z)
!  IN  NOMTE  K16 : NOM DU TYPE D ELEMENT
!  OUT IRET   I   : =1 PRESENCE DE METALLURGIE
!                   =0 PAS DE METALLURGIE
!
!-----------------------------------------------------------------------
    integer :: k, mater, nbres
    real(kind=8) :: rbid, zalpha
!-----------------------------------------------------------------------
    parameter (nbres=6)
!
    character(len=8) :: nomres(nbres), acier(4), zirc(2), materi
    integer :: icodre(nbres)
    real(kind=8) :: vk3al, valres(nbres), coef1, coef2, epsth
    real(kind=8) :: dfdx(9), dfdy(9), poids, r, phaspg(7), epsthe(2)
    integer :: nno, kp, npg1, i, ivectu, nz, l
    integer :: ire1, ire2, iret1
    logical :: lacier
    integer :: ipoids, ivf, idfde, igeom, imate, ndim, nnos, jgano
!
!
    data acier /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/
    data zirc /'ALPHPUR','ALPHBETA'/
!
!
    iret=1
    materi = ' '
    lacier=.false.
!
    call rcvarc(' ', acier(1), '+', 'RIGI', 1,&
                1, rbid, ire1)
    if (ire1 .eq. 1) then
        call rcvarc(' ', zirc(1), '+', 'RIGI', 1,&
                    1, rbid, ire2)
        if (ire2 .eq. 1) then
            iret=0
            goto 9999
        else
            nz=2
        endif
    else
        nz=4
        lacier=.true.
    endif
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    mater = zi(imate)
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'F_ALPHA'
    nomres(4) = 'C_ALPHA'
    nomres(5) = 'PHASE_REFE'
    nomres(6) = 'EPSF_EPSC_TREF'
!
!
    call jevech('PVECTUR', 'E', ivectu)
!
    do 40 kp = 1, npg1
        k = (kp-1)*nno
!
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
        r = 0.d0
!    RECUPERATION DES PHASES METALLURGIQUES
        do 5 l = 1, nz
            if (lacier) then
                call rcvarc(' ', acier(l), '+', 'RIGI', kp,&
                            1, phaspg(l), ire1)
            else
                call rcvarc(' ', zirc(l), '+', 'RIGI', kp,&
                            1, phaspg(l), ire1)
            endif
 5      continue
!
        do 10 i = 1, nno
            r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
10      continue
!
        call verift('RIGI', kp, 1, '+', mater,&
                    materi, 'ELAS_META', 2, epsthe(1), iret1)
        call rcvalb('RIGI', kp, 1, '+', mater,&
                    ' ', 'ELAS_META', 0, 'TEMP', 0.d0,&
                    6, nomres, valres, icodre, 1)
        vk3al = valres(1)/ (1.d0-2.d0*valres(2))
        if (lteatt(' ','AXIS','OUI')) then
            poids = poids*r
            do 20 i = 1, nno
                k = (kp-1)*nno
                dfdx(i) = dfdx(i) + zr(ivf+k+i-1)/r
20          continue
        endif
!
        zalpha=0.d0
        do 25 i = 1, nz
            zalpha=zalpha+phaspg(i)
25      continue
!
        coef1 = (1.d0-zalpha)* (epsthe(1)- (1-valres(5))*valres(6))
        coef2 = zalpha* (epsthe(2)+valres(5)*valres(6))
        epsth = coef1 + coef2
        poids = poids*vk3al*epsth
        do 30 i = 1, nno
            k = (kp-1)*nno
            zr(ivectu+2*i-2) = zr(ivectu+2*i-2) + poids*dfdx(i)
            zr(ivectu+2*i-1) = zr(ivectu+2*i-1) + poids*dfdy(i)
30      continue
40  end do
9999  continue
end subroutine
