subroutine te0274(option, nomte)
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
!          CORRESPONDANT AU FLUX NON-LINEAIRE
!          ELEMENTS DE FACE 2D
!
!         OPTION : 'CHAR_THER_FLUNL'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/connec.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/foderi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/teattr.h"
#include "asterfort/vff2dn.h"
    character(len=16) :: option, nomte
    real(kind=8) :: poids, r, nx, ny, theta, alpha, rbid, tpg, coorse(18)
    real(kind=8) :: vectt(9)
    integer :: nno, nnos, jgano, ndim, kp, npg, ipoids, ivf, idfde, igeom, i, j
    integer :: l, li, iflux, ivectt, nnop2, c(6, 9), ise, nse, itempr, itemps
    integer :: ibid
    character(len=8) :: coef, elrefe, alias8
    logical(kind=1) :: laxi
!
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
    call jemarq()
!
    call elref1(elrefe)
!
    if (lteatt('LUMPE','OUI')) then
        call teattr('S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'SE3') elrefe='SE2'
    endif
!
    call elrefe_info(elrefe=elrefe,fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    if (lteatt('AXIS','OUI')) then
        laxi = .true.
    else
        laxi = .false.
    endif
!
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
!
! RECUPERATION DE T-
    call jevech('PTEMPER', 'L', itempr)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
! FLUX NON-LIN
    call jevech('PFLUXNL', 'L', iflux)
    call jevech('PVECTTR', 'E', ivectt)
!
!====
! 1.3 PREALABLES LIES AUX CALCULS
!====
!
    theta = zr(itemps+2)
    coef = zk8(iflux)
    if (coef(1:7) .eq. '&FOZERO') goto 130
!
    call connec(nomte, nse, nnop2, c)
    do 10 i = 1, nnop2
        vectt(i) = 0.d0
10  end do
!
!====
! 2. CALCULS TERMES
!====
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do 110 ise = 1, nse
!
        do 30 i = 1, nno
            do 20 j = 1, 2
                coorse(2* (i-1)+j) = zr(igeom-1+2* (c(ise,i)-1)+j)
20          continue
30      continue
!
! BOUCLE SUR LES POINTS DE GAUSS
        do 100 kp = 1, npg
            call vff2dn(ndim, nno, kp, ipoids, idfde,&
                        coorse, nx, ny, poids)
            tpg = 0.d0
            do 40 i = 1, nno
! CALCUL DE T-
                l = (kp-1)*nno + i
                tpg = tpg + zr(itempr-1+c(ise,i))*zr(ivf+l-1)
40          continue
!
! CALCUL DU JACOBIEN EN AXI
            if (laxi) then
                r = 0.d0
                do 60 i = 1, nno
                    l = (kp-1)*nno + i
                    r = r + coorse(2* (i-1)+1)*zr(ivf+l-1)
60              continue
                poids = poids*r
            endif
!
            call foderi(coef, tpg, alpha, rbid)
!
            do 70 i = 1, nno
                li = ivf + (kp-1)*nno + i - 1
                vectt(c(ise,i)) = vectt( c(ise,i)) + poids* (1.d0- theta)*alpha*zr(li)
70          continue
! FIN BOUCLE SUR LES PTS DE GAUSS
100      continue
! FIN BOUCLE SUR LES SOUS-ELEMENTS
110  end do
!
    do 120 i = 1, nnop2
        zr(ivectt-1+i) = vectt(i)
120  end do
!
130  continue
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
