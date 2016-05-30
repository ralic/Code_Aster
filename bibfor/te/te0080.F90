subroutine te0080(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=C1513
    implicit none
#include "jeveux.h"
#include "asterfort/connec.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe_varc.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/teattr.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'CHAR_THER_SOUR_F'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: ndim, nno, nnos, kp, npg, i, k, itemps, ivectt, isour
    integer :: ipoids, ivf, idfde, igeom, j, icode
    integer :: nnop2, c(6, 9), ise, nse, nbres, jgano, ibid
    parameter         ( nbres=3 )
    character(len=8) :: nompar(nbres), elrefe, alias8
    real(kind=8) :: valpar(nbres), poids, r, z, sour
    real(kind=8) :: coorse(18), vectt(9), theta, soun, sounp1
!
!
    call elref1(elrefe)
!
    if (lteatt('LUMPE','OUI')) then
        call teattr('S', 'ALIAS8', alias8, ibid)
        if (alias8(6:8) .eq. 'QU9') elrefe='QU4'
        if (alias8(6:8) .eq. 'TR6') elrefe='TR3'
    endif
!
    call elrefe_info(elrefe=elrefe,fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PSOURCF', 'L', isour)
    call jevech('PVECTTR', 'E', ivectt)
    theta = zr(itemps+2)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'INST'
!
    call connec(nomte, nse, nnop2, c)
!
    do i = 1, nnop2
        vectt(i)=0.d0
    end do
!
! BOUCLE SUR LES SOUS-ELEMENTS
!
    do ise = 1, nse
        do i = 1, nno
            do j = 1, 2
                coorse(2*(i-1)+j) = zr(igeom-1+2*(c(ise,i)-1)+j)
            end do
        end do
        do kp = 1, npg
            k=(kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coorse,&
                        poids)
            r = 0.d0
            z = 0.d0
            do i = 1, nno
                r = r + coorse(2*(i-1)+1) * zr(ivf+k+i-1)
                z = z + coorse(2*(i-1)+2) * zr(ivf+k+i-1)
            end do
            if (lteatt('AXIS','OUI')) poids = poids*r
            valpar(1) = r
            valpar(2) = z
            valpar(3) = zr(itemps)
!           EC : je voulais mettre fami = RIGI et kpg = kp 
!                mais il n'y a que FPG1 dans MATER pour cet élément
            call fointe_varc('FM', 'FPG1', 1, 1, '+',&
                        zk8(isour), 3, nompar, valpar,&
                        sounp1, icode)
            if (theta .ne. 1.0d0) then
                valpar(3) = zr(itemps)-zr(itemps+1)
                call fointe_varc('FM', 'FPG1', 1, 1, '+',&
                            zk8(isour), 3, nompar, valpar,&
                            soun, icode)
            else
                soun = 0.d0
            endif
            sour = theta*sounp1 + (1.0d0-theta)*soun
!DIR$ IVDEP
            do i = 1, nno
                k=(kp-1)*nno
                vectt(c(ise,i)) = vectt(c(ise,i)) + poids * zr(ivf+k+ i-1 ) * sour
            end do
        end do
    end do
!
    do i = 1, nnop2
        zr(ivectt-1+i)=vectt(i)
    end do
!
end subroutine
