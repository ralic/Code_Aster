subroutine te0210(option, nomte)
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
!                          OPTION : 'CHAR_THER_PARO_F'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elref2.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/vff2dn.h"
    integer :: nbres
    parameter (nbres=3)
    character(len=16) :: option, nomte
    character(len=8) :: nompar(nbres), lirefe(2)
    real(kind=8) :: valpar(nbres), poids, poids1, poids2, r, r1, r2, z, hechp
    real(kind=8) :: nx, ny, tpg, theta, z1, z2
    integer :: nno, nnos, jgano, ndim, kp, npg, ipoids, ivf, idfde, igeom
    integer :: ivectt, i, l, li, ihechp, itemp, icode, nbelr, itemps
    logical :: laxi
!
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
    call elref2(nomte, 2, lirefe, nbelr)
    ASSERT(nbelr.eq.2)
    call elrefe_info(elrefe=lirefe(2),fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PHECHPF', 'L', ihechp)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PVECTTR', 'E', ivectt)
    theta = zr(itemps+2)
!
!====
! 2. CALCULS TERMES DE MASSE
!====
!    BOUCLE SUR LES POINTS DE GAUSS
    do 50 kp = 1, npg
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids1)
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom+2*nno), nx, ny, poids2)
        r1 = 0.d0
        r2 = 0.d0
        z1 = 0.d0
        z2 = 0.d0
        tpg = 0.d0
        do 10 i = 1, nno
            l = (kp-1)*nno + i
            r1 = r1 + zr(igeom+2*i-2)*zr(ivf+l-1)
            r2 = r2 + zr(igeom+2* (nno+i)-2)*zr(ivf+l-1)
            z1 = z1 + zr(igeom+2*i-1)*zr(ivf+l-1)
            z2 = z2 + zr(igeom+2* (nno+i)-1)*zr(ivf+l-1)
            tpg = tpg + (zr(itemp+nno+i-1)-zr(itemp+i-1))*zr(ivf+l-1)
10      continue
        if (laxi) then
            poids1 = poids1*r1
            poids2 = poids2*r2
        endif
!
        r = (r1+r2)/2.0d0
        z = (z1+z2)/2.0d0
        poids = (poids1+poids2)/2
        valpar(1) = r
        valpar(2) = z
        valpar(3) = zr(itemps)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'INST'
        call fointe('FM', zk8(ihechp), 3, nompar, valpar,&
                    hechp, icode)
!
        do 30 i = 1, nno
            li = ivf + (kp-1)*nno + i - 1
            zr(ivectt+i-1) = zr(ivectt+i-1) + poids*zr(li)*hechp* ( 1.0d0-theta)*tpg
            zr(ivectt+i-1+nno) = zr(ivectt+i-1+nno) - poids*zr(li)* hechp* (1.0d0-theta)*tpg
30      continue
50  end do
end subroutine
