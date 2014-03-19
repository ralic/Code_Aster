subroutine te0163(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vff3d.h"
!
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
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
!     CALCUL FORCES ELEMENTAIRES DE LAPLACE DES CABLES
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'CHAR_MECA' : CALCUL DE LA FORCE DE LAPLACE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECABL2'     : CABLE2
!     ------------------------------------------------------------------
!
!
!
    character(len=8) :: elrefe, nomail
    character(len=16) :: listma, ltrans
    character(len=19) :: chgeom
    real(kind=8) :: zero
    real(kind=8) :: xl, e1, e2, e3, f1, f2, f3, g1, g2, g3, r1, r2, r3, q1, q2
    real(kind=8) :: q3, dd
    real(kind=8) :: b1, b2, b3, u(3), s, d
    real(kind=8) :: poids(20)
    integer :: iadzi, iazk24
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, idfdk, ilapl, ilist, ima, ipoids, ivect
    integer :: ivf, j, jgano, jgeom, jlima, k, kp
    integer :: lx, nbma, nbma2, nddl, ndim, nno, nnos
    integer :: no1, no2, npg
!-----------------------------------------------------------------------
    call jemarq()
    call elref1(elrefe)
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
    zero = 0.d0
!
!
    if (nomte .eq. 'MECA_POU_D_T_GD') then
        nddl = 6
    else
        nddl = 3
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2+ (zr(lx+5)-zr(lx+2))**2+ (zr(lx+6)-zr(lx+3))**2 )
    if (xl .eq. zero) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
!
!     ------------------- CALCUL DES VECTEURS ELEMENTAIRES ------------
!
    call jevech('PFLAPLA', 'L', ilapl)
    call jevech('PLISTMA', 'L', ilist)
    call jevech('PVECTUR', 'E', ivect)
    listma = zk16(ilist)
    ltrans = zk16(ilist+1)
    chgeom = zk24(ilapl+1) (1:19)
    call jeveuo(chgeom//'.VALE', 'L', jgeom)
!
    e1 = zr(lx+4) - zr(lx+1)
    e2 = zr(lx+5) - zr(lx+2)
    e3 = zr(lx+6) - zr(lx+3)
    s = sqrt(e1**2+e2**2+e3**2)
    e1 = e1/s
    e2 = e2/s
    e3 = e3/s
    if (listma .eq. ' ' .or. ltrans .eq. ' ') goto 60
    call jeveuo(listma, 'L', jlima)
    call jelira(listma, 'LONMAX', nbma2)
    nbma = nbma2/2
!C    2 BARRES EN POSITION QUELCONQUE
    do 50 ima = 1, nbma
        no1 = zi(jlima+2*ima-2)
        no2 = zi(jlima+2*ima-1)
        g1 = zr(jgeom+3*no2-3) - zr(jgeom+3*no1-3)
        g2 = zr(jgeom+3*no2-2) - zr(jgeom+3*no1-2)
        g3 = zr(jgeom+3*no2-1) - zr(jgeom+3*no1-1)
        s = sqrt(g1**2+g2**2+g3**2)
        f1 = g1/s
        f2 = g2/s
        f3 = g3/s
        do 40 kp = 1, npg
            k = (kp-1)*nno
            if (ima .eq. 1) call vff3d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(lx+1), poids(kp))
            r1 = -zr(jgeom+3*no2-3)
            r2 = -zr(jgeom+3*no2-2)
            r3 = -zr(jgeom+3*no2-1)
            do 10 i = 1, nno
                r1 = r1 + zr(lx+1+3* (i-1))*zr(ivf+k+i-1)
                r2 = r2 + zr(lx+2+3* (i-1))*zr(ivf+k+i-1)
                r3 = r3 + zr(lx+3+3* (i-1))*zr(ivf+k+i-1)
10          continue
            q1 = r1 + g1
            q2 = r2 + g2
            q3 = r3 + g3
            b1 = f2*q3 - f3*q2
            b2 = f3*q1 - f1*q3
            b3 = f1*q2 - f2*q1
            d = sqrt(b1**2+b2**2+b3**2)
            dd = d/sqrt(q1**2+q2**2+q3**2)
            if (dd .lt. 1.d-8) goto 40
            b1 = b1/d
            b2 = b2/d
            b3 = b3/d
            u(1) = e2*b3 - e3*b2
            u(2) = e3*b1 - e1*b3
            u(3) = e1*b2 - e2*b1
            s = sqrt(q1**2+q2**2+q3**2)
            q1 = q1/s
            q2 = q2/s
            q3 = q3/s
            s = sqrt(r1**2+r2**2+r3**2)
            r1 = r1/s
            r2 = r2/s
            r3 = r3/s
            s = f1* (q1-r1) + f2* (q2-r2) + f3* (q3-r3)
            s = s/d/2.d0
            do 30 i = 1, nno
                do 20 j = 1, 3
                    zr(ivect-1+j+nddl* (i-1)) = zr(&
                                                ivect-1+j+nddl* (i- 1)) + s*u(j)*poids(kp)*zr(ivf&
                                                &+k+i-1&
                                                )
20              continue
30          continue
40      continue
50  end do
!
60  continue
!
    call jedema()
end subroutine
