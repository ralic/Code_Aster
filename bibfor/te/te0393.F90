subroutine te0393(option, nomte)
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
!
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/gdfint.h"
#include "asterfort/gdjrg0.h"
#include "asterfort/jevech.h"
#include "asterfort/marota.h"
#include "asterfort/promat.h"
#include "asterfort/terefe.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES FORCES NODALES DE MECA_POU_D_T_GD
!                          OPTION : 'FORC_NODA' OU 'REFE_FORC_NODA'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: elrefe
    real(kind=8) :: en(3, 2), enprim(3, 2), fint(6, 3), y0(3), x00(3, 3)
    real(kind=8) :: x0k(3, 3), qik(3, 3), x0pg(3), qig(3), rot(3, 3), rot0(3, 3)
    real(kind=8) :: rotabs(3, 3), gn(3), gm(3), pn(3), pm(3), zero, un, unsurj
    real(kind=8) :: pjacob, ajacob
!
    real(kind=8) :: forref, momref
    integer :: nno, nc, ino, i, ndim, nnos, npg, ipoids, ivf, idfdk, jgano, kp
    integer :: ne, ic, kc, k0, k1, k2, ico
    integer :: ivectu, idepm, igeom, idepde, isigma, lorien, jefint
    integer :: ifint
!
    parameter (zero=0.0d0,un=1.0d0)
! ----------------------------------------------------------------------
!
!
    if (option .eq. 'REFE_FORC_NODA') then
        nno = 2
        nc = 6
        call terefe('MOMENT_REFE', 'MECA_POUTRE', momref)
        call terefe('EFFORT_REFE', 'MECA_POUTRE', forref)
        call jevech('PVECTUR', 'E', ivectu)
        do 201 ino = 1, nno
            do 203 i = 1, 3
                zr(ivectu+(ino-1)*nc+i-1)=forref
203          continue
            do 202 i = 4, nc
                zr(ivectu+(ino-1)*nc+i-1)=momref
202          continue
201      continue
!
    else if (option.eq.'FORC_NODA') then
        call elref1(elrefe)
!
!        PARAMETRES EN ENTREE
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfdk, jgano)
!
        ico = 0
        do 20 kp = 1, npg
            do 10 ne = 1, nno
                ico = ico + 1
                en(ne,kp) = zr(ivf-1+ico)
                enprim(ne,kp) = zr(idfdk-1+ico)
10          continue
20      continue
!
!        CALL JEVECH('PTEMPPR','L',ITEMPR)
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PDEPLMR', 'L', idepm)
        call jevech('PDEPLPR', 'L', idepde)
        call jevech('PCONTMR', 'L', isigma)
!
!        --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
        call jevech('PCAORIE', 'L', lorien)
        y0(1) = zr(lorien)
        y0(2) = zr(lorien+1)
        y0(3) = zr(lorien+2)
!
!        PARAMETRES EN SORTIE
        call jevech('PVECTUR', 'E', jefint)
        do 40 ne = 1, nno
            do 30 kc = 1, 6
                fint(kc,ne) = zero
30          continue
40      continue
!
!       DO 21 NE=1,NNO
!         TEMPN(NE) = ZR(ITEMPR-1+NE)
!21      CONTINUE
!
        k0 = igeom - 1
        k1 = idepm - 1
        k2 = idepde - 1
!
        do 70 ne = 1, nno
            do 50 kc = 1, 3
                k0 = k0 + 1
                k1 = k1 + 1
                k2 = k2 + 1
                x00(kc,ne) = zr(k0)
                x0k(kc,ne) = zr(k0) + zr(k1) + zr(k2)
50          continue
            do 60 kc = 1, 3
                k1 = k1 + 1
                k2 = k2 + 1
                qik(kc,ne) = zr(k1) + zr(k2)
60          continue
70      continue
!
!        BOUCLE SUR LES POINTS DE GAUSS
        do 120 kp = 1, npg
            call gdjrg0(kp, nno, enprim, x00, y0,&
                        ajacob, rot0)
            pjacob = zr(ipoids-1+kp)*ajacob
!
            do 80 ic = 1, 3
                x0pg(ic) = zero
                qig(ic) = zero
80          continue
!           TEMPG = ZERO
            unsurj = un/ajacob
            do 100 ic = 1, 3
                do 90 ne = 1, nno
                    x0pg(ic) = x0pg(ic) + unsurj*enprim(ne,kp)*x0k(ic, ne)
                    qig(ic) = qig(ic) + en(ne,kp)*qik(ic,ne)
90              continue
100          continue
!         DO 45 NE=1,NNO
!          TEMPG = TEMPG + EN(NE,KP)*TEMPN(NE)
!45       CONTINUE
!
            call marota(qig, rot)
            call promat(rot, 3, 3, 3, rot0,&
                        3, 3, 3, rotabs)
            do 110 ic = 1, 3
                gn(ic) = zr(isigma-1+6* (kp-1)+ic)
                gm(ic) = zr(isigma+2+6* (kp-1)+ic)
110          continue
            call promat(rotabs, 3, 3, 3, gn,&
                        3, 3, 1, pn)
            call promat(rotabs, 3, 3, 3, gm,&
                        3, 3, 1, pm)
!
            call gdfint(kp, nno, ajacob, pjacob, en,&
                        enprim, x0pg, pn, pm, fint)
!
120      continue
!        FIN DE BOUCLE SUR LES POINTS DE GAUSS
        ifint = jefint - 1
        do 140 ne = 1, nno
            do 130 kc = 1, 6
                ifint = ifint + 1
                zr(ifint) = fint(kc,ne)
130          continue
140      continue
    endif
!
end subroutine
