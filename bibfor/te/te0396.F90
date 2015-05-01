subroutine te0396(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W0104
    implicit none
!
#include "jeveux.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gdfint.h"
#include "asterfort/gdjrg0.h"
#include "asterfort/jevech.h"
#include "asterfort/marota.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/promat.h"
#include "asterfort/rcvalb.h"
#include "asterfort/verift.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  OPTION : 'CHAR_MECA_TEMP_R'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nno, npg, ipoids, ivf, idfdk, kp, ne, imate
    integer :: lorien, jefint, kc, igeom, i, k0, ifint, ic
    character(len=8) :: elrefe
    character(len=16) :: nomres(4)
    integer :: icodre(4)
    real(kind=8) :: en(3, 2), enprim(3, 2), valres(4), granc(6), fint(6, 3)
    real(kind=8) :: y0(3), x00(3, 3), x0pg(3), rot0(3, 3), nu, gn(3), gm(3)
    real(kind=8) :: pn(3), pm(3), qigk(3), rotk(3, 3), rotabs(3, 3), epsthe
!
!      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
!
!-----------------------------------------------------------------------
    integer :: ico, jgano, ndim, nnos
    real(kind=8) :: a, ajacob, ay, az, e, g, pjacob
    real(kind=8) :: r8bid, unsurj, xiy, xiz, xjx
!-----------------------------------------------------------------------
    integer, parameter :: nb_cara = 6
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','JX1'/
!-----------------------------------------------------------------------
    call elref1(elrefe)
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
!
    ico = 0
    do kp = 1, npg
        do ne = 1, nno
            ico = ico + 1
            en(ne,kp) = zr(ivf-1+ico)
            enprim(ne,kp) = zr(idfdk-1+ico)
        end do
    end do
!
!
! PARAMETRES EN ENTREE
    call jevech('PMATERC', 'L', imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'RHO'
    r8bid = 0.0d0
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', 'ELAS', 0, '  ', [r8bid],&
                2, nomres, valres, icodre, 1)
    call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                ' ', 'ELAS', 0, '  ', [r8bid],&
                1, nomres(3), valres(3), icodre(3), 0)
    e = valres(1)
    nu = valres(2)
    g = e/ (2.d0* (1.0d0+nu))
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!     --- LA SECTION EST SUPPOSEE CONSTANTE ---
!
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
    a      = vale_cara(1)
    xiy    = vale_cara(2)
    xiz    = vale_cara(3)
    ay  = vale_cara(4)
    az  = vale_cara(5)
    xjx    = vale_cara(6)
    granc(1) = e*a
    granc(2) = g*a/ay
    granc(3) = g*a/az
    granc(4) = g*xjx
    granc(5) = e*xiy
    granc(6) = e*xiz
!
!     --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
!
    call jevech('PCAORIE', 'L', lorien)
    y0(1) = zr(lorien)
    y0(2) = zr(lorien+1)
    y0(3) = zr(lorien+2)
!
! PARAMETRES EN SORTIE
!
    call jevech('PVECTUR', 'E', jefint)
    do ne = 1, nno
        do kc = 1, 6
            fint(kc,ne) = 0.d0
        end do
    end do
!
    call jevech('PGEOMER', 'L', igeom)
!
    k0 = igeom - 1
!
    do ne = 1, nno
        do kc = 1, 3
            k0 = k0 + 1
            x00(kc,ne) = zr(k0)
        end do
    end do
!
!     MATRICE DE ROTATION ASSOCIE AU VECTEUR ROTATION NUL
    do i = 1, 3
        qigk(i) = 0.d0
    end do
    call marota(qigk, rotk)
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    do kp = 1, npg
        call gdjrg0(kp, nno, enprim, x00, y0,&
                    ajacob, rot0)
        pjacob = zr(ipoids-1+kp)*ajacob
        call promat(rotk, 3, 3, 3, rot0,&
                    3, 3, 3, rotabs)
!
        do ic = 1, 3
            x0pg(ic) = 0.d0
        end do
        unsurj = 1.d0/ajacob
        do ic = 1, 3
            do ne = 1, nno
                x0pg(ic) = x0pg(ic) + unsurj*enprim(ne,kp)*x00(ic,ne)
            end do
        end do
        call verift('RIGI', kp, 1, '+', zi(imate),&
                    epsth=epsthe)
        do i = 1, 3
            gn(i) = 0.d0
            gm(i) = 0.d0
        end do
!
!        DILATATION THERMIQUE : -E*A*ALPHA*(T-TREF)
!
        gn(1) = gn(1) + granc(1)*epsthe
!
        call promat(rotabs, 3, 3, 3, gn,&
                    3, 3, 1, pn)
        call promat(rotabs, 3, 3, 3, gm,&
                    3, 3, 1, pm)
        call gdfint(kp, nno, ajacob, pjacob, en,&
                    enprim, x0pg, pn, pm, fint)
!
    end do
!
!     FIN DE BOUCLE SUR LES POINTS DE GAUSS
!
    ifint = jefint - 1
    do ne = 1, nno
        do kc = 1, 6
            ifint = ifint + 1
            zr(ifint) = fint(kc,ne)
        end do
    end do
end subroutine
