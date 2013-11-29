subroutine te0537(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfdef.h"
#include "asterfort/pmfdge.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfpti.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
!
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
!     POU_D_EM : POUTRE MULTIFIBRE EULER BERNOULLI
!     CALCUL DE L'OPTION EPSI_ELGA
!       - DEFORMATIONS DANS LES FIBRES (SOUS PTS DE GAUSS) A PARTIR DES
!         DEPLACEMENTS
!     CALCUL DE L'OPTION SIEF_ELGA
!       - CONTRAINTE DANS LES FIBRES (SOUS PTS DE GAUSS) COMPO LINEAIRE
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!     ------------------------------------------------------------------
!
    integer :: jcont, lorien, jdepl, imate, isect, lx, nno, nc, i, iret
    parameter (nno=2,nc=6)
    character(len=16) :: ch16
    real(kind=8) :: ul(12), pgl(3, 3), dege(6), xl, e, nu
    real(kind=8) :: g, xjx, gxjx
    integer :: nbfib, ncarfi, jacf, jtab(7)
    real(kind=8) :: casect(6), casec1(6), xl2, nx, ty, tz, mx, my, mz
    real(kind=8) :: dep1, dep2, dep3, dep4, co6, co12
    real(kind=8) :: zero, un, deux, six
    parameter (zero=0.0d+0,un=1.0d+0,deux=2.d+0,six=6.d+0)
    real(kind=8) :: b(4), gg, xi, wi, valres(2), sign
    integer :: ip, ipos, nbgfmx, iadzi, iazk24, isicom, istrxr
    integer :: ipos1, ipos2, nbfig, nbgf, ig, nugf, ifb, icp, isdcom, icompo
    character(len=8) :: materi, nomres(2), nomail
    integer :: codres(2), ncomp
    integer :: npg, ndim, nnoel, nnos, ipoids, ivf, iplouf
!     ------------------------------------------------------------------
! ----------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nnoel, nnos,&
                npg, ipoids, ivf, iplouf, iplouf)
    ASSERT(nno.eq.nnoel)
!     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES :
    call jevech('PNBSP_I', 'L', ifb)
    nbfib = zi(ifb)
    nbgf=zi(ifb+1)
    call jevech('PFIBRES', 'L', jacf)
    ncarfi = 3
!
!   NOMBRE DE COMPOSANTES DES CHAMPS PSTRX? PAR POINTS DE GAUSS
    ncomp = 18
!
    if (option .eq. 'EPSI_ELGA') then
        call tecach('OON', 'PDEFOPG', 'E', iret, nval=7,&
                    itab=jtab)
        jcont = jtab(1)
    else if (option.eq.'SIEF_ELGA') then
        call jevech('PMATERC', 'L', imate)
        call tecach('OON', 'PCONTRR', 'E', iret, nval=7,&
                    itab=jtab)
        jcont = jtab(1)
    else if (option.eq.'STRX_ELGA') then
        call jevech('PMATERC', 'L', imate)
        call jevech('PSTRXRR', 'E', istrxr)
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
    call jevech('PGEOMER', 'L', lx)
    call jevech('PCAORIE', 'L', lorien)
    call jevech('PDEPLAR', 'L', jdepl)
!
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
    if (xl .eq. zero) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call matrot(zr(lorien), pgl)
!
!     --- PASSAGE DES DEPLACEMENTS DANS LE REPERE LOCAL ---
    call utpvgl(nno, nc, pgl, zr(jdepl), ul)
!
! --- SI OPTION EPSI_ELGA OU SIEF_ELGA
!
! --- BOUCLE SUR LES POINTS DE GAUSS
    if (option .ne. 'STRX_ELGA') then
        do 20 ip = 1, npg
!        ---  MATRICE B PUIS DEGE PUIS DEFORMATIONS SUR LES FIBRES
            call pmfpti(ip, zr(ipoids), zr(ivf), xl, xi,&
                        wi, b, gg)
!          ZERO POUR LA VARIABLE ALPHA DES MODES INCOMPATIBLES CAR
!          NON ACTIF SI CALCUL ELASTIQUE (RIGI_MECA et X_X_DEPL)
            call pmfdge(b, gg, ul, zero, dege)
            ipos=jcont+nbfib*(ip-1)
            call pmfdef(nbfib, ncarfi, zr(jacf), dege, zr(ipos))
20      continue
    endif
!
! --- SI EPSI_ELGA JCONT EST L'ADRESSE PDEFORR, ON SORT
!
    if (option .eq. 'SIEF_ELGA') then
!
! --- SI OPTION SIEF_ELGA ON CONTINUE
!
! --- RECUPERATION DES DIFFERENTS MATERIAUX DANS SDCOMP DANS COMPOR
        call jevech('PCOMPOR', 'L', icompo)
        call jeveuo(zk16(icompo-1+7), 'L', isdcom)
!
! --- BOUCLE SUR LES GROUPES DE FIBRE
        ipos1=jcont-1
        ipos2=ipos1+nbfib
        do 100 ig = 1, nbgf
            nugf=zi(ifb+1+ig)
            icp=isdcom-1+(nugf-1)*6
            read(zk24(icp+6),'(I24)')nbfig
            materi=zk24(icp+2)(1:8)
! ---    ON MULTIPLIE PAR E (CONSTANT SUR LE GROUPE)
            nomres(1) = 'E'
            nomres(2) = 'NU'
            call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                        materi, 'ELAS', 0, ' ', [0.d0],&
                        2, nomres, valres, codres, 1)
            e = valres(1)
            nu = valres(2)
! ---    ON MULTIPLIE LES ZR(JCONT) (DEFORMATIONS) PAR E
!        POUR AVOIR DES CONTRAINTES
            do 32 i = 1, nbfig
                zr(ipos1+i)=zr(ipos1+i) * e
                zr(ipos2+i)=zr(ipos2+i) * e
32          continue
            ipos1=ipos1+nbfig
            ipos2=ipos2+nbfig
100      continue
    endif
!
!
    if (option .eq. 'STRX_ELGA') then
!
! --- RECUPERATION DES DIFFERENTS MATERIAUX DANS SDCOMP DANS COMPOR
        call jevech('PCOMPOR', 'L', icompo)
        call jeveuo(zk16(icompo-1+7), 'L', isdcom)
!
!     RECUPERATION DU MATERIAU TORSION
!
        call jeveuo(zk16(icompo-1+7)(1:8)//'.CPRI', 'L', isicom)
        nbgfmx=zi(isicom+2)
        materi=zk24(isdcom-1+6*nbgfmx+1)(1:8)
        call matela(zi(imate), materi, 0, 0.d0, e,&
                    nu)
        g = e/ (deux* (un+nu))
! --- TORSION A PART
        call jevech('PCAGNPO', 'L', isect)
        xjx = zr(isect+7)
        gxjx = g*xjx
!
! --- CALCUL DES CARACTERISTIQUES INTEGREES DE LA SECTION ---
        do 116 i = 1, 6
            casect(i) = zero
116      continue
! ---   BOUCLE SUR LES GROUPES DE FIBRE
        ipos=jacf
        do 200 ig = 1, nbgf
            nugf=zi(ifb+1+ig)
            icp=isdcom-1+(nugf-1)*6
            read(zk24(icp+6),'(I24)')nbfig
            materi=zk24(icp+2)(1:8)
! ---    CALCUL DES CARACTERISTIQUES DE LA SECTION ---
            call pmfitg(nbfig, ncarfi, zr(ipos), casec1)
! ---    ON MULTIPLIE PAR E (CONSTANT SUR LE GROUPE)
            nomres(1) = 'E'
            nomres(2) = 'NU'
            call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                        materi, 'ELAS', 0, ' ', [0.d0],&
                        2, nomres, valres, codres, 1)
            e = valres(1)
            nu = valres(2)
            do 126 i = 1, 6
                casect(i) = casect(i) + e*casec1(i)
126          continue
            ipos=ipos+nbfig*ncarfi
!
200      continue
!
!
! --- ON DOIT AJOUTER LES EFFORTS GENERALISES (VOIR TE0535 ET TE0536)
! --- ON FAIT KELE*UL (TOUTES LES INTEGRATIONS SONT FAITES
!                           ANALYTIQUEMENT)
        xl2 = xl/deux
        co6 = six/xl/xl
        co12 = co6/xl2
! --- EFORTS TRANCHANTS
!     RAPPEL: KS22=MATSEC(5), KS33=MATSEC(4), KS23=-MATSEC(6)
        dep1 = ul(11) + ul(5)
        dep2 = ul(12) + ul(6)
        dep3 = ul(9) - ul(3)
        dep4 = ul(8) - ul(2)
        ty = co6* ( casect(6)*dep1-casect(4)*dep2) + co12* (casect(6)* dep3+casect(4)*dep4 )
        tz = co6* ( casect(5)*dep1-casect(6)*dep2) + co12* (casect(5)* dep3+casect(6)*dep4 )
! --- EFORT NORMAL ET MOMENTS
!     RAPPEL: KS11=MATSEC(1), KS12=MATSEC(3), KS13=-MATSEC(2)
        dep1 = ul(7) - ul(1)
        dep2 = ul(12) - ul(6)
        dep3 = ul(11) - ul(5)
        nx = (casect(1)*dep1-casect(2)*dep2+casect(3)*dep3)/xl
        my = (casect(3)*dep1-casect(6)*dep2+casect(5)*dep3)/xl
        mz = (-casect(2)*dep1-casect(6)*dep3+casect(4)*dep2)/xl
! --- TORSION
        mx = gxjx* (ul(10)-ul(4))/xl
!
        do ip = 1, npg
            if (ip .eq. 1) then
                sign = -1.d0
            else
                sign = 1.d0
            endif
            zr(istrxr-1+ncomp*(ip-1)+1)= sign * nx
            zr(istrxr-1+ncomp*(ip-1)+2)= sign * ty
            zr(istrxr-1+ncomp*(ip-1)+3)= sign * tz
            zr(istrxr-1+ncomp*(ip-1)+4)= sign * mx
            zr(istrxr-1+ncomp*(ip-1)+5)= sign * my + tz*xl2
            zr(istrxr-1+ncomp*(ip-1)+6)= sign * mz - ty*xl2
!
        end do
!
    endif
end subroutine
