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
#include "asterfort/pmavec.h"
#include "asterfort/pmfdef.h"
#include "asterfort/pmfdge.h"
#include "asterfort/pmfitx.h"
#include "asterfort/pmfpti.h"
#include "asterfort/pmfrig.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
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
    integer :: jcont, lorien, jdepl, imate, lx, nno, nc, i, iret
    parameter (nno=2,nc=6)
    character(len=16) :: ch16
    real(kind=8) :: ul(12), pgl(3, 3), dege(6), xl, e, nu
    real(kind=8) :: g
    integer :: nbfib, ncarfi, jacf, jtab(7)
    real(kind=8) :: casect(6)
    real(kind=8) :: coa, cob, ex12, ex13
    real(kind=8) :: zero, deux, trois, quatre
    parameter (zero=0.0d+0,deux=2.d+0,trois=3.d+0)
    parameter (quatre=4.d+0)
    real(kind=8) :: b(4), gg, xi, wi, valres(2), sign_noeu, alpha
    integer :: ip, ipos, iadzi, iazk24, istrxr
    integer :: ipos1, ipos2, nbfig, nbgf, ig, nugf, ifb, icp, isdcom, icompo
    character(len=8) :: materi, nomres(2), nomail
    integer :: codres(2), ncomp
    integer :: npg, ndim, nnoel, nnos, ipoids, ivf, iplouf
    real(kind=8) :: klv(78),klc(12,12) ,effo(12)
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
    alpha = 0.d0
    call jevech('PGEOMER', 'L', lx)
    call jevech('PCAORIE', 'L', lorien)
    call jevech('PDEPLAR', 'L', jdepl)
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
!   --- SI EXCENTRICITE CALCUL DE ALPHA
!   --- APPEL INTEGRATION SUR SECTION ET CALCUL G TORSION
        call pmfitx(zi(imate),1,casect,g)
        if(casect(2).ne.zero.or.casect(3).ne.zero)then
            coa = trois/deux/xl
            cob = trois/quatre
            ex13=casect(2)/casect(1)
            ex12=casect(3)/casect(1)
            alpha = coa*ex13*ul(2)-coa*ex12*ul(3)&
                   +cob*ex12*ul(5)+cob*ex13*ul(6)&
                   -coa*ex13*ul(8)+coa*ex12*ul(9)&
                   +cob*ex12*ul(11)+cob*ex13*ul(12)
        else
            alpha=zero
        endif
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
! --- SI OPTION EPSI_ELGA OU SIEF_ELGA
!
! --- BOUCLE SUR LES POINTS DE GAUSS
    if (option .ne. 'STRX_ELGA') then
!       alpha modes incompatibles
        call jevech('PSTRXRR', 'L', istrxr)
        alpha=zr(istrxr-1+15)
        do ip = 1, npg
!        ---  MATRICE B PUIS DEGE PUIS DEFORMATIONS SUR LES FIBRES
            call pmfpti(ip, zr(ipoids), zr(ivf), xl, xi,&
                        wi, b, gg)
            call pmfdge(b, gg, ul, alpha, dege)
            ipos=jcont+nbfib*(ip-1)
            call pmfdef(nbfib, ncarfi, zr(jacf), dege, zr(ipos))
        enddo
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
        do ig = 1, nbgf
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
            do i = 1, nbfig
                zr(ipos1+i)=zr(ipos1+i) * e
                zr(ipos2+i)=zr(ipos2+i) * e
            enddo
            ipos1=ipos1+nbfig
            ipos2=ipos2+nbfig
        enddo
    endif
!
!
    if (option .eq. 'STRX_ELGA') then
!
! ---   CALCUL DES EFFORTS GENERALISES (CAS ELASTIQUE)
! ---   ON FAIT KELE*UL
        call pmfrig(nomte,zi(imate),klv)
        call vecma ( klv, 78, klc, 12 )
        call pmavec('ZERO', 12, klc, ul, effo)

        do ip = 1, npg
            if (ip .eq. 1) then
                sign_noeu = -1.d0
            else
                sign_noeu = 1.d0
            endif
            zr(istrxr-1+ncomp*(ip-1)+1)= effo(6*(ip-1)+1)
            zr(istrxr-1+ncomp*(ip-1)+2)= effo(6*(ip-1)+2)
            zr(istrxr-1+ncomp*(ip-1)+3)= effo(6*(ip-1)+3)
            zr(istrxr-1+ncomp*(ip-1)+4)= effo(6*(ip-1)+4)
            zr(istrxr-1+ncomp*(ip-1)+5)= effo(6*(ip-1)+5)
            zr(istrxr-1+ncomp*(ip-1)+6)= effo(6*(ip-1)+6)
            zr(istrxr-1+ncomp*(ip-1)+15)= alpha
!
        end do
!
    endif
end subroutine
