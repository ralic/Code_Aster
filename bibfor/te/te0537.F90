subroutine te0537(option, nomte)
!
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
!
! --------------------------------------------------------------------------------------------------
!                   POU_D_EM : POUTRE MULTIFIBRE EULER BERNOULLI
!
!   Calcul de l'option EPSI_ELGA
!       - déformations dans les fibres (sous pts de gauss) à partir des déplacements
!   Calcul de l'option SIEF_ELGA
!       - contrainte dans les fibres (sous pts de gauss) comportement linéaire
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=16) :: option, nomte
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/pmavec.h"
#include "asterfort/pmfdef.h"
#include "asterfort/pmfdge.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfitx.h"
#include "asterfort/pmfpti.h"
#include "asterfort/pmfrig.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jcont, lorien, jdepl, imate, nno, nc, i, iret
    integer :: ip, ipos, istrxr, ipos1, ipos2, nbfig, ig, icp, isdcom, icompo
    integer :: codres(2), ncomp
    integer :: npg, ndim, nnoel, nnos, ipoids, ivf
    integer :: jacf, jtab(7)
    parameter (nno=2,nc=6)
    real(kind=8) :: ul(12), pgl(3, 3), dege(6), xl, e, nu
    real(kind=8) :: g
    real(kind=8) :: casect(6)
    real(kind=8) :: coa, cob, ex12, ex13
    real(kind=8) :: b(4), gg, xi, wi, valres(2), alpha
    real(kind=8) :: klv(78),klc(12,12) ,effo(12)
    character(len=8) :: materi
    character(len=16) :: ch16, nomres(2)
!
    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)
!
! --------------------------------------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nnoel,nnos=nnos,&
                     npg=npg,jpoids=ipoids,jvf=ivf)
    ASSERT(nno.eq.nnoel)
! --------------------------------------------------------------------------------------------------
!   Récupération des caractéristiques des fibres
    call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug)
    call jevech('PFIBRES', 'L', jacf)
!
    alpha = 0.d0
    call jevech('PCAORIE', 'L', lorien)
    call jevech('PDEPLAR', 'L', jdepl)
!   Recuperation des coordonnees des noeuds
    xl = lonele()
!
!   Recuperation des orientations
    call matrot(zr(lorien), pgl)
!   Passage des deplacements dans le repere local
    call utpvgl(nno, nc, pgl, zr(jdepl), ul)
!   Nombre de composantes des champs PSTRX par points de gauss
    ncomp = 18
!
    jcont = -1
    if (option .eq. 'EPSI_ELGA') then
        call tecach('OON', 'PDEFOPG', 'E', iret, nval=7, itab=jtab)
        jcont = jtab(1)
    else if (option.eq.'SIEF_ELGA') then
        call jevech('PMATERC', 'L', imate)
        call tecach('OON', 'PCONTRR', 'E', iret, nval=7, itab=jtab)
        jcont = jtab(1)
    else if (option.eq.'STRX_ELGA') then
        call jevech('PMATERC', 'L', imate)
        call jevech('PSTRXRR', 'E', istrxr)
!       Si excentrecement calcul de alpha appel intégration sur section et calcul G torsion
        call pmfitx(zi(imate),1,casect,g)
        if ( ( casect(2).ge.r8prem() ).or.( casect(3).ge.r8prem() ) ) then
            coa = 3.0d0/2.0d0/xl
            cob = 3.0d0/4.0d0
            ex13=casect(2)/casect(1)
            ex12=casect(3)/casect(1)
            alpha = coa*ex13*ul(2)-coa*ex12*ul(3)&
                   +cob*ex12*ul(5)+cob*ex13*ul(6)&
                   -coa*ex13*ul(8)+coa*ex12*ul(9)&
                   +cob*ex12*ul(11)+cob*ex13*ul(12)
        else
            alpha=0.0d0
        endif
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
!   si option EPSI_ELGA ou SIEF_ELGA boucle sur les points de gauss
    if (option .ne. 'STRX_ELGA') then
!       alpha modes incompatibles
        call jevech('PSTRXRR', 'L', istrxr)
        alpha=zr(istrxr-1+15)
        do ip = 1, npg
!           Matrice B puis DEGE puis déformations sur les fibres
            call pmfpti(ip, zr(ipoids), zr(ivf), xl, xi, wi, b, gg)
            call pmfdge(b, gg, ul, alpha, dege)
            ipos=jcont+nbfibr*(ip-1)
            call pmfdef(tygrfi, nbfibr, nbcarm, zr(jacf), dege, zr(ipos))
        enddo
    endif
!   Si EPSI_ELGA : jcont est l'adresse PDEFORR, on sort
    if (option .eq. 'SIEF_ELGA') then
!       Si option sief_elga on continue
!       Récupération des différents matériaux dans SDCOMP dans COMPOR
        call jevech('PCOMPOR', 'L', icompo)
        call jeveuo(zk16(icompo-1+7), 'L', isdcom)
!       boucle sur les groupes de fibre
        ipos1=jcont-1
        ipos2=ipos1+nbfibr
        do ig = 1, nbgrfi
            icp=isdcom-1+(nug(ig)-1)*6
            read(zk24(icp+6),'(I24)')nbfig
            materi=zk24(icp+2)(1:8)
!           On multiplie par E (constant sur le groupe)
            nomres(1) = 'E'
            nomres(2) = 'NU'
            call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                        materi, 'ELAS', 0, ' ', [0.d0],&
                        2, nomres, valres, codres, 1)
            e = valres(1)
            nu = valres(2)
!           On multiplie les zr(jcont) (déformations) par E pour avoir des contraintes
            do i = 1, nbfig
                zr(ipos1+i)=zr(ipos1+i) * e
                zr(ipos2+i)=zr(ipos2+i) * e
            enddo
            ipos1=ipos1+nbfig
            ipos2=ipos2+nbfig
        enddo
    endif
!
    if (option .eq. 'STRX_ELGA') then
!       Calcul des efforts généralisés (cas élastique) on fait kele*ul
        call pmfrig(nomte,zi(imate),klv)
        call vecma ( klv, 78, klc, 12 )
        call pmavec('ZERO', 12, klc, ul, effo)
        do ip = 1, npg
            zr(istrxr-1+ncomp*(ip-1)+1)= effo(6*(ip-1)+1)
            zr(istrxr-1+ncomp*(ip-1)+2)= effo(6*(ip-1)+2)
            zr(istrxr-1+ncomp*(ip-1)+3)= effo(6*(ip-1)+3)
            zr(istrxr-1+ncomp*(ip-1)+4)= effo(6*(ip-1)+4)
            zr(istrxr-1+ncomp*(ip-1)+5)= effo(6*(ip-1)+5)
            zr(istrxr-1+ncomp*(ip-1)+6)= effo(6*(ip-1)+6)
            zr(istrxr-1+ncomp*(ip-1)+15)= alpha
        enddo
    endif
end subroutine
