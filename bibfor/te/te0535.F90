subroutine te0535(option, nomte)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!     Calcul des options FULL_MECA ou RAPH_MECA ou RIGI_MECA_TANG
!     pour les éléments de poutre 'MECA_POU_D_EM'
!
!     'MECA_POU_D_EM' : poutre droite d'EULER multifibre
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=16) :: option, nomte
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lcsovn.h"
#include "asterfort/lonele.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfasseinfo.h"
#include "asterfort/pmfbkb.h"
#include "asterfort/pmfbts.h"
#include "asterfort/pmfdef.h"
#include "asterfort/pmfdge.h"
#include "asterfort/pmfdgedef.h"
#include "asterfort/pmffft.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfite.h"
#include "asterfort/pmfitebkbbts.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfits.h"
#include "asterfort/pmfitsbts.h"
#include "asterfort/pmfmats.h"
#include "asterfort/pmfmcf.h"
#include "asterfort/pmfpti.h"
#include "asterfort/pmftorcor.h"
#include "asterfort/porea1.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptkg00.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nno, nc, nd, nk
    parameter  (nno=2,nc=6,nd=nc*nno,nk=nd*(nd+1)/2)
!
    integer :: igeom, icompo, imate, iorien, iret
    integer :: icarcr, icontm, ideplm, ideplp, imatuu
    integer :: ivectu, icontp, ivarim, ivarip, i
    integer :: jmodfb, jsigfb, jacf, nbvalc
    integer :: jtab(7), ivarmp, istrxp, istrxm
    integer :: ip, jcret, codret, codrep
    integer :: iposcp, iposig, ipomod, iinstp, iinstm
    integer :: icomax, ico, isdcom, ncomp
    integer :: npg, nnoel, ipoids, ivf
!
    real(kind=8) :: e, nu, g, xl, xjx, gxjx, epsm
    real(kind=8) :: pgl(3, 3), fl(nd), klv(nk), sk(nk), rgeom(nk)
    real(kind=8) :: deplm(12), deplp(12), matsec(6)
    real(kind=8) :: xi, wi, b(4), gg, vs(3), ve(12)
    real(kind=8) :: defam(6), defap(6)
    real(kind=8) :: alicom, dalico, ss1, hv, he, minus
    real(kind=8) :: vv(12), fv(12), sv(78)
    real(kind=8) :: gamma, angp(3), sigma(nd), cars1(6)
    real(kind=8) :: a, xiy, xiz, ey, ez
    aster_logical :: vecteu, matric, reactu
    character(len=8) :: mator
    character(len=24) :: valk(2)
!
    real(kind=8), pointer :: defmfib(:) => null()
    real(kind=8), pointer :: defpfib(:) => null()
    real(kind=8), pointer :: gxjxpou(:) => null()
    real(kind=8), pointer :: yj(:) => null(), zj(:) => null() 
    real(kind=8), pointer :: deffibasse(:) => null(), vsigv(:) => null()
    real(kind=8), pointer :: vev(:) => null()
!
    real(kind=8), allocatable :: vfv(:,:), matsecp(:,:), vvp(:,:), skp(:,:)

    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)

    integer :: nbassepou,maxfipoutre
    integer, pointer :: nbfipoutre(:) => null()
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nb_cara = 3
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'EY1','EZ1','JX1'/
!
! --------------------------------------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI', nno=nnoel, npg=npg, jpoids=ipoids, jvf=ivf)
    ASSERT(nno.eq.nnoel)
!   Nombre de composantes des champs PSTRX par points de gauss
    ncomp = 18
!
    codret = 0
    codrep = 0
!   Booleens pratiques
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
! --------------------------------------------------------------------------------------------------
!   Récupération des caractéristiques des fibres et preparation des tableaux dynamiques
    call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug,jacf=jacf,nbassfi=nbassepou)
    AS_ALLOCATE( size=nbassepou , vi= nbfipoutre)
    AS_ALLOCATE(vr=gxjxpou, size=nbassepou)
    call pmfasseinfo(tygrfi,nbfibr,nbcarm,zr(jacf),maxfipoutre, nbfipoutre, gxjxpou)
    AS_ALLOCATE(vr=yj, size=nbassepou)
    AS_ALLOCATE(vr=zj, size=nbassepou)
    AS_ALLOCATE(vr=deffibasse, size=maxfipoutre)
    AS_ALLOCATE(vr=vsigv, size=maxfipoutre)
    AS_ALLOCATE(vr=vev, size=maxfipoutre)

!   Dimension = 2 on doit passer par allocate
    allocate(vfv(7,maxfipoutre))
    allocate(matsecp(6,nbassepou))
    allocate(vvp(12,nbassepou))
    allocate(skp(78,nbassepou))
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCAORIE', 'L', iorien)
    call jevech('PCARCRI', 'L', icarcr)
    call jevech('PDEPLMR', 'L', ideplm)
!   La présence du champ de deplacement a l'instant T+ devrait être conditionné  par l'option
!   (avec RIGI_MECA_TANG ca n'a pas de sens). Cependant ce champ est initialisé a 0 par
!   la routine nmmatr.
    call jevech('PDEPLPR', 'L', ideplp)
    call tecach('OON', 'PCONTMR', 'L', iret, nval=7, itab=jtab)
    icontm = jtab(1)
!
    call jevech('PSTRXMR', 'L', istrxm)
!
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7, itab=jtab)
    ivarim = jtab(1)
!
    if (vecteu) then
        call tecach('OON', 'PVARIMP', 'L', iret, nval=7, itab=jtab)
        ivarmp = jtab(1)
    else
        ivarmp=1
    endif
!
!   Déformations anélastiques
    call r8inir(6, 0.d0, defam, 1)
    call r8inir(6, 0.d0, defap, 1)
!   Paramètres en sortie
    if (option .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imatuu)
        ivarip = ivarim
        icontp = icontm
        istrxp = istrxm
    else if (option.eq.'FULL_MECA') then
        call jevech('PMATUUR', 'E', imatuu)
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PSTRXPR', 'E', istrxp)
    else if (option.eq.'RAPH_MECA') then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PSTRXPR', 'E', istrxp)
    endif
!
! --------------------------------------------------------------------------------------------------
!   Récupération du nombre de fibres total et du nombre de groupes de fibres sur cet élément
!   Verification que c'est bien des multifibres
    call jeexin(zk16(icompo-1+7), iret)
    if (iret .eq. 0) then
        call utmess('F', 'ELEMENTS4_14', sk=nomte)
    endif
!
!   SD_COMPOR où le comportement des groupes de fibres de cet élément est stocké
!   <nom, mater, loi, algo1d, deformation , nbfig> pour chaque groupe dans l'ordre croissant des
!   numéros de groupes
    call jeveuo(zk16(icompo-1+7), 'L', isdcom)
    read (zk16(icompo-1+2),'(I16)') nbvalc
!
!   Recuperation pour assemblage de fibres
!   On reserve quelques places
    AS_ALLOCATE(vr=defmfib, size=nbfibr)
    AS_ALLOCATE(vr=defpfib, size=nbfibr)
    call wkvect('&&TE0535.MODUFIB', 'V V R8', (nbfibr*2), jmodfb)
    call wkvect('&&TE0535.SIGFIB', 'V V R8', (nbfibr*2), jsigfb)
!
!   Longueur de l'élément
    xl = lonele()
!
    if (zk16(icompo+2) .ne. 'PETIT' .and. zk16(icompo+2) .ne. 'GROT_GDEP') then
        valk(1) = zk16(icompo+2)
        valk(2) = nomte
        call utmess('F', 'ELEMENTS3_40', nk=2, valk=valk)
    endif
    reactu = zk16(icompo+2) .eq. 'GROT_GDEP'
!
!   Calcul des matrices de changement de repère
    if (reactu) then
!       Recuperation du 3eme angle nautique au temps T-
        gamma = zr(istrxm+18-1)
!       Calcul de pgl,xl et angp
        call porea1(nno, nc, zr(ideplm), zr(ideplp), zr(igeom),&
                    gamma, vecteu, pgl, xl, angp)
!       Sauvegarde des angles nautiques
        if (vecteu) then
            zr(istrxp+16-1) = angp(1)
            zr(istrxp+17-1) = angp(2)
            zr(istrxp+18-1) = angp(3)
        endif
    else
        call matrot(zr(iorien), pgl)
    endif
!
!   récupération des caractéristiques de section utiles
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
    ey  = vale_cara(1)
    ez  = vale_cara(2)
    xjx = vale_cara(3)
!
!   Caractéristiques élastiques (pas de température pour l'instant)
!   On prend le E et NU du matériau torsion (voir op0059)
    call pmfmats(imate, mator)
    ASSERT( mator.ne.' ')
    call matela(zi(imate), mator, 0, 0.d0, e, nu)
    g = e/ (2.d0* (1.d0+nu))
    gxjx = g*xjx

!   Déplacements dans le repère local
    call utpvgl(nno, nc, pgl, zr(ideplm), deplm)
    call utpvgl(nno, nc, pgl, zr(ideplp), deplp)
    epsm = (deplm(7)-deplm(1))/xl
!   On recupère alpha mode incompatible=alico
    alicom=zr(istrxm-1+15)
!   Mises à zéro
    call r8inir(nk, 0.0d+0, klv, 1)
    call r8inir(nk, 0.0d+0, sk, 1)
    call r8inir(12, 0.0d+0, fl, 1)
    call r8inir(12, 0.0d+0, ve, 1)
    call r8inir(12, 0.0d+0, fv, 1)
!
!   Boucle pour calculer le alpha mode incompatible : alico
    icomax=100
    minus=1.d-6
    ss1=0.0d+0
    dalico=0.0d+0

    do ico = 1, icomax
        he=0.0d+0
        hv=0.0d+0
!       Boucle sur les points de gauss
        do ip = 1, npg
!           Position, poids x jacobien et matrice B et G
            call pmfpti(ip, zr(ipoids), zr(ivf), xl, xi, wi, b, gg)
!           Déformations '-' et increment de deformation par fibre
            call pmfdgedef(tygrfi, b, gg, deplm, alicom, nbfibr, nbcarm, &
                           zr(jacf), nbassepou, maxfipoutre, nbfipoutre, yj, zj, &
                           deffibasse, vfv, defmfib)
            call pmfdgedef(tygrfi, b, gg, deplp, dalico, nbfibr, nbcarm, &
                           zr(jacf), nbassepou, maxfipoutre, nbfipoutre, yj, zj, &
                           deffibasse, vfv, defpfib)

!
            iposig=jsigfb + nbfibr*(ip-1)
            ipomod=jmodfb + nbfibr*(ip-1)
!           Module et contraintes sur chaque fibre (comportement)
            call pmfmcf(ip, nbgrfi, nbfibr, nug, zk24(isdcom),&
                        zr(icarcr), option, zr(iinstm), zr(iinstp), zi(imate),&
                        nbvalc, defam, defap, zr(ivarim), zr(ivarmp),&
                        zr(icontm), defmfib, defpfib, epsm, zr(ipomod),&
                        zr(iposig), zr(ivarip), codrep)
!
            if (codrep .ne. 0) then
                codret = codrep
!               code 3: on continue et on le renvoie a la fin, autre codes sortie immediate
                if (codrep .ne. 3) goto 999
            endif
!           calcul matrice section
            call pmfite(tygrfi, nbfibr, nbcarm, zr(jacf), zr(ipomod), matsec)
!           Integration des contraintes sur la section
            call pmfits(tygrfi, nbfibr, nbcarm, zr(jacf), zr(iposig), vs)
!           Calculs mode incompatible hv=int(gt ks g), he=int(gt fs)
            hv = hv+wi*gg*gg*matsec(1)
            he = he+wi*gg*vs(1)
        enddo
!       Fin boucle points de gauss
!       Encore un peu de mode incompatible
        if (abs(hv) .le. r8prem()) then
            call utmess('F', 'ELEMENTS_8')
        endif
        dalico = dalico-he/hv
        if (ico .eq. 1) then
            if (abs(vs(1)) .le. minus) then
                goto 710
            else
                ss1 = abs(vs(1))
            endif
        endif
        if (abs(he) .le. (ss1*minus)) then
            goto 710
        endif
    enddo
!   Fin boucle calcul alico
710 continue
!
!   Quand on a convergé sur alico, on peut intégrer sur l'élément
    do ip = 1, npg
        call pmfpti(ip, zr(ipoids), zr(ivf), xl, xi, wi, b, gg)
!       Calcul la matrice elementaire (sauf pour RAPH_MECA)
        if (option .ne. 'RAPH_MECA') then
            ipomod = jmodfb + nbfibr*(ip-1)
!           Calcul des caracteristiques de section par integration sur les fibres
            call pmfitebkbbts(tygrfi, nbfibr, nbcarm, zr(jacf), zr(ipomod), b, wi, gxjx, gxjxpou, &
                              g, gg, nbassepou, maxfipoutre, nbfipoutre, vev, yj, zj, &
                              vfv, skp, sk, vv, vvp)
            do i = 1, nk
                klv(i) = klv(i)+sk(i)
            enddo

            do i = 1, 12
                fv(i) = fv(i)+vv(i)
            enddo

        endif
!       si pas RIGI_MECA_TANG, on calcule les forces internes
        if (option .ne. 'RIGI_MECA_TANG') then
            iposig=jsigfb + nbfibr*(ip-1)
            call pmfitsbts(tygrfi, nbfibr, nbcarm, zr(jacf), zr(iposig), b, wi, &
                           nbassepou, yj, zj, maxfipoutre, nbfipoutre, vsigv, vfv, vvp, ve)
            do i = 1, 12
                fl(i) = fl(i)+ve(i)
            enddo
        endif
    enddo
!   On modifie la matrice de raideur par condensation statique
    if (option .ne. 'RAPH_MECA') then
        call pmffft(fv, sv)
        do i = 1, nk
            klv(i) = klv(i) - sv(i)/hv
        enddo
    endif
!
!   Torsion a part pour les forces interne
    call pmftorcor(tygrfi, nbassepou, gxjx, gxjxpou, deplm, deplp, xl, fl)

!   Stockage des efforts généralisés et passage des forces en repère local
    if (vecteu) then
!       on sort les contraintes sur chaque fibre
        do ip = 1, 2
            iposcp=icontp + nbfibr*(ip-1)
            iposig=jsigfb + nbfibr*(ip-1)
            do i = 0, nbfibr-1
                zr(iposcp+i) = zr(iposig+i)
            enddo
!           Stockage des forces intégrées
            zr(istrxp-1+ncomp*(ip-1)+1) = fl(6*(ip-1)+1)
            zr(istrxp-1+ncomp*(ip-1)+2) = fl(6*(ip-1)+2)
            zr(istrxp-1+ncomp*(ip-1)+3) = fl(6*(ip-1)+3)
            zr(istrxp-1+ncomp*(ip-1)+4) = fl(6*(ip-1)+4)
            zr(istrxp-1+ncomp*(ip-1)+5) = fl(6*(ip-1)+5)
            zr(istrxp-1+ncomp*(ip-1)+6) = fl(6*(ip-1)+6)
            zr(istrxp-1+ncomp*(ip-1)+15)= alicom+dalico
        enddo
        call utpvlg(nno, nc, pgl, fl, zr(ivectu))
    endif
!
!   Calcul de la matrice de rigidité géométrique
    if (matric .and. reactu) then
        do i = 1, nc
            sigma(i) = - zr(istrxp+i-1)
            sigma(i+nc) = zr(istrxp+ncomp+i-1)
        enddo
        call r8inir(nk, 0.0d+0, rgeom, 1)
        call pmfitg(tygrfi, nbfibr, nbcarm, zr(jacf), cars1)
        a   = cars1(1)
        xiy = cars1(5)
        xiz = cars1(4)
        call ptkg00(sigma, a, a, xiz, xiz, xiy, xiy, xl, ey, ez, rgeom)
        call lcsovn(nk, klv, rgeom, klv)
    endif
!   On sort la matrice de rigidité tangente
    if (matric) then
        call utpslg(nno, nc, pgl, klv, zr(imatuu))
    endif
!
999 continue
!   Sortie propre : code retour et libération des ressources
    if (vecteu) then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
!   Deallocation memoire pour tableaux temporaires
    AS_DEALLOCATE(vi=nbfipoutre)
    AS_DEALLOCATE(vr=defmfib)
    AS_DEALLOCATE(vr=defpfib)
    AS_DEALLOCATE(vr=gxjxpou)
    AS_DEALLOCATE(vr=yj)
    AS_DEALLOCATE(vr=zj)
    AS_DEALLOCATE(vr=deffibasse)
    AS_DEALLOCATE(vr=vsigv)
    AS_DEALLOCATE(vr=vev)
    deallocate(vfv,matsecp,vvp,skp)
    call jedetr('&&TE0535.MODUFIB')
    call jedetr('&&TE0535.SIGFIB')
end subroutine
