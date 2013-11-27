subroutine te0535(option, nomte)
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
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lcsovn.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfbkb.h"
#include "asterfort/pmfbts.h"
#include "asterfort/pmfdef.h"
#include "asterfort/pmfdge.h"
#include "asterfort/pmffft.h"
#include "asterfort/pmfite.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfits.h"
#include "asterfort/pmfmcf.h"
#include "asterfort/pmfpti.h"
#include "asterfort/porea1.h"
#include "asterfort/ptkg00.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/wkvect.h"
!
!
    character(len=16) :: option, nomte
! --- ------------------------------------------------------------------
!
!     CALCUL DES OPTIONS FULL_MECA OU RAPH_MECA OU RIGI_MECA_TANG
!     POUR LES ELEMENTS DE POUTRE 'MECA_POU_D_EM'
!
!     'MECA_POU_D_EM' : POUTRE DROITE D'EULER MULTIFIBRES
!
! --- ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!     NOMTE  : K16 : NOM DU TYPE ELEMENT
! --- ------------------------------------------------------------------
    integer :: igeom, icompo, imate, isect, iorien, nd, nk, iret
    integer :: icarcr, icontm, ideplm, ideplp, imatuu, isecan
    integer :: ivectu, icontp, nno, nc, ivarim, ivarip, i, isicom
    parameter  (nno=2,nc=6,nd=nc*nno,nk=nd*(nd+1)/2)
    real(kind=8) :: e, nu, g, xl, xjx, gxjx, epsm
    integer :: lx
    real(kind=8) :: pgl(3, 3), fl(nd), klv(nk), sk(nk), rgeom(nk)
    real(kind=8) :: deplm(12), deplp(12), matsec(6), dege(6)
    real(kind=8) :: zero, deux
    integer :: jdefm, jdefp, jmodfb, jsigfb, nbfib, ncarfi, jacf, nbvalc
    integer :: jtab(7), ivarmp, istrxp, istrxm
    integer :: ip, inbf, jcret, codret, codrep
    integer :: iposcp, iposig, ipomod, iinstp, iinstm
    integer :: icomax, ico, nbgf, isdcom, nbgfmx,ncomp
    integer :: npg, ndim, nnoel, nnos, ipoids, ivf, iplouf
    real(kind=8) :: xi, wi, b(4), gg, vs(3), ve(12)
    real(kind=8) :: defam(6), defap(6)
    real(kind=8) :: alicom, dalico, ss1, hv, he, minus, xls2
    real(kind=8) :: vv(12), fv(12), sv(78), ksg(3), sign, my, mz
    real(kind=8) :: gamma, angp(3), sigma(nd), cars1(6)
    real(kind=8) :: a, xiy, xiz, ey, ez
    logical :: vecteu, matric, reactu
    character(len=8) :: mator
    character(len=24) :: valk(2)
    parameter  (zero=0.0d+0,deux=2.d+0)
!
! --- ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nnoel, nnos,&
                npg, ipoids, ivf, iplouf, iplouf)
    ASSERT(nno.eq.nnoel)
!   NOMBRE DE COMPOSANTES DES CHAMPS PSTRX? PAR POINTS DE GAUSS
    ncomp = 18
!
    ncarfi = 3
    codret = 0
    codrep = 0
!
! --- BOOLEENS PRATIQUES
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
    call jevech('PNBSP_I', 'L', inbf)
    call jevech('PFIBRES', 'L', jacf)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCAGNPO', 'L', isect)
    call jevech('PCAORIE', 'L', iorien)
    call jevech('PCARCRI', 'L', icarcr)
    call jevech('PDEPLMR', 'L', ideplm)
! --- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
!     DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
!     CA N A PAS DE SENS).
!     CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
    call jevech('PDEPLPR', 'L', ideplp)
    call tecach('OON', 'PCONTMR', 'L', iret, nval=7,&
                itab=jtab)
    icontm = jtab(1)
!
    call jevech('PSTRXMR', 'L', istrxm)
!
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    ivarim = jtab(1)
!
    if (vecteu) then
        call tecach('OON', 'PVARIMP', 'L', iret, nval=7,&
                    itab=jtab)
        ivarmp = jtab(1)
    else
        ivarmp=1
    endif
!
! --- DEFORMATIONS ANELASTIQUES
    call r8inir(6, 0.d0, defam, 1)
    call r8inir(6, 0.d0, defap, 1)
! --- PARAMETRES EN SORTIE
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
! --- ------------------------------------------------------------------
! --- RECUPERATION DU NOMBRE DE FIBRES TOTAL DE L'ELEMENT
!     ET DU NOMBRE DE GROUPES DE FIBRES SUR CET ELEMENT
    nbfib = zi(inbf)    
    nbgf = zi(inbf+1)
!
! --- VERIFICATION QUE C'EST BIEN DES MULTIFIBRES
    call jeexin(zk16(icompo-1+7), iret)
    if (iret .eq. 0) then
        call utmess('F', 'ELEMENTS4_14', sk=nomte)
    endif
!
! --- RECUPERATION DE LA SD_COMPOR OU LE COMPORTEMENT DES GROUPES DE
!     FIBRES DE CET ELEMENT EST STOCKE
!     (NOM, MATER, LOI, ALGO1D, DEFORMATION NBFIG) POUR CHAQUE GROUPE
!     DANS L'ORDRE CROISSANT DE NUMEROS DE GROUPES)
    call jeveuo(zk16(icompo-1+7), 'L', isdcom)
    read (zk16(icompo-1+2),'(I16)') nbvalc
!
! --- ON RESERVE QUELQUES PLACES
    call wkvect('&&TE0535.DEFMFIB', 'V V R8', nbfib, jdefm)
    call wkvect('&&TE0535.DEFPFIB', 'V V R8', nbfib, jdefp)
    call wkvect('&&TE0535.MODUFIB', 'V V R8', (nbfib*2), jmodfb)
    call wkvect('&&TE0535.SIGFIB', 'V V R8', (nbfib*2), jsigfb)
!
!
! --- LONGUEUR DE L'ELEMENT
    lx = igeom - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2+ (zr(lx+5)-zr(lx+2))**2+ (zr(lx+6)-zr(lx+3))**2 )
    if (xl .eq. zero) then
        call utmess('F', 'ELEMENTS_17')
    endif
!
    if (zk16(icompo+2) .ne. 'PETIT' .and. zk16(icompo+2) .ne. 'GROT_GDEP') then
        valk(1) = zk16(icompo+2)
        valk(2) = nomte
        call utmess('F', 'ELEMENTS3_40', nk=2, valk=valk)
    endif
    reactu = zk16(icompo+2) .eq. 'GROT_GDEP'
!
!   CALCUL DES MATRICES DE CHANGEMENT DE REPERE
!
    if (reactu) then
!
!        RECUPERATION DU 3EME ANGLE NAUTIQUE AU TEMPS T-
        gamma = zr(istrxm+18-1)
!
!       CALCUL DE PGL,XL ET ANGP
        call porea1(nno, nc, zr(ideplm), zr(ideplp), zr(igeom),&
                    gamma, vecteu, pgl, xl, angp)
!
!        SAUVEGARDE DES ANGLES NAUTIQUES
        if (vecteu) then
            zr(istrxp+16-1) = angp(1)
            zr(istrxp+17-1) = angp(2)
            zr(istrxp+18-1) = angp(3)
        endif
!
    else
        call matrot(zr(iorien), pgl)
    endif
!
! --- CARACTERISTIQUES ELASTIQUES (PAS DE TEMPERATURE POUR L'INSTANT)
!     ON PREND LE E ET NU DU MATERIAU TORSION (VOIR OP0059)
    call jeveuo(zk16(icompo-1+7)(1:8)//'.CPRI', 'L', isicom)
    nbgfmx = zi(isicom+2)
    mator = zk24(isdcom-1+nbgfmx*6+1)(1:8)
    call matela(zi(imate), mator, 0, 0.d0, e,&
                nu)
    g = e/ (2.d0* (1.d0+nu))
! --- TORSION A PART
    xjx = zr(isect+7)
    gxjx = g*xjx
!
! --- DEPLACEMENTS DANS LE REPERE LOCAL
    call utpvgl(nno, nc, pgl, zr(ideplm), deplm)
    call utpvgl(nno, nc, pgl, zr(ideplp), deplp)
    epsm = (deplm(7)-deplm(1))/xl
! --- ON RECUPERE ALPHA MODE INCOMPATIBLE=ALICO
    alicom=zr(istrxm-1+15)
!
! --- MISES A ZERO
    call r8inir(nk, zero, klv, 1)
    call r8inir(nk, zero, sk, 1)
    call r8inir(12, zero, fl, 1)
    call r8inir(12, zero, fv, 1)
!
! --- BOUCLE POUR CALCULER LE ALPHA MODE INCOMPATIBLE : ALICO
    icomax=100
    minus=1.d-6
    ss1=zero
    dalico=zero
    do ico = 1, icomax
        he=zero
        hv=zero
! ---    BOUCLE SUR LES POINTS DE GAUSS
        do 500 ip = 1, npg
! ---       POSITION, POIDS X JACOBIEN ET MATRICE B ET G
            call pmfpti(ip, zr(ipoids), zr(ivf), xl, xi,&
                        wi, b, gg)
            print*,'xi ',xi
            print*,'wi ',wi
            print*,'b ',b
            print*,'gg ',gg
! ---       DEFORMATIONS '-' ET INCREMENT DE DEFORMATION PAR FIBRE
!           MOINS --> M
            call pmfdge(b, gg, deplm, alicom, dege)
            call pmfdef(nbfib, ncarfi, zr(jacf), dege, zr(jdefm))
!  --       INCREMENT --> P
            call pmfdge(b, gg, deplp, dalico, dege)
            call pmfdef(nbfib, ncarfi, zr(jacf), dege, zr(jdefp))
!
            iposig=jsigfb + nbfib*(ip-1)
            ipomod=jmodfb + nbfib*(ip-1)
! ---       MODULE ET CONTRAINTES SUR CHAQUE FIBRE (COMPORTEMENT)
            call pmfmcf(ip, nbgf, nbfib, zi(inbf+2), zk24(isdcom),&
                        zr(icarcr), option, zr(iinstm), zr(iinstp), zi(imate),&
                        nbvalc, defam, defap, zr(ivarim), zr(ivarmp),&
                        zr(icontm), zr( jdefm), zr(jdefp), epsm, zr(ipomod),&
                        zr(iposig), zr(ivarip), isecan, codrep)
!
            if (codrep .ne. 0) then
                codret = codrep
!              CODE 3: ON CONTINUE ET ON LE RENVOIE A LA FIN
!              AUTRE CODES: SORTIE IMMEDIATE
                if (codrep .ne. 3) goto 900
            endif
! ---       CALCUL MATRICE SECTION
            call pmfite(nbfib, ncarfi, zr(jacf), zr(ipomod), matsec)
! ---       INTEGRATION DES CONTRAINTES SUR LA SECTION
            call pmfits(nbfib, ncarfi, zr(jacf), zr(iposig), vs)
! ---       CALCULS MODE INCOMPATIBLE HV=INT(GT KS G), HE=INT(GT FS)
            hv = hv+wi*gg*gg*matsec(1)
            he = he+wi*gg*vs(1)
500      continue
! ---    FIN BOUCLE POINTS DE GAUSS
! ---    ENCORE UN PEU DE MODE INCOMPATIBLE
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
    end do
! --- FIN BOUCLE CALCUL ALICO
710  continue
!
! --- QUAND ON A CONVERGE SUR ALICO, ON PEUT INTEGRER SUR L'ELEMENT
    do ip = 1, npg
        call pmfpti(ip, zr(ipoids), zr(ivf), xl, xi,&
                     wi, b, gg)
! ---    CALCUL LA MATRICE ELEMENTAIRE (SAUF POUR RAPH_MECA)
        if (option .ne. 'RAPH_MECA') then
            ipomod = jmodfb + nbfib*(ip-1)
! ---       CALCUL DES CARACTERISTIQUES DE SECTION PAR INTEGRATION
!           SUR LES FIBRES
            call pmfite(nbfib, ncarfi, zr(jacf), zr(ipomod), matsec)
!
            call pmfbkb(matsec, b, wi, gxjx, sk)
            do 320 i = 1, nk
                klv(i) = klv(i)+sk(i)
320          continue
! ---       ON SE SERT DE PMFBTS POUR CALCULER BT,KS,G. G EST SCALAIRE
            ksg(1) = matsec(1)*gg
            ksg(2) = matsec(2)*gg
            ksg(3) = matsec(3)*gg
            call pmfbts(b, wi, ksg, vv)
            do 340 i = 1, 12
                fv(i) = fv(i)+vv(i)
340          continue
        endif
! ---    SI PAS RIGI_MECA_TANG, ON CALCULE LES FORCES INTERNES
        if (option .ne. 'RIGI_MECA_TANG') then
            iposig=jsigfb + nbfib*(ip-1)
            call pmfits(nbfib, ncarfi, zr(jacf), zr(iposig), vs)
            call pmfbts(b, wi, vs, ve)
            do 360 i = 1, 12
                fl(i) = fl(i)+ve(i)
360          continue
        endif
    end do
! --  ON MODIFIE LA MATRICE DE RAIDEUR PAR CONDENSATION STATIQUE
    if (option .ne. 'RAPH_MECA') then
        call pmffft(fv, sv)
        do 380 i = 1, nk
            klv(i) = klv(i) - sv(i)/hv
380      continue
    endif
!
! --- TORSION A PART POUR LES FORCES INTERNE
    fl(10) = gxjx*(deplm(10)+deplp(10)-deplm(4)-deplp(4))/xl
    fl(4) = -fl(10)
!
!   STOCKAGE DES EFFORTS GENERALISES ET PASSAGE DES FORCES EN REP LOCAL
    if (vecteu) then
        xls2 = xl/2.d0
        my = (fl(11)-fl(5))/deux
        mz = (fl(12)-fl(6))/deux
! ---    ON SORT LES CONTRAINTES SUR CHAQUE FIBRE
        do 310 ip = 1, 2
            iposcp=icontp + nbfib*(ip-1)
            iposig=jsigfb + nbfib*(ip-1)
            do 300 i = 0, nbfib-1
                zr(iposcp+i) = zr(iposig+i)
300          continue
!           STOCKAGE DES FORCES INTEGREES
            if (ip .eq. 1) then
                sign = - 1.d0
            else
                sign = 1.d0
            endif
            zr(istrxp-1+ncomp*(ip-1)+1) = fl(6*(ip-1)+1)
            zr(istrxp-1+ncomp*(ip-1)+2) = fl(6*(ip-1)+2)
            zr(istrxp-1+ncomp*(ip-1)+3) = fl(6*(ip-1)+3)
            zr(istrxp-1+ncomp*(ip-1)+4) = fl(6*(ip-1)+4)
            zr(istrxp-1+ncomp*(ip-1)+5) = sign *( my + fl(6*(ip-1)+3)*xls2)
            zr(istrxp-1+ncomp*(ip-1)+6) = sign *( mz - fl(6*(ip-1)+2)*xls2)
310      continue
        call utpvlg(nno, nc, pgl, fl, zr(ivectu))
        zr(istrxp-1+15)=alicom+dalico
        zr(istrxp-1+ncomp+15)=alicom+dalico
    endif
!
!   CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE
    if (matric .and. reactu) then
!
        do 15 i = 1, nc
            sigma(i) = - zr(istrxp+i-1)
            sigma(i+nc) = zr(istrxp+ncomp+i-1)
15      continue
!
        call r8inir(nk, zero, rgeom, 1)
        call pmfitg(nbfib, 3, zr(jacf), cars1)
        a = cars1(1)
        xiy = cars1(5)
        xiz = cars1(4)
        ey = -zr(isect+5)
        ez = -zr(isect+6)
        call ptkg00(sigma, a, a, xiz, xiz,&
                    xiy, xiy, xl, ey, ez,&
                    rgeom)
        call lcsovn(nk, klv, rgeom, klv)
    endif
!
    if (matric) then
! ---    ON SORT LA MATRICE DE RIGIDITE TANGENTE
        call utpslg(nno, nc, pgl, klv, zr(imatuu))
    endif
!
!
900  continue
! --- SORTIE PROPRE: CODE RETOUR ET LIBERATION DES RESSOURCES
    if (vecteu) then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
    call jedetr('&&TE0535.DEFMFIB')
    call jedetr('&&TE0535.DEFPFIB')
    call jedetr('&&TE0535.MODUFIB')
    call jedetr('&&TE0535.SIGFIB')
end subroutine
