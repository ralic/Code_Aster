subroutine te0036(option, nomte)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT EN PRESSION REPARTIE
!          SUR DES FACES D'ELEMENTS X-FEM
!          (LA PRESSION PEUT ETRE DONNEE SOUS FORME D'UNE FONCTION)
!
!          OPTIONS : 'CHAR_MECA_PRES_R'
!                    'CHAR_MECA_PRES_F'
!                    'CHAR_MECA_FR2D3D'
!                    'CHAR_MECA_FR1D2D'
!                    'CHAR_MECA_FF2D3D'
!                    'CHAR_MECA_FF1D2D'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/abscvf.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dismoi.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/iselli.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/reereg.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/tefrep.h"
#include "asterfort/vecini.h"
#include "asterfort/xdeffe.h"
#include "asterfort/xteddl.h"
#include "blas/ddot.h"
!
    character(len=8) :: nompar(4), noma, elrefp, elrese(4), enr, lag
    character(len=8) :: k8bid, elref
    character(len=16) :: nomte, option
    integer :: jpintt, jcnset, jheavt, jlonch, jlsn, jlst, k
    integer :: jpmilt, irese, nfiss, ifiss, jfisno, jtab(7), ncomp
    integer :: ibid, ier, ndim, nno, nnop, nnops, npg, nnos, kpg
    integer :: ipoids, ivf, idfde, igeom, ipres, itemps, ires, i, j
    integer :: nfh, nfe, nse, ise
    integer :: in, ino, iadzi, iazk24, jstno
    integer :: iforc, iret, ig, pos, ndime, nddl, ddls
    real(kind=8) :: y(3), xg(4), rbid, fe(4), xe(2), lsng, lstg, rg, tg
    real(kind=8) :: pres, ff(27), a(3), b(3), c(3), ab(3), ac(3), coorse(81)
    real(kind=8) :: nd(3), norme, nab, rb1(3), rb2(3), gloc(2), n(3), cisa
    real(kind=8) :: an(3), poids, forrep(3), vf, r, coorlo(6), geomlo(81)
    logical :: lbid, axi
    real(kind=8) :: rb3, rb4, ksib, ksig, dx, dy, dff(1, 3), seg(3), jac,mat(1)
    integer :: kk
    data          elrese /'SE2','TR3','SE3','TR3'/
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
!     ELEMENT DE REFERENCE PARENT
    call elref1(elrefp)
    call elref4(' ', 'RIGI', ndime, nnop, nnops,&
                ibid, ibid, ibid, ibid, ibid)
    ASSERT(ndime.eq.1.or.ndime.eq.2)
!
    axi = lteatt(' ','AXIS','OUI')
!
!     DIMENSION DE L'ESPACE
    call tecael(iadzi, iazk24)
    noma=zk24(iazk24)(1:8)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8bid, iret)
!
!     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
!     NDIM EST LA DIMENSION DU MAILLAGE
!     NDIME EST DIMENSION DE L'ELEMENT FINI
!     SUR UN ELET DE BORD, ON A :  NDIM = NDIME + 1
!
!     SOUS-ELEMENT DE REFERENCE
    if (.not.iselli(elrefp) .and. ndim .le. 2) then
        irese=2
    else
        irese=0
    endif
    elref=elrese(ndime+irese)
    call elref4(elref, 'RIGI', ibid, nno, nnos,&
                npg, ipoids, ivf, idfde, ibid)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
!     IL NE FAUT PAS APPELER XTEINI CAR IL NE GERE PAS LES ELEMENTS
!     DE BORD
!      CALL XTEINI(NOMTE,DDLH,NFE,IBID,IBID,IBID,IBID,IBID)
    nfe = 0
    nfh = 0
    nfiss = 1
    ifiss = 1
    call teattr(nomte, 'S', 'XFEM', enr, ier)
    if (enr(1:2) .eq. 'XH') then
! --- NOMBRE DE FISSURES
        call tecach('NOO', 'PHEAVTO', 'L', iret, nval=7,&
                    itab=jtab)
        ncomp = jtab(2)
        nfiss = jtab(7)
        nfh = 1
        if (enr .eq. 'XH2') nfh = 2
        if (enr .eq. 'XH3') nfh = 3
        if (enr .eq. 'XH4') nfh = 4
    endif
!
    if (enr(1:2) .eq. 'XT' .or. enr(3:3) .eq. 'T') then
        nfe = 4
    endif
!
    ASSERT(nfe.gt.0.or.nfh.gt.0)
!
!-----------------------------------------------------------------------
!     RECUPERATION DES ENTREES / SORTIE
!-----------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
!
    if (option .eq. 'CHAR_MECA_PRES_R') then
!
!       SI LA PRESSION N'EST CONNUE SUR AUCUN NOEUD, ON LA PREND=0.
        call jevecd('PPRESSR', ipres, 0.d0)
!
    else if (option.eq.'CHAR_MECA_PRES_F') then
!
        call jevech('PPRESSF', 'L', ipres)
        call jevech('PTEMPSR', 'L', itemps)
!
        elseif (option.eq.'CHAR_MECA_FR2D3D'.or.&
     &        option.eq.'CHAR_MECA_FR1D2D') then
        if (ndim .eq. 3) call tefrep(option, nomte, 'PFR2D3D', iforc)
        if (ndim .eq. 2) call tefrep(option, nomte, 'PFR1D2D', iforc)
!
        elseif (option.eq.'CHAR_MECA_FF2D3D'.or.&
     &        option.eq.'CHAR_MECA_FF1D2D') then
!
        if (ndim .eq. 3) call jevech('PFF2D3D', 'L', iforc)
        if (ndim .eq. 2) call jevech('PFF1D2D', 'L', iforc)
        call jevech('PTEMPSR', 'L', itemps)
!
    endif
!
!     PARAMETRES PROPRES A X-FEM
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PSTANO', 'L', jstno)
!     PROPRE AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr(nomte, 'S', 'XFEM', enr, ier)
    if (ier .eq. 0 .and. nomte(3:4) .ne. 'AX' .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC') .and. ndim .le. 2) &
    call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
    call jevech('PVECTUR', 'E', ires)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=zi(jlonch-1+1)
!
!       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do 110 ise = 1, nse
!
!       BOUCLE SUR LES SOMMETS DU SOUS-TRIA (DU SOUS-SEG)
        do 111 in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do 112 j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
112          continue
111      continue
!
!       ON RENOMME LES SOMMETS DU SOUS-ELEMENT
        call vecini(3, 0.d0, a)
        call vecini(3, 0.d0, b)
        call vecini(3, 0.d0, ab)
        do 113 j = 1, ndim
            a(j)=coorse(ndim*(1-1)+j)
            b(j)=coorse(ndim*(2-1)+j)
            ab(j)=b(j)-a(j)
            if (.not.iselli(elref) .and. ndim .eq. 2) then
                c(j)=coorse(ndim*(3-1)+j)
            endif
            if (ndim .eq. 3) c(j)=coorse(ndim*(3-1)+j)
            if (ndim .eq. 3) ac(j)=c(j)-a(j)
113      continue
!
        if (ndime .eq. 2) then
!         CREATION DU REPERE LOCAL 2D : (AB,Y)
            call provec(ab, ac, nd)
            call normev(nd, norme)
            call normev(ab, nab)
            call provec(nd, ab, y)
        endif
!
!       COORDONNÉES DES SOMMETS DE LA FACETTE DANS LE REPÈRE LOCAL
        if (ndime .eq. 2) then
            coorlo(1)=0.d0
            coorlo(2)=0.d0
            coorlo(3)=nab
            coorlo(4)=0.d0
            coorlo(5)=ddot(3,ac,1,ab,1)
            coorlo(6)=ddot(3,ac,1,y ,1)
        else if (ndime.eq.1) then
            if (iselli(elref)) then
!         EN LINEAIRE 2D
                call normev(ab, nab)
                coorlo(1)=0.d0
                coorlo(2)=0.d0
                coorlo(3)=nab
                coorlo(4)=0.d0
                coorlo(5)=0.d0
                coorlo(6)=0.d0
                call vecini(3, 0.d0, nd)
                nd(1) = ab(2)
                nd(2) = -ab(1)
            else if (.not.iselli(elref)) then
!         EN QUADRATIQUE 2D
                ksib=1.d0
                call abscvf(ndim, coorse, ksib, nab)
                coorlo(1)=0.d0
                coorlo(2)=0.d0
                coorlo(3)=nab
                coorlo(4)=0.d0
                coorlo(5)=nab/2
                coorlo(6)=0.d0
                seg(1)=0.d0
                seg(2)=nab
                seg(3)=nab/2
                call normev(ab, nab)
            endif
        endif
!
!       COORDONNÉES DES NOEUDS DE L'ELREFP DANS LE REPÈRE LOCAL
        do 116 ino = 1, nnop*ndime
            geomlo(ino)=0.d0
116      continue
        do 114 ino = 1, nnop
            do 115 j = 1, ndim
                n(j)=zr(igeom-1+ndim*(ino-1)+j)
                an(j)=n(j)-a(j)
115          continue
            geomlo(ndime*(ino-1)+1)=ddot(ndim,an,1,ab,1)
!
            if (ndime .eq. 2) geomlo(ndime*(ino-1)+2)=ddot(ndim,an,1,y , 1)
114      continue
!
!-----------------------------------------------------------------------
!         BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
        if (axi) then
            r = 0.d0
            do 1000 ino = 1, nnop
                r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
1000          continue
            ASSERT(r.ge.0d0)
!          ATTENTION : LE POIDS N'EST PAS X R
!          CE SERA FAIT PLUS TARD AVEC JAC = JAC X R
        endif
!
        do 200 kpg = 1, npg
!
!         CALCUL DU POIDS : POIDS = POIDS DE GAUSS * DET(J)
            if (ndime .eq. 2) then
                call dfdm2d(nno, kpg, ipoids, idfde, coorlo,&
                            poids, rb1, rb2)
            else if (ndime.eq.1) then
                kk = (kpg-1)*nno
                call dfdm1d(nno, zr(ipoids-1+kpg), zr(idfde+kk), coorlo, rb1,&
                            rb2, poids, rb3, rb4)
            endif
!
!         COORDONNÉES RÉELLES LOCALES DU POINT DE GAUSS
            call vecini(ndime, 0.d0, gloc)
            do 210 j = 1, nno
                vf=zr(ivf-1+nno*(kpg-1)+j)
                do 211 k = 1, ndime
                    gloc(k)=gloc(k)+vf*coorlo(2*j+k-2)
211              continue
210          continue
!
!         CALCUL DE LA NORMALE A LA FACE AU POINT DE GAUSS
            if (ndim .eq. 2 .and. .not.iselli(elref)) then
                ASSERT(elref.eq.'SE3')
                call vecini(3, 0.d0, nd)
!           COORDONNEES DE REFERENCE 1D DU POINT DE GAUSS
                call reereg('S', elref, nno, seg, gloc,&
                            ndime, ksig, ibid)
!           CALL ELRFDF(ELREF,KSIG,NDIME*NNO,DFF1,IB1,IB2)
                dff(1,1) = ksig-5.d-1
                dff(1,2) = ksig+5.d-1
                dff(1,3) = -2*ksig
                dx=0.d0
                dy=0.d0
                do 212 i = 1, nno
                    dx = dx+dff(1,i)*coorse(ndim*(i-1)+1)
                    dy = dy+dff(1,i)*coorse(ndim*(i-1)+2)
212              continue
                jac=sqrt(dx*dx+dy*dy)
! MODIFIER LE JAC
                if (axi) then
                    jac= jac * r
                endif
                if (abs(jac) .gt. r8prem()) then
                    nd(1) = dy/jac
                    nd(2) = -dx/jac
                else
                    nd(1) = ab(2)
                    nd(2) = -ab(1)
                endif
            endif
!
!         JUSTE POUR CALCULER LES FF AUX NOEUDS DE L'ELREFP
!
            call reeref(elrefp, axi, nnop, ibid, geomlo,&
                        gloc, 1, .false., ndime, rbid,&
                        rbid, rbid, ibid, ibid, ibid,&
                        ibid, ibid, ibid, rbid, rbid,&
                        'NON', xe, ff, rbid, rbid,&
                        rbid, rbid)
!
!         COORDONNES REELLES DU POINT DE GAUSS
            call vecini(4, 0.d0, xg)
            do 220 i = 1, ndim
                do 221 in = 1, nno
                    xg(i) = xg(i) + zr(ivf-1+nno*(kpg-1)+in) * coorse( ndim*(in-1)+i)
221              continue
220          continue
!
!           2EME METHODE POUR CALCULER LES COORDONNÉES RÉELLES
!           DU POINT DE GAUSS
!            G(1)=A(1)+AB(1)*GLOC(1)+Y(1)*GLOC(2)
!            G(2)=A(2)+AB(2)*GLOC(1)+Y(2)*GLOC(2)
!            G(3)=A(3)+AB(3)*GLOC(1)+Y(3)*GLOC(2)
!
!           CALCUL DES FONCTIONS D'ENRICHISSEMENT
!           -------------------------------------
!
            if (nfe .gt. 0) then
!           LEVEL SETS AU POINT DE GAUSS
                lsng = 0.d0
                lstg = 0.d0
                do 230 ino = 1, nnop
                    lsng = lsng + zr(jlsn-1+ino) * ff(ino)
                    lstg = lstg + zr(jlst-1+ino) * ff(ino)
230              continue
!
!           COORDONNÉES POLAIRES DU POINT
                rg=sqrt(lsng**2+lstg**2)
                tg = zi(jheavt-1+ise) * abs(atan2(lsng,lstg))
!
!           FONCTIONS D'ENRICHISSEMENT
                call xdeffe(rg, tg, fe)
!
            endif
!
!         CALCUL DES FORCES REPARTIES SUIVANT LES OPTIONS
!         -----------------------------------------------
!
            call vecini(3, 0.d0, forrep)
            nompar(1)='X'
            nompar(2)='Y'
            if (ndim .eq. 3) nompar(3)='Z'
            if (ndim .eq. 3) nompar(4)='INST'
            if (ndim .eq. 2) nompar(3)='INST'
!
            if (option .eq. 'CHAR_MECA_PRES_R') then
!
!             CALCUL DE LA PRESSION AUX POINTS DE GAUSS
                pres = 0.d0
                cisa = 0.d0
                do 240 ino = 1, nnop
                    if (ndim .eq. 3) pres = pres + zr(ipres-1+ino) * ff( ino)
                    if (ndim .eq. 2) then
                        pres = pres + zr(ipres-1+2*(ino-1)+1) * ff( ino)
                        cisa = cisa + zr(ipres-1+2*(ino-1)+2) * ff( ino)
                    endif
240              continue
!           ATTENTION AU SIGNE : POUR LES PRESSIONS, IL FAUT UN - DVT
!           CAR LE SECOND MEMBRE SERA ECRIT AVEC UN + (VOIR PLUS BAS)
                do 250 j = 1, ndim
                    forrep(j) = -pres * nd(j)
250              continue
                if (ndim .eq. 2) then
                    forrep(1) = forrep(1)- cisa * nd(2)
                    forrep(2) = forrep(2)+ cisa * nd(1)
                endif
!
            else if (option.eq.'CHAR_MECA_PRES_F') then
!
!             VALEUR DE LA PRESSION
                xg(ndim+1) = zr(itemps)
!
                call fointe('FM', zk8(ipres), ndim+1, nompar, xg,&
                            pres, ier)
                if (ndim .eq. 2) call fointe('FM', zk8(ipres+1), ndim+1, nompar, xg,&
                                             cisa, ier)
                do 260 j = 1, ndim
                    forrep(j) = -pres * nd(j)
260              continue
                if (ndim .eq. 2) then
                    forrep(1) = forrep(1)-cisa * nd(2)
                    forrep(2) = forrep(2)+cisa * nd(1)
                endif
!
                elseif (option.eq.'CHAR_MECA_FR2D3D'.or.&
     &            option.eq.'CHAR_MECA_FR1D2D') then
!
                call vecini(ndim, 0.d0, forrep)
                do 270 ino = 1, nnop
                    do 271 j = 1, ndim
                        forrep(j)=forrep(j)+zr(iforc-1+ndim*(ino-1)+j)&
                        *ff(ino)
271                  continue
270              continue
!
                elseif (option.eq.'CHAR_MECA_FF2D3D'.or.&
     &            option.eq.'CHAR_MECA_FF1D2D') then
!
                xg(ndim+1) = zr(itemps)
                do 280 j = 1, ndim
                    call fointe('FM', zk8(iforc-1+j), ndim+1, nompar, xg,&
                                forrep(j), ier)
280              continue
!
            endif
!
!         CALCUL EFFECTIF DU SECOND MEMBRE
!         --------------------------------
            pos=0
            do 290 ino = 1, nnop
!
!           TERME CLASSIQUE
                do 291 j = 1, ndim
                    pos=pos+1
                    zr(ires-1+pos) = zr(ires-1+pos) + forrep(j) * poids * ff(ino)
291              continue
!
!           TERME HEAVISIDE
                do 295 ig = 1, nfh
                    if (nfiss .gt. 1) ifiss = zi(jfisno-1+(ino-1)*nfh+ ig)
                    do 292 j = 1, ndim
                        pos=pos+1
                        zr(ires-1+pos) = zr(ires-1+pos) + zi(jheavt-1+ (ifiss-1)*ncomp+ise)*forre&
                                         &p(j)*poids*ff(ino)
292                  continue
295              continue
!
!           TERME SINGULIER
                do 293 ig = 1, nfe
                    do 294 j = 1, ndim
                        pos=pos+1
                        zr(ires-1+pos) = zr(ires-1+pos) + fe(ig) * forrep(j) * poids * ff(ino)
294                  continue
293              continue
290          continue
200      continue
!
!-----------------------------------------------------------------------
!         FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
!
110  end do
!
!     SUPPRESSION DES DDLS SUPERFLUS
    ddls = ndim*(1+nfh+nfe)
    nddl = nnop*ddls
    call teattr(nomte, 'C', 'XLAG', lag, ibid)
    if (ibid .eq. 0 .and. lag .eq. 'ARETE') then
        nnop = nnos
    endif
    call xteddl(ndim, nfh, nfe, ddls, nddl,&
                nnop, nnops, zi(jstno), .false., lbid,&
                option, nomte, mat, zr(ires), ddls,&
                nfiss, jfisno)
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
!
end subroutine
