subroutine te0036(option, nomte)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm1d.h"
#include "asterfort/dfdm2b.h"
#include "asterfort/dismoi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
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
#include "asterfort/xnormv.h"
#include "asterfort/xhmddl.h"
#include "asterfort/xteddl.h"
#include "asterfort/xcalc_heav.h"
!
    character(len=8) :: nompar(4), noma, elrefp, elrese(4), enr, lag
    character(len=8) :: elref
    character(len=16) :: nomte, option
    integer :: jpintt, jcnset, jheavt, jlonch, jlsn, jlst, k
    integer :: jpmilt, irese, nfiss, jfisno, jtab(7), ncomp, jheavn, jheavs, ncompn
    integer :: ibid, ier, ndim, nno, nnop, nnops, npg, nnos, kpg
    integer :: ipoids, ivf, idfde, igeom, ipres, itemps, ires, j
    integer :: nfh, nfe, nse, ise
    integer :: in, ino, iadzi, iazk24, jstno
    integer :: iforc, iret, ig, pos, ndime, nddl, ddls
    real(kind=8) :: xg(4), fe(4), lsng, lstg, rg, tg, r
    real(kind=8) :: pres, ff(27), coorse(81), cosa, sina
    real(kind=8) :: nd(3), norme, rb1(3), rb2(3), cisa
    real(kind=8) :: poids, forrep(3), vf, td(3), rbid(1)
    aster_logical :: lbid, axi, pre1
    integer :: kk, ddlm, nnopm
    data          elrese /'SE2','TR3','SE3','TR6'/
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
!     ELEMENT DE REFERENCE PARENT
    call elref1(elrefp)
    call elrefe_info(fami='RIGI', ndim=ndime, nno=nnop, nnos=nnops)
    ASSERT(ndime.eq.1.or.ndime.eq.2)
!
    axi = lteatt('AXIS','OUI')

      pre1 = .false.
!     SI ON EST DANS LE CAS HM-XFEM, PRE1=.TRUE.
      if (nomte(1:2).eq.'HM') then
        pre1=.true.
      endif
!
!     DIMENSION DE L'ESPACE
    call tecael(iadzi, iazk24, noms=0)
    noma=zk24(iazk24)(1:8)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
!
!     SOUS-ELEMENT DE REFERENCE
    if (.not.iselli(elrefp)) then
        irese=2
    else
        irese=0
    endif
    elref=elrese(ndime+irese)
    call elrefe_info(elrefe=elref, fami='RIGI', nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
!     IL NE FAUT PAS APPELER XTEINI CAR IL NE GERE PAS LES ELEMENTS
!     DE BORD
!      CALL XTEINI(NOMTE,DDLH,NFE,IBID,IBID,IBID,IBID,IBID)
    nfe = 0
    nfh = 0
    nfiss = 1
    call teattr('S', 'XFEM', enr, ier)
    if (enr(1:2).eq.'XH') then
! --- NOMBRE DE FISSURES
        call tecach('NOO', 'PHEAVTO', 'L', iret, nval=7,&
                    itab=jtab)
        ncomp = jtab(2)
        nfiss = jtab(7)
        nfh = 1
        if (enr .eq. 'XH2') then
           nfh = 2
        else if (enr .eq. 'XH3') then
           nfh = 3
        else if (enr .eq. 'XH4') then
           nfh = 4
        endif
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
        if (ndim .eq. 3) then
           call tefrep(option, nomte, 'PFR2D3D', iforc)
        else if (ndim .eq. 2) then
           call tefrep(option, nomte, 'PFR1D2D', iforc)
        endif
!
    elseif (option.eq.'CHAR_MECA_FF2D3D'.or.&
     &        option.eq.'CHAR_MECA_FF1D2D') then
!
        if (ndim .eq. 3) then
           call jevech('PFF2D3D', 'L', iforc)
        else if (ndim .eq. 2) then
           call jevech('PFF1D2D', 'L', iforc)
        endif
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
    call teattr('S', 'XFEM', enr, ier)
    if (ier .eq. 0 .and. .not.iselli(elref)) call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
!   LECTURE DES DONNES TOPOLOGIQUE DES FONCTIONS HEAVISIDE
    if (enr(1:2).eq.'XH' .or. pre1 .or. enr(1:2).eq.'XT') then
        call jevech('PHEA_NO', 'L', jheavn)
        call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7,&
                itab=jtab)
        ncompn = jtab(2)/jtab(3)
        call jevech('PHEA_SE', 'L', jheavs)
    endif
!
    call jevech('PVECTUR', 'E', ires)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=zi(jlonch-1+1)
!
!       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do ise = 1, nse
!
!       BOUCLE SUR LES NOEUDS DU SOUS-TRIA (DU SOUS-SEG)
        do in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do j = 1, ndim
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
            end do
        end do
!
!-----------------------------------------------------------------------
!         BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
!       if (axi) then
!            r = 0.d0
!            do ino = 1, nnop
!                r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
!            end do
!            ASSERT(r.ge.0d0)
!          ATTENTION : LE POIDS N'EST PAS X R
!          CE SERA FAIT PLUS TARD AVEC JAC = JAC X R
!        endif
!
        do kpg = 1, npg
!
!         CALCUL DU POIDS : POIDS = POIDS DE GAUSS * DET(J)
            if (ndime .eq. 2) then
                kk = 2*(kpg-1)*nno
                call dfdm2b(nno, zr(ipoids-1+kpg), zr(idfde+kk), coorse,&
                            poids, nd)
            else if (ndime.eq.1) then
                kk = (kpg-1)*nno
                call dfdm1d(nno, zr(ipoids-1+kpg), zr(idfde+kk), coorse, rb1,&
                            rb2(1), poids, cosa, sina)
            endif
!
!         COORDONNÉES RÉELLES GLOBALES DU POINT DE GAUSS
            call vecini(ndim+1, 0.d0, xg)
            do j = 1, nno
                vf=zr(ivf-1+nno*(kpg-1)+j)
                do k = 1, ndim
                    xg(k)=xg(k)+vf*coorse(ndim*(j-1)+k)
                end do
            end do
!
            if (ndime.eq.1) then
                ASSERT(elref(1:2).eq.'SE')
                call vecini(ndim, 0.d0, td)
                call vecini(ndim, 0.d0, nd)
!         CALCUL DE LA NORMALE AU SEGMENT AU POINT DE GAUSS
               nd(1) = cosa
               nd(2) = sina
               call xnormv(2, nd, norme)
               ASSERT(norme.gt.0.d0)
!         CALCUL DE LA TANGENTE AU SEGMENT AU POINT DE GAUSS
!                do j = 1, nno
!                   vf=zr(idfde-1+nno*(kpg-1)+j)
!                   do k = 1, ndim
!                       td(k)=td(k)+vf*coorse(ndim*(j-1)+k)
!                   end do
!               end do
                td(1) = -sina
                td(2) = cosa
            else if (ndime.eq.2) then
!                call vecini(ndim, 0.d0, td1)
!                call vecini(ndim, 0.d0, td2)
!         CALCUL DES TANGENTES A LA FACE AU POINT DE GAUSS
!                do j = 1, nno
!                   do k = 1, ndim
!                       td1(k)=td1(k)+coorse(ndim*(j-1)+k)* zr(idfde-1+2*nno*(kpg-1)+2*j-1)
!                       td2(k)=td2(k)+coorse(ndim*(j-1)+k)* zr(idfde-1+2*nno*(kpg-1)+2*j)
!                   end do
!               end do
!         CALCUL DE LA NORMALE A LA FACE AU POINT DE GAUSS
!               call provec(td1,td2,nd)
               call xnormv(3, nd, norme)
!               nd(1:3) = -nd(1:3)
               ASSERT(norme.gt.0.d0)
            endif
!
!           CALCUL DES FONCTIONS D'ENRICHISSEMENT
!           -------------------------------------
!
            call reeref(elrefp, nnop, zr(igeom), xg, ndim, rb2, ff)
            if (nfe .gt. 0) then
!           LEVEL SETS AU POINT DE GAUSS
                lsng = 0.d0
                lstg = 0.d0
                do ino = 1, nnop
                    lsng = lsng + zr(jlsn-1+ino) * ff(ino)
                    lstg = lstg + zr(jlst-1+ino) * ff(ino)
                end do
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
                do ino = 1, nnop
                    if (ndim .eq. 3) pres = pres + zr(ipres-1+ino) * ff(ino)
                    if (ndim .eq. 2) then
                        pres = pres + zr(ipres-1+2*(ino-1)+1) * ff(ino)
                        cisa = cisa + zr(ipres-1+2*(ino-1)+2) * ff(ino)
                    endif
                end do
!           ATTENTION AU SIGNE : POUR LES PRESSIONS, IL FAUT UN - DVT
!           CAR LE SECOND MEMBRE SERA ECRIT AVEC UN + (VOIR PLUS BAS)
                do j = 1, ndim
                    forrep(j) = -pres * nd(j)
                end do
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
                do j = 1, ndim
                    forrep(j) = -pres * nd(j)
                end do
                if (ndim .eq. 2) then
                    forrep(1) = forrep(1)-cisa * nd(2)
                    forrep(2) = forrep(2)+cisa * nd(1)
                endif
!
            elseif (option.eq.'CHAR_MECA_FR2D3D'.or.&
     &            option.eq.'CHAR_MECA_FR1D2D') then
!
                call vecini(ndim, 0.d0, forrep)
                do ino = 1, nnop
                    do j = 1, ndim
                        forrep(j)=forrep(j)+zr(iforc-1+ndim*(ino-1)+j)&
                        *ff(ino)
                    end do
                end do
!
            elseif (option.eq.'CHAR_MECA_FF2D3D'.or.&
     &            option.eq.'CHAR_MECA_FF1D2D') then
!
                xg(ndim+1) = zr(itemps)
                do j = 1, ndim
                    call fointe('FM', zk8(iforc-1+j), ndim+1, nompar, xg,&
                                forrep(j), ier)
                end do
!
            endif
            if (axi) then
                 r = 0.d0
                 do ino = 1, nnop
                     r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
                 end do
                 ASSERT(r.ge.0d0)
                 poids = poids*r
            endif
!
!         CALCUL EFFECTIF DU SECOND MEMBRE
!         --------------------------------
            if (pre1) then
               pos=0
               do ino = 1, nnop
!
!           TERME CLASSIQUE
                   do j = 1, ndim
                       pos=pos+1
                       zr(ires-1+pos) = zr(ires-1+pos) + forrep(j) * poids * ff(ino)
                   end do
!
!           ON ZAPPE LES TERMES DE PRESSION CLASSIQUE SI ON ES SUR UN NOEUD SOMMET
                   if (ino.le.nnops) pos = pos+1
!           TERME HEAVISIDE
                do ig = 1, nfh
                    do j = 1, ndim
                        pos=pos+1
                        zr(ires-1+pos) = zr(ires-1+pos) + xcalc_heav(&
                                                           zi(jheavn-1+ncompn*(ino-1)+ig)&
                                                          ,zi(jheavs-1+ise),&
                                                           zi(jheavn-1+ncompn*(ino-1)+ncompn))&
                                                        *forrep(j)*poids*ff(ino)
                    end do
                end do
!
!           ON ZAPPE LES TERMES DE PRESSION HEAVISIDE SI ON ES SUR UN NOEUD SOMMET
                   if (ino.le.nnops) then
                      pos = pos+1
                   endif
               end do
            else
               pos=0
               do ino = 1, nnop
!
!           TERME CLASSIQUE
                   do j = 1, ndim
                       pos=pos+1
                       zr(ires-1+pos) = zr(ires-1+pos) + forrep(j) * poids * ff(ino)
                   end do
!
!           TERME HEAVISIDE
                   do ig = 1, nfh
                       do j = 1, ndim
                           pos=pos+1
                           zr(ires-1+pos) = zr(ires-1+pos) + xcalc_heav(&
                                                             zi(jheavn-1+ncompn*(ino-1)+ig)&
                                                            ,zi(jheavs-1+ise),&
                                                             zi(jheavn-1+ncompn*(ino-1)+ncompn))&
                                                           *forrep(j)*poids*ff(ino)
                       end do
                   end do
!           TERME SINGULIER
                do ig = 1, nfe
                    do j = 1, ndim
                        pos=pos+1
                        zr(ires-1+pos) = zr(ires-1+pos) + fe(ig) * forrep(j) * poids *&
                                         ff(ino)
                    end do
                end do
               end do
            endif
        end do
!
!-----------------------------------------------------------------------
!         FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
!
    end do
!
!     SUPPRESSION DES DDLS SUPERFLUS
    if (pre1) then
       ddls = ndim*(1+nfh+nfe)+(1+nfh)
       ddlm = ndim*(1+nfh+nfe)
       nnopm= nnop-nnops
       nddl = nnops*ddls + nnopm*ddlm
       call xhmddl(ndim, ddls, nddl, nnop, nnops, zi(jstno),.false._1,&
                   option, nomte, rbid, zr(ires), ddlm)
    else
       ddls = ndim*(1+nfh+nfe)
       nddl = nnop*ddls
       call teattr('C', 'XLAG', lag, ibid)
       if (ibid .eq. 0 .and. lag .eq. 'ARETE') then
           nnop = nnos
       endif
       call xteddl(ndim, nfh, nfe, ddls, nddl,&
                   nnop, nnops, zi(jstno), .false._1, lbid,&
                   option, nomte, ddls,&
                   nfiss, jfisno, vect=zr(ires))
    endif
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
!
end subroutine
