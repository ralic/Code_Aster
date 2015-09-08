subroutine te0037(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/confac.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/reeref.h"
#include "asterfort/teattr.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xhmddl.h"
#include "asterfort/xhmini.h"
#include "asterfort/xjacf2.h"
#include "asterfort/xjacff.h"
#include "asterfort/xteddl.h"
#include "asterfort/tecach.h"
#include "asterfort/xteini.h"
#include "asterfort/xxmmvd.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_saut.h"
!
    character(len=16) :: option, nomte
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
! person_in_charge: samuel.geniaut at edf.fr
!
!.......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT EN PRESSION REPARTIE
!          SUR LES LEVRES DES FISSURES X-FEM
!          (LA PRESSION PEUT ETRE DONNEE SOUS FORME D'UNE FONCTION)
!
!          OPTIONS : 'CHAR_MECA_PRES_R'
!                    'CHAR_MECA_PRES_F'
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!.......................................................................
!
!
    character(len=8) :: elref, typma, fpg, elc, nompar(4), lag, elrefc, enr, enr2
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano, fisno(27,10)
    integer :: nfh, nfe, singu, ddlc, nnom, ddls, nddl, ier, ddlm
    integer :: igeom, ipres, itemps, ires, iadzi, iazk24, jheavn, ncompn, hea_fa(2)
    integer :: jlst, jptint, jaint, jcface, jlonch, jstno, jbasec, contac
    integer :: i, j, ninter, nface, cface(30, 6), ifa, nfiss, jfisno
    integer :: ibid, ilev, ifiss, ncompc, jtab(7), ncompp, ino
    integer :: nnof, npgf, ipoidf, ivff, idfdef, ipgf, pos, zxain, nptf, ifh
    real(kind=8) :: pres, cisa, forrep(3, 2), ff(27), jac, nd(3), he(2), mat(1)
    real(kind=8) :: rr(2), lst, xg(4), dfbid(27, 3), r27bid(27), r3bid(3), r
    aster_logical :: lbid, pre1, axi
    integer :: compt, nddlm, nddls, nddlp, iret, jheafa, ncomph, ncompb
    real(kind=8) :: thet, pinter(3), pinref(3)
    data    he / -1.d0 , 1.d0/
!
    call jemarq()
!
!     PAR CONVENTION :
!     LEVRE INFERIEURE (HE=-1) EST LA LEVRE 1, DE NORMALE SORTANTE  ND
!     LEVRE SUPERIEURE (HE=+1) EST LA LEVRE 2, DE NORMALE SORTANTE -ND
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
    zxain = xxmmvd('ZXAIN')
!
    call elref1(elref)
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
    axi = lteatt('AXIS','OUI')
!
    call teattr('C', 'MODTHM', enr2, iret)
    pre1=(iret.eq.0)
!
!-----------------------------------------------------------------------
!     RECUPERATION DES ENTREES / SORTIE
!-----------------------------------------------------------------------
!
    if (option .eq. 'CHAR_MECA_PRES_R') then
!
!       SI LA PRESSION N'EST CONNUE SUR AUCUN NOEUD, ON LA PREND=0.
        call jevecd('PPRESSR', ipres, 0.d0)
        compt = 0
        do i = 1, nno
            thet = abs(zr(ipres-1+(i-1)+1))
            if (thet .lt. r8prem()) compt = compt + 1
        end do
        if (compt .eq. nno) goto 999
!
    else if (option.eq.'CHAR_MECA_PRES_F') then
!
        call jevech('PPRESSF', 'L', ipres)
        call jevech('PTEMPSR', 'L', itemps)
!
    else
        ASSERT(.false.)
    endif
!
    call jevech('PVECTUR', 'E', ires)
!
!   INITIALISATION DES DIMENSIONS DES DDLS X-FEM
!     SI PRE1=.FALSE. -> MODELISATION MECA XFEM CLASSIQUE
!     SI PRE1=.TRUE.  -> MODELISATION HM XFEM
    if (pre1) then
        call xhmini(nomte, nfh, ddls, ddlm, nddlp, nfiss)
!
        contac = 0
        singu = 0
        nddls = ddls + nddlp
        nddlm = ddlm
        nnom = nno - nnos
        nddl = nnos*nddls + nnom*nddlm
    else
        call xteini(nomte, nfh, nfe, singu, ddlc,&
                    nnom, ddls, nddl, ddlm, nfiss,&
                    contac)
    endif
!
    call tecael(iadzi, iazk24, noms=0)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
!
    if (ndim .eq. 3) then
        if (iselli(elref)) then
            elc='TR3'
        else
            elc='TR3'
        endif
        fpg='XCON'
    else if (ndim.eq.2) then
        if (iselli(elref)) then
            elc='SE2'
        else
            elc='SE3'
        endif
        fpg='MASS'
    endif
!
!     PARAMETRES PROPRES A X-FEM
    call jevech('PLST', 'L', jlst)
    call jevech('PPINTER', 'L', jptint)
    call tecach('OOO', 'PPINTER', 'L', iret, nval=2,&
                itab=jtab)
    ncompp = jtab(2)
    call jevech('PAINTER', 'L', jaint)
    call jevech('PCFACE', 'L', jcface)
    call tecach('OOO', 'PCFACE', 'L', iret, nval=2,&
                itab=jtab)
    ncompc = jtab(2)
    call tecach('OOO', 'PBASECO', 'L', iret, nval=2,&
                itab=jtab)
    ncompb = jtab(2)
    call jevech('PLONGCO', 'L', jlonch)
    call jevech('PSTANO', 'L', jstno)
    call jevech('PBASECO', 'L', jbasec)
    if (nfiss .gt. 1) then
       call jevech('PFISNO', 'L', jfisno)
       call jevech('PHEA_FA', 'L', jheafa)
       call tecach('OOO', 'PHEA_FA', 'L', iret, nval=2,&
                   itab=jtab)
       ncomph = jtab(2)
    endif
!
! --- CONECTIVITÃ~I DES FISSURE ET DES DDL HEAVISIDES
    if (nfiss .eq. 1) then
        do ino = 1, nno
            fisno(ino,1) = 1
        enddo
    else
        do ifh = 1, nfh
            do ino = 1, nno
                fisno(ino,ifh) = zi(jfisno-1+(ino-1)*nfh+ifh)
            enddo
        enddo
    endif
!
    call jevech('PGEOMER', 'L', igeom)
!
!     RECUPERATION DE LA DEFNITION DES FONCTIONS HEAVISIDE
    hea_fa(1:2)=0
    call teattr('S', 'XFEM', enr, ier)
    call jevech('PHEA_NO', 'L', jheavn)
    call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7,&
                itab=jtab)
    ncompn = jtab(2)/jtab(3)
    if (enr(1:2).eq.'XH'.and. nfiss .eq.1) then
        hea_fa(1)=xcalc_code(1, he_real=[he(1)])
        hea_fa(2)=xcalc_code(1, he_real=[he(2)])
    endif
!
!     RÉCUPÉRATIONS DES DONNÉES SUR LA TOPOLOGIE DES FACETTES
    do ifiss = 1, nfiss
       ninter=zi(jlonch+3*(ifiss-1)-1+1)
       nface=zi(jlonch+3*(ifiss-1)-1+2)
       nptf=zi(jlonch+3*(ifiss-1)-1+3)
       if (nptf .eq. 6 .and. ndim .eq. 3) elc = 'TR6'
       if (ninter .lt. ndim) goto 998
!
       do i = 1, nface
           do j = 1, nptf
               cface(i,j)=zi(jcface-1+ncompc*(ifiss-1)+nptf*(i-1)+j)
           end do
       end do
!
!-----------------------------------------------------------------------
!     BOUCLE SUR LES FACETTES
!-----------------------------------------------------------------------
!
       do ifa = 1, nface
!
           call elrefe_info(elrefe=elc,fami=fpg,nno=nnof,&
                            npg=npgf,jpoids=ipoidf,jvf=ivff,jdfde=idfdef)
!
!       ON VERIFIE QUE LES NOEUDS DE LA FACETTE DE CONTACT ONT LST<0
!
           if (singu.eq.1) then
              call vecini(3, 0.d0, pinter)
              do i = 1, nptf
                 do j = 1, ndim
                    pinter(j) = zr(jptint-1+ndim*(cface(ifa,i)-1)+j)
                 end do
                 call reeref(elref, nno, zr(igeom), pinter, ndim, pinref, ff, dfbid)
                 lst = 0.d0
                 do j = 1, nno
                     lst=lst+zr(jlst-1+j)*ff(j)
                 end do
                 ASSERT(lst.le.1.d-4)
              end do
           endif
!       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
           do ipgf = 1, npgf
!
!         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
!         ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
!         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
!         ET DE XG : COORDONNEES REELLES DU POINT DE GAUSS
               elrefc='NON'
               if (ndim .eq. 3) then
                   call xjacff(elref, elrefc, elc, ndim, fpg,&
                               jptint, ifa, cface, ipgf, nno,&
                               igeom, jbasec, xg, jac, ff,&
                               r27bid, dfbid, nd, r3bid, r3bid,&
                               ifiss, ncompp, ncompb)
               else if (ndim.eq.2) then
                   call xjacf2(elref, elrefc, elc, ndim, fpg,&
                               jptint, ifa, cface, nptf, ipgf,&
                               nno, igeom, jbasec, xg, jac,&
                               ff, r27bid, dfbid, nd, r3bid,&
                               ifiss, ncompp, ncompb)
               endif
!
!         CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
               if (singu .eq. 1) then
                   lst=0.d0
                   do i = 1, nno
                       lst=lst+zr(jlst-1+i)*ff(i)
                   end do
                   if (lst.gt.0.d0) lst = 0.d0
                   rr(1)=-sqrt(-lst)
                   rr(2)= sqrt(-lst)
               endif
!
!         CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
               if (axi) then
                   r = 0.d0
                   do ino = 1, nno
                       r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
                   end do
                   ASSERT(r.ge.0d0)
!              ATTENTION : LE POIDS N'EST PAS X R
!              CE SERA FAIT PLUS TARD AVEC JAC = JAC X R
               endif
!
!         CALCUL DES FORCES REPARTIES SUIVANT LES OPTIONS
!         -----------------------------------------------
!
               call vecini(3*2, 0.d0, forrep)
               nompar(1)='X'
               nompar(2)='Y'
               if (ndim .eq. 3) nompar(3)='Z'
               if (ndim .eq. 3) nompar(4)='INST'
               if (ndim .eq. 2) nompar(3)='INST'
!
! MODIFIER LE JAC
               if (axi) then
                   jac = jac * r
               endif
!
               if (option .eq. 'CHAR_MECA_PRES_R') then
!
!           CALCUL DE LA PRESSION AUX POINTS DE GAUSS
                   pres = 0.d0
                   cisa = 0.d0
                   do ino = 1, nno
                       if (ndim .eq. 3) pres = pres + zr(ipres-1+ino) * ff( ino)
                       if (ndim .eq. 2) then
                           pres = pres + zr(ipres-1+2*(ino-1)+1) * ff( ino)
                           cisa = cisa + zr(ipres-1+2*(ino-1)+2) * ff( ino)
                       endif
                   end do
!           ATTENTION AU SIGNE : POUR LES PRESSIONS, IL FAUT UN - DVT
!           CAR LE SECOND MEMBRE SERA ECRIT AVEC UN + (VOIR PLUS BAS)
!           ON CALCULE FORREP POUR LES DEUX LEVRES  : 1 = INF ET 2 = SUP
                   do j = 1, ndim
                       forrep(j,1) = -pres * nd(j)
                       forrep(j,2) = -pres * (-nd(j))
                   end do
                   if (ndim .eq. 2) then
                       forrep(1,1) = forrep(1,1)- cisa * nd(2)
                       forrep(2,1) = forrep(2,1)+ cisa * nd(1)
                       forrep(1,2) = forrep(1,2)- cisa * (-nd(2))
                       forrep(2,2) = forrep(2,2)+ cisa * (-nd(1))
                   endif
!
               else if (option.eq.'CHAR_MECA_PRES_F') then
!
!           VALEUR DE LA PRESSION
                   xg(ndim+1) = zr(itemps)
                   call fointe('FM', zk8(ipres), ndim+1, nompar, xg,&
                               pres, ier)
                   if (ndim .eq. 2) call fointe('FM', zk8(ipres+1), ndim+1, nompar, xg,&
                                             cisa, ier)
                   do j = 1, ndim
                       forrep(j,1) = -pres * nd(j)
                       forrep(j,2) = -pres * (-nd(j))
                   end do
                   if (ndim .eq. 2) then
                       forrep(1,1) = forrep(1,1)- cisa * nd(2)
                       forrep(2,1) = forrep(2,1)+ cisa * nd(1)
                       forrep(1,2) = forrep(1,2)- cisa * (-nd(2))
                       forrep(2,2) = forrep(2,2)+ cisa * (-nd(1))
                   endif
               else
                   call utmess('F', 'XFEM_15')
               endif
!
!         CALCUL EFFECTIF DU SECOND MEMBRE SUR LES DEUX LEVRES
               if (pre1) then
                   do ilev = 1, 2
                       pos=0
                       do ino = 1, nno
!
!               TERME CLASSIQUE
                           do j = 1, ndim
                               pos=pos+1
                               zr(ires-1+pos) = zr(ires-1+pos) + forrep( j,ilev)*jac*ff(ino)
                           end do
!
!               ON ZAPPE LES TERMES DE PRESSION CLASSIQUE SI ON EST SUR UN
!               NOEUD SOMMET
                           if (ino .le. nnos) pos=pos+1
!
!               TERME HEAVISIDE
                           do ifh = 1, nfh
!               EN MULTI-FISSURATION, IL FAUT RECUPERER LES BONNES VALEURS DE HE
                               if (nfiss.gt.1) then
                                  hea_fa(ilev) = zi(jheafa-1+ncomph*(ifiss-1)&
                                             +2*(ifa-1)+ilev)
                               endif
                               do j = 1, ndim
                                  pos=pos+1
                                  zr(ires-1+pos) = zr(ires-1+pos) + xcalc_heav(&
                                                   zi(jheavn-1+ncompn*(ino-1)+ifh),hea_fa(ilev),&
                                                   zi(jheavn-1+ncompn*(ino-1)+ncompn))&
                                                   *forrep(j,ilev)*jac*ff(ino)
                               end do
!               ON ZAPPE LES TERMES DE PRESSION HEAVISIDE SI ON 
!               EST SUR UN NOEUD SOMMET
                               if (ino.le.nnos) pos=pos+1 
                           end do
                       end do
                   end do
               else
                   do ilev = 1, 2
!
                      pos=0
                      do ino = 1, nno
!
!               TERME CLASSIQUE
                         do j = 1, ndim
                            pos=pos+1
                            zr(ires-1+pos) = zr(ires-1+pos) + forrep( j,ilev) * jac * ff(ino)
                         end do
!
!               TERME HEAVISIDE
                         do j = 1, ndim
                            do ifh = 1, nfh
                               pos=pos+1
                               zr(ires-1+pos) = zr(ires-1+pos) + xcalc_heav(&
                                                zi(jheavn-1+ncompn*(ino-1)+ifh),&
                                                hea_fa(ilev),&
                                                zi(jheavn-1+ncompn*(ino-1)+ncompn))&
                                                * forrep(j,ilev) * jac * ff(ino)
                            enddo
                         end do
!
!               TERME SINGULIER
                         do j = 1, singu*ndim
                            pos=pos+1
                            zr(ires-1+pos) = zr(ires-1+pos) + rr(ilev) * forrep(j,ilev) * jac * f&
                                             &f(ino) 
                         end do
!
!               ON SAUTE LES POSITIONS DES DDLS ASYMPTOTIQUES E2, E3, E4
                         pos = pos + (nfe-1) * ndim * singu
!
!               ON SAUTE LES POSITIONS DES LAG DE CONTACT FROTTEMENT
!
                         if (contac .eq. 3) then
                            if (ino .le. nnos) pos = pos + ddlc
                         else
                            pos = pos + ddlc
                         endif
!
                      end do
                   end do
               endif
           end do
       end do
998    continue
    end do
!
!     SUPPRESSION DES DDLS SUPERFLUS
    if (pre1) then
        call xhmddl(ndim, nfh, nddls, nddl, nno, nnos,&
                    zi(jstno), .false._1, option, nomte, mat,&
                    zr(ires), nddlm, nfiss, jfisno)
    else
        call teattr('C', 'XLAG', lag, ibid)
        if (ibid .eq. 0 .and. lag .eq. 'ARETE') then
            nno = nnos
        endif
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, zi(jstno), .false._1, lbid,&
                    option, nomte, ddlm, nfiss, jfisno,&
                    vect=zr(ires))
    endif
!
!
999 continue
    call jedema()
end subroutine
