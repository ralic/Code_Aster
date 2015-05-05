subroutine op0143()
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
!
!     OPERATEUR "DEFI_FLUI_STRU"
!
!-----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/tfimpr.h"
#include "asterfort/tfveri.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: ocvect, ocprho, ocpvis, ocpesa, ocrugo, occapa, ocgril, iret, ifm
    integer :: niv, nbno, jno
    integer :: dimvi, dimvk, dimvr, dimgm, dimgr
    integer :: iunit, unit1, unit2, ibid1, ibid2
    integer :: irho, jrho
    character(len=2) :: carapa(4)
    character(len=3) :: ouinon
    character(len=9) :: typas(2), tpas
    character(len=16) :: concep, cmd, nommcf, mcfac(4), motcle(2), typmcl(2)
    character(len=19) :: nomu
    character(len=8) :: nomu8, modele, grappe, carael, maillage
    character(len=24) :: fsic, fsvi, fsvr, fsvk, fsgm, fscr, fsgr, lisno
    real(kind=8) :: vect(3), valepa(4)
!
!-----------------------------------------------------------------------
    integer :: i, ibid, icael, icar, icara, icm, icmp
    integer :: icoor, icoup, ideccr, idecvr, ience, iequiv, iocc
    integer :: ipas, ipesa, iprho, ipvis, irhoe, irhoi, irugo
    integer :: itpas, itres, itypf2, itypfl, ivect, j, jcm
    integer :: jcoup, jpas, jtpas, lfscr, lfsgm, lfsgr, lfsic
    integer :: lfsvi, lfsvk, lfsvr, lnbcr, nbangl, nbcara, nbcoor
    integer :: nbcr, nbgrma, nbgtot, nbocc, ntypg, nzex
    real(kind=8) :: pas, pi, y
!-----------------------------------------------------------------------
    data typas   /'CARRE_LIG','TRIA_LIGN'/
    data mcfac   /'FAISCEAU_TRANS ','GRAPPE',&
                  'FAISCEAU_AXIAL ','COQUE_COAX'/
    data motcle  /'NOEUD','GROUP_NO'/
    data typmcl  /'NOEUD','GROUP_NO'/

!=======================================================================
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
    pi = r8pi()
!
    call getres(nomu, concep, cmd)
    nomu8=nomu(1:8)
!
    do itypf2 = 1, 4
        call getfac(mcfac(itypf2), nbocc)
        if (nbocc .ge. 1) goto 11
    end do
11  continue
    itypfl=itypf2
    nommcf=mcfac(itypf2)
!
!=====================================================================
! ----VERIFICATIONS AVANT EXECUTION ----
!     =============================
    call tfveri(nommcf, nbocc, itypfl)
!
!=====================================================================
! ----LECTURE DES INFORMATIONS ET STOCKAGE -----
!     ====================================
    fsic = nomu//'.FSIC'
    fsvi = nomu//'.FSVI'
    fsvr = nomu//'.FSVR'
    fsvk = nomu//'.FSVK'
    fsgm = nomu//'.FSGM'
    fscr = nomu//'.FSCR'
    fsgr = nomu//'.FSGR'
!
    call wkvect(fsic, 'G V I', 2, lfsic)
    zi(lfsic) = itypfl
!
!-----------------------------------------------------------------------
! ----- 1.CAS D'UN FAISCEAU_TRANS
!       -------------------------
    if (itypfl .eq. 1) then
!
! ---     STOCKAGE DE L'UNITE LOGIQUE
        call getvis(nommcf, 'UNITE_CD', iocc=1, scal=unit1, nbret=ibid1)
        call getvis(nommcf, 'UNITE_CK', iocc=1, scal=unit2, nbret=ibid2)
        call jeexin(nomu8//'.UNIT_FAISCEAU', iret)
        if (iret .eq. 0) then
            call wkvect(nomu8//'.UNIT_FAISCEAU', 'G V I', 2, iunit)
        else
            call jeveuo(nomu8//'.UNIT_FAISCEAU', 'L', iunit)
        endif
        zi(iunit-1+1) = unit1
        zi(iunit-1+2) = unit2
!
        jcm = 0
        jrho = 1
        do iocc = 1, nbocc
! ---     RECHERCHE DE LA DERNIERE OCCURENCE DES MOTS-CLES OBLIGATOIRES
            call getvtx(nommcf, 'COUPLAGE', iocc=iocc, nbval=0, nbret=icoup)
            if (icoup .ne. 0) jcoup = iocc
            call getvid(nommcf, 'CARA_ELEM', iocc=iocc, nbval=0, nbret=icara)
            call getvid(nommcf, 'PROF_RHO_F_INT', iocc=iocc, nbval=0, nbret=irhoi)
            call getvid(nommcf, 'PROF_RHO_F_EXT', iocc=iocc, nbval=0, nbret=irhoe)
            call getvtx(nommcf, 'NOM_CMP       ', iocc=iocc, nbval=0, nbret=icmp)
! ---     RECHERCHE DE LA DERNIERE OCCURENCE DES MOTS-CLES FACULTATIFS
            call getvr8(nommcf, 'COEF_MASS_AJOU', iocc=iocc, nbval=0, nbret=icm)
            if (icm .ne. 0) jcm = iocc
            call getvr8(nommcf, 'RHO_TUBE', iocc=iocc, nbval=0, nbret=irho)
            if (irho .ne. 0) jrho=iocc
            call getvtx(nommcf, 'TYPE_PAS', iocc=iocc, nbval=0, nbret=itpas)
            if (itpas .ne. 0) jtpas = iocc
            call getvis(nommcf, 'TYPE_RESEAU', iocc=iocc, nbval=0, nbret=itres)
            call getvr8(nommcf, 'PAS', iocc=iocc, nbval=0, nbret=ipas)
            if (ipas .ne. 0) jpas = iocc
        end do
!
        nzex = nbocc
        call getvtx(nommcf, 'COUPLAGE', iocc=jcoup, scal=ouinon, nbret=ibid)
!
! --------1.1.SI PRISE EN COMPTE DU COUPLAGE
        if (ouinon .eq. 'OUI') then
!
            zi(lfsic+1) = 1
!
            call wkvect(fsvr, 'G V R', 3+2*nzex, lfsvr)
            call wkvect(fsvi, 'G V I', 2+2*nzex, lfsvi)
            zi(lfsvi+1) = nzex
            do iocc = 1, nzex
                call getvis(nommcf, 'TYPE_RESEAU', iocc=iocc, scal=zi( lfsvi+1+iocc), nbret=ibid)
                call getvr8(nommcf, 'CSTE_CONNORS', iocc=iocc, nbval=2, vect=zr(lfsvr+2*iocc+1),&
                            nbret=ibid)
                call getvis(nommcf, 'NB_CONNORS', iocc=iocc, nbval=2,&
                            vect=zi(lfsvi+ 1+nzex+iocc), nbret=ibid)
            end do
            call getvtx(nommcf, 'TYPE_PAS', iocc=jtpas, scal=tpas, nbret=ibid)
            if (tpas .eq. typas(1)) then
                zi(lfsvi) = 1
            else
                zi(lfsvi) = 2
            endif
!
! ---------- PAS REDUIT
            call getvr8(nommcf, 'PAS', iocc=jpas, scal=pas, nbret=ibid)
            if (jcm .eq. 0) then
                if (zi(lfsvi) .eq. 2) then
! ---------------- RESEAU A PAS TRIANGULAIRE
                    y = (0.96d0+0.5d0*pas)*pas
                else
! ---------------- RESEAU A PAS CARRE
                    y = (1.07d0+0.56d0*pas)*pas
                endif
                zr(lfsvr) = (pi*(y*y+1.d0))/(2.d0*(y*y-1.d0))
                if (niv .eq. 2) write(ifm,100) zr(lfsvr)
            else
                call getvr8(nommcf, 'COEF_MASS_AJOU', iocc=jcm, scal=zr(lfsvr), nbret=ibid)
            endif
            zr(lfsvr+1) = pas
!
            call getvr8(nommcf, 'RHO_TUBE', iocc=jrho, scal=zr(lfsvr+2), nbret=irho)
!
! --------1.2.SI NON PRISE EN COMPTE DU COUPLAGE
        else
!
            zi(lfsic+1) = 0
!
            call wkvect(fsvi, 'G V I', 2, lfsvi)
            zi(lfsvi+1) = nzex
!
            call wkvect(fsvr, 'G V R', 1, lfsvr)
            call getvr8(nommcf, 'COEF_MASS_AJOU', iocc=jcm, scal=zr(lfsvr), nbret=ibid)
!
        endif
!
! --------1.3.DANS LES DEUX CAS CREATION ET REMPLISSAGE DU .FSVK
        call wkvect(fsvk, 'G V K8', 4+nzex, lfsvk)
        call getvid(nommcf, 'CARA_ELEM', iocc=1, scal=zk8(lfsvk), nbret=ibid)
        call getvtx(nommcf, 'NOM_CMP', iocc=1, scal=zk8(lfsvk+1), nbret=ibid)
        call getvid(nommcf, 'PROF_RHO_F_INT', iocc=1, scal=zk8(lfsvk+2), nbret=ibid)
        call getvid(nommcf, 'PROF_RHO_F_EXT', iocc=1, scal=zk8(lfsvk+3), nbret=ibid)
        do iocc = 1, nzex
            call getvid(nommcf, 'PROF_VITE_FLUI', iocc=iocc, scal=zk8( lfsvk+3+iocc), nbret=ibid)
        end do
!
! -------1.4.VERIFICATION DES NUMERO ET NOMS DE ZONE D EXCITATION DU
!            FLUIDE
        do i = 1, nzex-1
            do j = i+1, nzex
                if (zk8(lfsvk+i+3) .eq. zk8(lfsvk+j+3)) then
                    call utmess('F', 'MODELISA5_65')
                endif
            end do
        end do
!-----------------------------------------------------------------------
! ----- 2.CAS D'UNE GRAPPE
!       ------------------
    else if (itypfl.eq.2) then
!
        call getvtx(nommcf, 'COUPLAGE', iocc=1, scal=ouinon, nbret=ibid)
!
! --------2.1.SI PRISE EN COMPTE DU COUPLAGE
        if (ouinon .eq. 'OUI') then
!
            zi(lfsic+1) = 1
!
            call wkvect(fsvk, 'G V K8', 4, lfsvk)
            call getvtx(nommcf, 'GRAPPE_2', iocc=1, scal=grappe, nbret=ibid)
            call getvid(nommcf, 'CARA_ELEM', iocc=1, scal=carael, nbret=ibid)
            call getvid(nommcf, 'MODELE', iocc=1, scal=modele, nbret=ibid)
            
            iocc = 1
            lisno = '&&OP0143.LISTE_NOEUD'
            call dismoi('NOM_MAILLA', modele, 'MODELE', repk=maillage)
            call reliem(modele, maillage, 'NO_NOEUD', 'GRAPPE', iocc,&
                        2, motcle, typmcl, lisno, nbno)
            ASSERT(nbno.eq.1)
            call jeveuo(lisno, 'L', jno)
!            
            zk8(lfsvk  )=grappe
            zk8(lfsvk+1)=zk8(jno-1+1)
            zk8(lfsvk+2)=carael
            zk8(lfsvk+3)=modele
!
! ----      STOCKAGE DE L'UNITE LOGIQUE
            call getvis(nommcf, 'UNITE_CA', iocc=1, scal=unit1, nbret=ibid1)
            call getvis(nommcf, 'UNITE_KA', iocc=1, scal=unit2, nbret=ibid2)
            call jeexin(nomu8//'.UNIT_GRAPPES', iret)
            if (iret .eq. 0) then
                call wkvect(nomu8//'.UNIT_GRAPPES', 'G V I', 2, iunit)
            else
                call jeveuo(nomu8//'.UNIT_GRAPPES', 'L', iunit)
            endif
!
            zi(iunit-1+1) = unit1
            zi(iunit-1+2) = unit2
!
            call wkvect(fsvr, 'G V R', 2, lfsvr)
            call getvr8(nommcf, 'COEF_MASS_AJOU', iocc=1, nbval=0, nbret=icm)
            if (icm .ne. 0) then
                call getvr8(nommcf, 'COEF_MASS_AJOU', iocc=1, scal=zr( lfsvr), nbret=ibid)
            else
                zr(lfsvr) = pi * 1.078014d0
            endif
            call getvr8(nommcf, 'RHO_FLUI', iocc=1, scal=zr(lfsvr+1), nbret=ibid)
!
! --------2.2.SI NON PRISE EN COMPTE DU COUPLAGE
        else
            zi(lfsic+1) = 0
!
        endif
!-----------------------------------------------------------------------
! ----- 3.CAS D'UN FAISCEAU_AXIAL
!       -------------------------
    else if (itypfl.eq.3) then
!
        zi(lfsic+1) = 1
!
! --------3.1.DIMENSIONNEMENT ET CREATION DES OBJETS EN FONCTION
! --------    DU TYPE DE REPRESENTATION
! --------    (FAISCEAU EQUIVALENT OU FAISCEAU COMPLET)
! --------    SIMULTANEMENT ON DETERMINE LES OCCURENCES DU MOT-CLE
! --------    FACTEUR POUR LESQUELLES ON IRA CHERCHER LES INFORMATIONS
        iequiv = 1
        if (nbocc .eq. 1) then
            call getvid(nommcf, 'CARA_ELEM', iocc=1, nbval=0, nbret=icael)
            if (icael .ne. 0) iequiv = 0
        endif
!
        ocvect = 1
        ocprho = 1
        ocpvis = 1
        ocpesa = 0
        ocrugo = 1
        occapa = 1
        ocgril = 0
!
        if (iequiv .eq. 0) then
!
            call getvtx(nommcf, 'GROUP_MA', iocc=1, nbval=0, nbret=nbgrma)
            nbgrma = abs(nbgrma)
            call getvr8(nommcf, 'PESANTEUR', iocc=1, nbval=0, nbret=ipesa)
            if (ipesa .ne. 0) ocpesa = 1
            call getvtx(nommcf, 'CARA_PAROI', iocc=1, nbval=0, nbret=nbcara)
            nbcara = abs(nbcara)
            if (nbcara .eq. 3) then
                ience = 1
                nbangl = 0
            else
                ience = 2
                nbangl = 1
            endif
            call getvr8(nommcf, 'COOR_GRILLE', iocc=1, nbval=0, nbret=nbgtot)
            if (nbgtot .ne. 0) then
                ocgril = 1
                nbgtot = abs(nbgtot)
                call getvr8(nommcf, 'LONG_TYPG', iocc=1, nbval=0, nbret=ntypg)
                ntypg = abs(ntypg)
            else
                ntypg = 0
                nbgtot = 0
            endif
!
            if (ocgril .ne. 0) then
                dimvi = 6 + nbgtot
                dimgr = nbgtot + 6*ntypg
            else
                dimvi = 5
                dimgr = 0
            endif
            dimvk = 3
            dimvr = 5 + nbcara + nbangl
            if (nbgrma .ne. 0) then
                dimgm = nbgrma
            else
                dimgm = 1
            endif
!
        else
!
            nbgrma = nbocc
            call wkvect('&&OP0143.TEMP.NBCR', 'V V I', nbocc, lnbcr)
            nbcoor = 0
!
            do iocc = 1, nbocc
                call getvr8(nommcf, 'VECT_X', iocc=iocc, nbval=0, nbret=ivect)
                if (ivect .ne. 0) ocvect = iocc
                call getvid(nommcf, 'PROF_RHO_FLUI', iocc=iocc, nbval=0, nbret=iprho)
                if (iprho .ne. 0) ocprho = iocc
                call getvid(nommcf, 'PROF_VISC_CINE', iocc=iocc, nbval=0, nbret=ipvis)
                if (ipvis .ne. 0) ocpvis = iocc
                call getvr8(nommcf, 'PESANTEUR', iocc=iocc, nbval=0, nbret=ipesa)
                if (ipesa .ne. 0) ocpesa = iocc
                call getvr8(nommcf, 'RUGO_TUBE', iocc=iocc, nbval=0, nbret=irugo)
                if (irugo .ne. 0) ocrugo = iocc
                call getvtx(nommcf, 'CARA_PAROI', iocc=iocc, nbval=0, nbret=icara)
                if (icara .ne. 0) occapa = iocc
                call getvr8(nommcf, 'COOR_TUBE', iocc=iocc, nbval=0, nbret=icoor)
                icoor = abs(icoor)
                zi(lnbcr+iocc-1) = icoor
                nbcoor = nbcoor + icoor
                call getvr8(nommcf, 'COOR_GRILLE', iocc=iocc, nbval=0, nbret=nbgtot)
                if (nbgtot .ne. 0) ocgril = iocc
            end do
!
            call getvtx(nommcf, 'CARA_PAROI', iocc=occapa, nbval=0, nbret=nbcara)
            nbcara = abs(nbcara)
            if (nbcara .eq. 3) then
                ience = 1
                nbangl = 0
            else
                ience = 2
                nbangl = 1
            endif
            if (ocgril .ne. 0) then
                call getvr8(nommcf, 'COOR_GRILLE', iocc=ocgril, nbval=0, nbret=nbgtot)
                nbgtot = abs(nbgtot)
                call getvr8(nommcf, 'LONG_TYPG', iocc=ocgril, nbval=0, nbret=ntypg)
                ntypg = abs(ntypg)
                dimvi = 6 + nbgrma + 1 + nbgtot
                dimgr = nbgtot + 6*ntypg
            else
                nbgtot = 0
                ntypg = 0
                dimvi = 6 + nbgrma
                dimgr = 0
            endif
            dimvk = 2
            dimvr = 5 + nbcara + nbangl + nbgrma
            dimgm = nbgrma
!
            call wkvect(fscr, 'G V R', nbcoor, lfscr)
!
        endif
!
        call wkvect(fsvi, 'G V I', dimvi, lfsvi)
        call wkvect(fsvk, 'G V K8', dimvk, lfsvk)
        call wkvect(fsvr, 'G V R', dimvr, lfsvr)
        call wkvect(fsgm, 'G V K24', dimgm, lfsgm)
        if (ocgril .ne. 0) call wkvect(fsgr, 'G V R', dimgr, lfsgr)
!
! --------3.2.RECUEIL DES INFORMATIONS COMMUNES AUX DEUX TYPES DE
! --------    REPRESENTATION
! --------3.2.1.OBJET .FSVI
        zi(lfsvi) = iequiv
!
        call getvr8(nommcf, 'VECT_X', iocc=ocvect, nbval=3, vect=vect(1),&
                    nbret=ibid)
        if (abs(vect(1)-1.d0) .le. r8prem()) then
            zi(lfsvi+1) = 1
        else if (abs(vect(2)-1.d0) .le. r8prem()) then
            zi(lfsvi+1) = 2
        else
            zi(lfsvi+1) = 3
        endif
!
        zi(lfsvi+2) = ience
        zi(lfsvi+3) = nbgrma
        zi(lfsvi+4) = ntypg
!
        if (ocgril .ne. 0) then
            if (iequiv .eq. 0) then
                zi(lfsvi+5) = nbgtot
                call getvis(nommcf, 'TYPE_GRILLE', iocc=ocgril, nbval=nbgtot, vect=zi(lfsvi+6),&
                            nbret=ibid)
            else
                zi(lfsvi+6+nbgrma) = nbgtot
                call getvis(nommcf, 'TYPE_GRILLE', iocc=ocgril, nbval=nbgtot,&
                            vect=zi(lfsvi+7+nbgrma), nbret=ibid)
            endif
        endif
!
! --------3.2.2.OBJET .FSVK
        call getvid(nommcf, 'PROF_RHO_FLUI', iocc=ocprho, scal=zk8(lfsvk), nbret=ibid)
        call getvid(nommcf, 'PROF_VISC_CINE', iocc=ocpvis, scal=zk8(lfsvk+ 1), nbret=ibid)
!
! --------3.2.3.OBJET .FSVR
        if (ocpesa .ne. 0) then
            call getvr8(nommcf, 'PESANTEUR', iocc=ocpesa, nbval=4, vect=zr(lfsvr),&
                        nbret=ibid)
        else
            zr(lfsvr) = 9.81d0
            zr(lfsvr+1) = 0.d0
            zr(lfsvr+2) = 0.d0
            zr(lfsvr+3) = -1.d0
        endif
        call getvr8(nommcf, 'RUGO_TUBE', iocc=ocrugo, scal=zr(lfsvr+4), nbret=ibid)
!
        call getvtx(nommcf, 'CARA_PAROI', iocc=occapa, nbval=nbcara, vect=carapa(1),&
                    nbret=ibid)
        call getvr8(nommcf, 'VALE_PAROI', iocc=occapa, nbval=nbcara, vect=valepa(1),&
                    nbret=ibid)
        if (ience .eq. 1) then
            do icar = 1, nbcara
                if (carapa(icar) .eq. 'YC') zr(lfsvr+5) = valepa(icar)
                if (carapa(icar) .eq. 'ZC') zr(lfsvr+6) = valepa(icar)
                if (carapa(icar)(1:1) .eq. 'R') zr(lfsvr+7) = valepa( icar)
            end do
        else
            do icar = 1, nbcara
                if (carapa(icar) .eq. 'YC') zr(lfsvr+5) = valepa(icar)
                if (carapa(icar) .eq. 'ZC') zr(lfsvr+6) = valepa(icar)
                if (carapa(icar) .eq. 'HY') zr(lfsvr+7) = valepa(icar)
                if (carapa(icar) .eq. 'HZ') zr(lfsvr+8) = valepa(icar)
            end do
            call getvr8(nommcf, 'ANGL_VRIL', iocc=occapa, scal=zr(lfsvr+9), nbret=ibid)
        endif
!
! --------3.2.4.OBJET .FSGR
        if (ocgril .ne. 0) then
            call getvr8(nommcf, 'COOR_GRILLE', iocc=ocgril, nbval=nbgtot, vect=zr(lfsgr),&
                        nbret=ibid)
            call getvr8(nommcf, 'LONG_TYPG', iocc=ocgril, nbval=ntypg, vect=zr( lfsgr+nbgtot),&
                        nbret=ibid)
            call getvr8(nommcf, 'LARG_TYPG', iocc=ocgril, nbval=ntypg,&
                        vect=zr( lfsgr+nbgtot+ntypg), nbret=ibid)
            call getvr8(nommcf, 'EPAI_TYPG', iocc=ocgril, nbval=ntypg,&
                        vect=zr( lfsgr+nbgtot+2*ntypg), nbret=ibid)
            call getvr8(nommcf, 'COEF_TRAI_TYPG', iocc=ocgril, nbval=ntypg,&
                        vect=zr(lfsgr+nbgtot+3*ntypg), nbret=ibid)
            call getvr8(nommcf, 'COEF_DPOR_TYPG', iocc=ocgril, nbval=ntypg,&
                        vect=zr(lfsgr+nbgtot+4*ntypg), nbret=ibid)
            call getvr8(nommcf, 'RUGO_TYPG', iocc=ocgril, nbval=ntypg,&
                        vect=zr( lfsgr+nbgtot+5*ntypg), nbret=ibid)
        endif
!
! --------3.3.INFORMATIONS PARTICULIERES POUR UN FAISCEAU EQUIVALENT
        if (iequiv .eq. 1) then
!
            zi(lfsvi+5) = nbcoor/2
            idecvr = 5 + nbcara + nbangl
            ideccr = 0
            do iocc = 1, nbocc
                nbcr = zi(lnbcr+iocc-1)
                zi(lfsvi+6+iocc-1) = nbcr/2
                call getvr8(nommcf, 'RAYON_TUBE', iocc=iocc, scal=zr(lfsvr+ idecvr+iocc-1),&
                            nbret=ibid)
                call getvtx(nommcf, 'GROUP_MA', iocc=iocc, scal=zk24(lfsgm+ iocc-1), nbret=ibid)
                call getvr8(nommcf, 'COOR_TUBE', iocc=iocc, nbval=nbcr, vect=zr(lfscr+ideccr),&
                            nbret=ibid)
                ideccr = ideccr + nbcr
            end do
!
! --------3.4.INFORMATIONS PARTICULIERES POUR UN FAISCEAU COMPLET
        else
!
            call getvid(nommcf, 'CARA_ELEM', iocc=1, scal=zk8(lfsvk+2), nbret=ibid)
            if (nbgrma .ne. 0) then
                call getvtx(nommcf, 'GROUP_MA', iocc=1, nbval=nbgrma, vect=zk24( lfsgm),&
                            nbret=ibid)
            else
                call getvtx(nommcf, 'TRI_GROUP_MA', iocc=1, scal=zk24( lfsgm), nbret=ibid)
            endif
!
        endif
!-----------------------------------------------------------------------
! ----- 4.CAS DE COQUE_COAX
!       -------------------
    else
!
        zi(lfsic+1) = 1
        call wkvect(fsvi, 'G V I', 2, lfsvi)
        call getvtx(nommcf, 'MASS_AJOU', iocc=1, scal=ouinon, nbret=ibid)
        if (ouinon .eq. 'OUI') then
            zi(lfsvi) = 1
        else
            zi(lfsvi) = 0
        endif
        call getvr8(nommcf, 'VECT_X', iocc=1, nbval=3, vect=vect(1),&
                    nbret=ibid)
        if (abs(vect(1)-1.d0) .le. r8prem()) then
            zi(lfsvi+1) = 1
        else if (abs(vect(2)-1.d0) .le. r8prem()) then
            zi(lfsvi+1) = 2
        else
            zi(lfsvi+1) = 3
        endif
!
        call wkvect(fsvk, 'G V K8', 3, lfsvk)
        call getvid(nommcf, 'CARA_ELEM', iocc=1, scal=zk8(lfsvk), nbret=ibid)
        call getvid(nommcf, 'MATER_INT', iocc=1, scal=zk8(lfsvk+1), nbret=ibid)
        call getvid(nommcf, 'MATER_EXT', iocc=1, scal=zk8(lfsvk+2), nbret=ibid)
!
        call wkvect(fsvr, 'G V R', 7, lfsvr)
        call getvr8(nommcf, 'RHO_FLUI', iocc=1, scal=zr(lfsvr), nbret=ibid)
        call getvr8(nommcf, 'VISC_CINE', iocc=1, scal=zr(lfsvr+1), nbret=ibid)
        call getvr8(nommcf, 'RUGOSITE', iocc=1, scal=zr(lfsvr+2), nbret=ibid)
        call getvr8(nommcf, 'PDC_MOY_1', iocc=1, scal=zr(lfsvr+3), nbret=ibid)
        call getvr8(nommcf, 'PDC_DYN_1', iocc=1, scal=zr(lfsvr+4), nbret=ibid)
        call getvr8(nommcf, 'PDC_MOY_2', iocc=1, scal=zr(lfsvr+5), nbret=ibid)
        call getvr8(nommcf, 'PDC_DYN_2', iocc=1, scal=zr(lfsvr+6), nbret=ibid)
!
        call wkvect(fsgm, 'G V K24', 2, lfsgm)
        call getvtx(nommcf, 'GROUP_MA_INT', iocc=1, scal=zk24(lfsgm), nbret=ibid)
        call getvtx(nommcf, 'GROUP_MA_EXT', iocc=1, scal=zk24(lfsgm+1), nbret=ibid)
!
    endif
!=======================================================================
! ----- IMPRESSION -----
!       ==========
    call getvis(' ', 'INFO', scal=niv, nbret=ibid)
    if (niv .eq. 2) call tfimpr(nomu)
!
    call jedetr('&&OP0143.TEMP.NBCR')
    call jedetc('G', 'AJGR2.FLAG', 4)
!
    100 format(1p,'    COEF_MASS_AJOU CALCULE: ',e12.5)
!
    call jedema()
end subroutine
