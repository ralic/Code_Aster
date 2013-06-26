subroutine op0143()
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1501
    implicit none
!-----------------------------------------------------------------------
!
!     OPERATEUR "DEFI_FLUI_STRU"
!
!-----------------------------------------------------------------------
!
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8pi.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/tfimpr.h'
    include 'asterfort/tfveri.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: ocvect, ocprho, ocpvis, ocpesa, ocrugo, occapa, ocgril, iret, ifm
    integer :: niv
    integer :: dimvi, dimvk, dimvr, dimgm, dimgr
    integer :: iunit, unit1, unit2, ibid1, ibid2
    integer :: irho, jrho
    character(len=2) :: carapa(4)
    character(len=3) :: ouinon
    character(len=8) :: k8bid
    character(len=9) :: typas(2), tpas
    character(len=16) :: concep, cmd, nommcf, mcfac(4)
    character(len=19) :: nomu
    character(len=8) :: nomu8
    character(len=24) :: fsic, fsvi, fsvr, fsvk, fsgm, fscr, fsgr
    real(kind=8) :: vect(3), valepa(4)
    integer :: iarg
!
!       DATA TYPAS   /'CARRE_LIGN ','TRIA_LIGN'/
!-----------------------------------------------------------------------
    integer :: i, ibid, icael, icar, icara, icm, icmp
    integer :: icoor, icoup, ideccr, idecvr, ience, iequiv, iocc
    integer :: ipas, ipesa, iprho, ipvis, irhoe, irhoi, irugo
    integer :: itpas, itres, itypf2, itypfl, ivect, j, jcm
    integer :: jcoup, jpas, jtpas, lfscr, lfsgm, lfsgr, lfsic
    integer :: lfsvi, lfsvk, lfsvr, lnbcr, nbangl, nbcara, nbcoor
    integer :: nbcr, nbgrma, nbgtot, nbocc, ntypg, nzex
    real(kind=8) :: pas, pi, rbid, y
!-----------------------------------------------------------------------
    data typas   /'CARRE_LIG','TRIA_LIGN'/
    data mcfac   /'FAISCEAU_TRANS ','GRAPPE',&
     &                'FAISCEAU_AXIAL ','COQUE_COAX'/
!=======================================================================
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
    pi = r8pi()
!
    call getres(nomu, concep, cmd)
    nomu8=nomu
!
    do 10 itypf2 = 1, 4
        call getfac(mcfac(itypf2), nbocc)
        if (nbocc .ge. 1) goto 11
10  end do
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
        call getvis(nommcf, 'UNITE_CD', 1, iarg, 1,&
                    unit1, ibid1)
        call getvis(nommcf, 'UNITE_CK', 1, iarg, 1,&
                    unit2, ibid2)
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
        do 20 iocc = 1, nbocc
! ---     RECHERCHE DE LA DERNIERE OCCURENCE DES MOTS-CLES OBLIGATOIRES
            call getvtx(nommcf, 'COUPLAGE', iocc, iarg, 0,&
                        k8bid, icoup)
            if (icoup .ne. 0) jcoup = iocc
            call getvid(nommcf, 'CARA_ELEM', iocc, iarg, 0,&
                        k8bid, icara)
            call getvid(nommcf, 'PROF_RHO_F_INT', iocc, iarg, 0,&
                        k8bid, irhoi)
            call getvid(nommcf, 'PROF_RHO_F_EXT', iocc, iarg, 0,&
                        k8bid, irhoe)
            call getvtx(nommcf, 'NOM_CMP       ', iocc, iarg, 0,&
                        k8bid, icmp)
! ---     RECHERCHE DE LA DERNIERE OCCURENCE DES MOTS-CLES FACULTATIFS
            call getvr8(nommcf, 'COEF_MASS_AJOU', iocc, iarg, 0,&
                        rbid, icm)
            if (icm .ne. 0) jcm = iocc
            call getvr8(nommcf, 'RHO_TUBE', iocc, iarg, 0,&
                        rbid, irho)
            if (irho .ne. 0) jrho=iocc
            call getvtx(nommcf, 'TYPE_PAS', iocc, iarg, 0,&
                        k8bid, itpas)
            if (itpas .ne. 0) jtpas = iocc
            call getvis(nommcf, 'TYPE_RESEAU', iocc, iarg, 0,&
                        ibid, itres)
            call getvr8(nommcf, 'PAS', iocc, iarg, 0,&
                        rbid, ipas)
            if (ipas .ne. 0) jpas = iocc
20      continue
!
        nzex = nbocc
        call getvtx(nommcf, 'COUPLAGE', jcoup, iarg, 1,&
                    ouinon, ibid)
!
! --------1.1.SI PRISE EN COMPTE DU COUPLAGE
        if (ouinon .eq. 'OUI') then
!
            zi(lfsic+1) = 1
!
            call wkvect(fsvr, 'G V R', 3+2*nzex, lfsvr)
            call wkvect(fsvi, 'G V I', 2+2*nzex, lfsvi)
            zi(lfsvi+1) = nzex
            do 30 iocc = 1, nzex
                call getvis(nommcf, 'TYPE_RESEAU', iocc, iarg, 1,&
                            zi( lfsvi+1+iocc), ibid)
                call getvr8(nommcf, 'CSTE_CONNORS', iocc, iarg, 2,&
                            zr(lfsvr+2*iocc+1), ibid)
                call getvis(nommcf, 'NB_CONNORS', iocc, iarg, 2,&
                            zi(lfsvi+ 1+nzex+iocc), ibid)
30          continue
            call getvtx(nommcf, 'TYPE_PAS', jtpas, iarg, 1,&
                        tpas, ibid)
            if (tpas .eq. typas(1)) then
                zi(lfsvi) = 1
            else
                zi(lfsvi) = 2
            endif
!
! ---------- PAS REDUIT
            call getvr8(nommcf, 'PAS', jpas, iarg, 1,&
                        pas, ibid)
            if (jcm .eq. 0) then
                if (zi(lfsvi) .eq. 2) then
! ---------------- RESEAU A PAS TRIANGULAIRE
                    y = (0.96d0+0.5d0*pas)*pas
                else
! ---------------- RESEAU A PAS CARRE
                    y = (1.07d0+0.56d0*pas)*pas
                endif
                zr(lfsvr) = (pi*(y*y+1.d0))/(2.d0*(y*y-1.d0))
                if (niv .eq. 2) write(ifm,1000) zr(lfsvr)
            else
                call getvr8(nommcf, 'COEF_MASS_AJOU', jcm, iarg, 1,&
                            zr(lfsvr), ibid)
            endif
            zr(lfsvr+1) = pas
!
            call getvr8(nommcf, 'RHO_TUBE', jrho, iarg, 1,&
                        zr(lfsvr+2), irho)
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
            call getvr8(nommcf, 'COEF_MASS_AJOU', jcm, iarg, 1,&
                        zr(lfsvr), ibid)
!
        endif
!
! --------1.3.DANS LES DEUX CAS CREATION ET REMPLISSAGE DU .FSVK
        call wkvect(fsvk, 'G V K8', 4+nzex, lfsvk)
        call getvid(nommcf, 'CARA_ELEM', 1, iarg, 1,&
                    zk8(lfsvk), ibid)
        call getvtx(nommcf, 'NOM_CMP', 1, iarg, 1,&
                    zk8(lfsvk+1), ibid)
        call getvid(nommcf, 'PROF_RHO_F_INT', 1, iarg, 1,&
                    zk8(lfsvk+2), ibid)
        call getvid(nommcf, 'PROF_RHO_F_EXT', 1, iarg, 1,&
                    zk8(lfsvk+3), ibid)
        do 40 iocc = 1, nzex
            call getvid(nommcf, 'PROF_VITE_FLUI', iocc, iarg, 1,&
                        zk8( lfsvk+3+iocc), ibid)
40      continue
!
! -------1.4.VERIFICATION DES NUMERO ET NOMS DE ZONE D EXCITATION DU
!            FLUIDE
        do 50 i = 1, nzex-1
            do 50 j = i+1, nzex
                if (zk8(lfsvk+i+3) .eq. zk8(lfsvk+j+3)) then
                    call u2mess('F', 'MODELISA5_65')
                endif
50          continue
!-----------------------------------------------------------------------
! ----- 2.CAS D'UNE GRAPPE
!       ------------------
    else if (itypfl.eq.2) then
!
        call getvtx(nommcf, 'COUPLAGE', 1, iarg, 1,&
                    ouinon, ibid)
!
! --------2.1.SI PRISE EN COMPTE DU COUPLAGE
        if (ouinon .eq. 'OUI') then
!
            zi(lfsic+1) = 1
!
            call wkvect(fsvk, 'G V K8', 4, lfsvk)
            call getvtx(nommcf, 'GRAPPE_2', 1, iarg, 1,&
                        zk8(lfsvk), ibid)
            call getvtx(nommcf, 'NOEUD', 1, iarg, 1,&
                        zk8(lfsvk+1), ibid)
            call getvid(nommcf, 'CARA_ELEM', 1, iarg, 1,&
                        zk8(lfsvk+2), ibid)
            call getvid(nommcf, 'MODELE', 1, iarg, 1,&
                        zk8(lfsvk+3), ibid)
!
! ----      STOCKAGE DE L'UNITE LOGIQUE
            call getvis(nommcf, 'UNITE_CA', 1, iarg, 1,&
                        unit1, ibid1)
            call getvis(nommcf, 'UNITE_KA', 1, iarg, 1,&
                        unit2, ibid2)
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
            call getvr8(nommcf, 'COEF_MASS_AJOU', 1, iarg, 0,&
                        rbid, icm)
            if (icm .ne. 0) then
                call getvr8(nommcf, 'COEF_MASS_AJOU', 1, iarg, 1,&
                            zr( lfsvr), ibid)
            else
                zr(lfsvr) = pi * 1.078014d0
            endif
            call getvr8(nommcf, 'RHO_FLUI', 1, iarg, 1,&
                        zr(lfsvr+1), ibid)
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
            call getvid(nommcf, 'CARA_ELEM', 1, iarg, 0,&
                        k8bid, icael)
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
            call getvtx(nommcf, 'GROUP_MA', 1, iarg, 0,&
                        k8bid, nbgrma)
            nbgrma = abs(nbgrma)
            call getvr8(nommcf, 'PESANTEUR', 1, iarg, 0,&
                        rbid, ipesa)
            if (ipesa .ne. 0) ocpesa = 1
            call getvtx(nommcf, 'CARA_PAROI', 1, iarg, 0,&
                        k8bid, nbcara)
            nbcara = abs(nbcara)
            if (nbcara .eq. 3) then
                ience = 1
                nbangl = 0
            else
                ience = 2
                nbangl = 1
            endif
            call getvr8(nommcf, 'COOR_GRILLE', 1, iarg, 0,&
                        rbid, nbgtot)
            if (nbgtot .ne. 0) then
                ocgril = 1
                nbgtot = abs(nbgtot)
                call getvr8(nommcf, 'LONG_TYPG', 1, iarg, 0,&
                            rbid, ntypg)
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
            do 60 iocc = 1, nbocc
                call getvr8(nommcf, 'VECT_X', iocc, iarg, 0,&
                            rbid, ivect)
                if (ivect .ne. 0) ocvect = iocc
                call getvid(nommcf, 'PROF_RHO_FLUI', iocc, iarg, 0,&
                            k8bid, iprho)
                if (iprho .ne. 0) ocprho = iocc
                call getvid(nommcf, 'PROF_VISC_CINE', iocc, iarg, 0,&
                            k8bid, ipvis)
                if (ipvis .ne. 0) ocpvis = iocc
                call getvr8(nommcf, 'PESANTEUR', iocc, iarg, 0,&
                            rbid, ipesa)
                if (ipesa .ne. 0) ocpesa = iocc
                call getvr8(nommcf, 'RUGO_TUBE', iocc, iarg, 0,&
                            rbid, irugo)
                if (irugo .ne. 0) ocrugo = iocc
                call getvtx(nommcf, 'CARA_PAROI', iocc, iarg, 0,&
                            k8bid, icara)
                if (icara .ne. 0) occapa = iocc
                call getvr8(nommcf, 'COOR_TUBE', iocc, iarg, 0,&
                            rbid, icoor)
                icoor = abs(icoor)
                zi(lnbcr+iocc-1) = icoor
                nbcoor = nbcoor + icoor
                call getvr8(nommcf, 'COOR_GRILLE', iocc, iarg, 0,&
                            rbid, nbgtot)
                if (nbgtot .ne. 0) ocgril = iocc
60          continue
!
            call getvtx(nommcf, 'CARA_PAROI', occapa, iarg, 0,&
                        k8bid, nbcara)
            nbcara = abs(nbcara)
            if (nbcara .eq. 3) then
                ience = 1
                nbangl = 0
            else
                ience = 2
                nbangl = 1
            endif
            if (ocgril .ne. 0) then
                call getvr8(nommcf, 'COOR_GRILLE', ocgril, iarg, 0,&
                            rbid, nbgtot)
                nbgtot = abs(nbgtot)
                call getvr8(nommcf, 'LONG_TYPG', ocgril, iarg, 0,&
                            rbid, ntypg)
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
        call getvr8(nommcf, 'VECT_X', ocvect, iarg, 3,&
                    vect(1), ibid)
        if (vect(1) .eq. 1.d0) then
            zi(lfsvi+1) = 1
        else if (vect(2).eq.1.d0) then
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
                call getvis(nommcf, 'TYPE_GRILLE', ocgril, iarg, nbgtot,&
                            zi(lfsvi+6), ibid)
            else
                zi(lfsvi+6+nbgrma) = nbgtot
                call getvis(nommcf, 'TYPE_GRILLE', ocgril, iarg, nbgtot,&
                            zi(lfsvi+7+nbgrma), ibid)
            endif
        endif
!
! --------3.2.2.OBJET .FSVK
        call getvid(nommcf, 'PROF_RHO_FLUI', ocprho, iarg, 1,&
                    zk8(lfsvk), ibid)
        call getvid(nommcf, 'PROF_VISC_CINE', ocpvis, iarg, 1,&
                    zk8(lfsvk+ 1), ibid)
!
! --------3.2.3.OBJET .FSVR
        if (ocpesa .ne. 0) then
            call getvr8(nommcf, 'PESANTEUR', ocpesa, iarg, 4,&
                        zr(lfsvr), ibid)
        else
            zr(lfsvr) = 9.81d0
            zr(lfsvr+1) = 0.d0
            zr(lfsvr+2) = 0.d0
            zr(lfsvr+3) = -1.d0
        endif
        call getvr8(nommcf, 'RUGO_TUBE', ocrugo, iarg, 1,&
                    zr(lfsvr+4), ibid)
!
        call getvtx(nommcf, 'CARA_PAROI', occapa, iarg, nbcara,&
                    carapa(1), ibid)
        call getvr8(nommcf, 'VALE_PAROI', occapa, iarg, nbcara,&
                    valepa(1), ibid)
        if (ience .eq. 1) then
            do 70 icar = 1, nbcara
                if (carapa(icar) .eq. 'YC') zr(lfsvr+5) = valepa(icar)
                if (carapa(icar) .eq. 'ZC') zr(lfsvr+6) = valepa(icar)
                if (carapa(icar)(1:1) .eq. 'R') zr(lfsvr+7) = valepa( icar)
70          continue
        else
            do 80 icar = 1, nbcara
                if (carapa(icar) .eq. 'YC') zr(lfsvr+5) = valepa(icar)
                if (carapa(icar) .eq. 'ZC') zr(lfsvr+6) = valepa(icar)
                if (carapa(icar) .eq. 'HY') zr(lfsvr+7) = valepa(icar)
                if (carapa(icar) .eq. 'HZ') zr(lfsvr+8) = valepa(icar)
80          continue
            call getvr8(nommcf, 'ANGL_VRIL', occapa, iarg, 1,&
                        zr(lfsvr+9), ibid)
        endif
!
! --------3.2.4.OBJET .FSGR
        if (ocgril .ne. 0) then
            call getvr8(nommcf, 'COOR_GRILLE', ocgril, iarg, nbgtot,&
                        zr(lfsgr), ibid)
            call getvr8(nommcf, 'LONG_TYPG', ocgril, iarg, ntypg,&
                        zr( lfsgr+nbgtot), ibid)
            call getvr8(nommcf, 'LARG_TYPG', ocgril, iarg, ntypg,&
                        zr( lfsgr+nbgtot+ntypg), ibid)
            call getvr8(nommcf, 'EPAI_TYPG', ocgril, iarg, ntypg,&
                        zr( lfsgr+nbgtot+2*ntypg), ibid)
            call getvr8(nommcf, 'COEF_TRAI_TYPG', ocgril, iarg, ntypg,&
                        zr(lfsgr+nbgtot+3*ntypg), ibid)
            call getvr8(nommcf, 'COEF_DPOR_TYPG', ocgril, iarg, ntypg,&
                        zr(lfsgr+nbgtot+4*ntypg), ibid)
            call getvr8(nommcf, 'RUGO_TYPG', ocgril, iarg, ntypg,&
                        zr( lfsgr+nbgtot+5*ntypg), ibid)
        endif
!
! --------3.3.INFORMATIONS PARTICULIERES POUR UN FAISCEAU EQUIVALENT
        if (iequiv .eq. 1) then
!
            zi(lfsvi+5) = nbcoor/2
            idecvr = 5 + nbcara + nbangl
            ideccr = 0
            do 90 iocc = 1, nbocc
                nbcr = zi(lnbcr+iocc-1)
                zi(lfsvi+6+iocc-1) = nbcr/2
                call getvr8(nommcf, 'RAYON_TUBE', iocc, iarg, 1,&
                            zr(lfsvr+ idecvr+iocc-1), ibid)
                call getvtx(nommcf, 'GROUP_MA', iocc, iarg, 1,&
                            zk24(lfsgm+ iocc-1), ibid)
                call getvr8(nommcf, 'COOR_TUBE', iocc, iarg, nbcr,&
                            zr(lfscr+ideccr), ibid)
                ideccr = ideccr + nbcr
90          continue
!
! --------3.4.INFORMATIONS PARTICULIERES POUR UN FAISCEAU COMPLET
        else
!
            call getvid(nommcf, 'CARA_ELEM', 1, iarg, 1,&
                        zk8(lfsvk+2), ibid)
            if (nbgrma .ne. 0) then
                call getvtx(nommcf, 'GROUP_MA', 1, iarg, nbgrma,&
                            zk24( lfsgm), ibid)
            else
                call getvtx(nommcf, 'TRI_GROUP_MA', 1, iarg, 1,&
                            zk24( lfsgm), ibid)
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
        call getvtx(nommcf, 'MASS_AJOU', 1, iarg, 1,&
                    ouinon, ibid)
        if (ouinon .eq. 'OUI') then
            zi(lfsvi) = 1
        else
            zi(lfsvi) = 0
        endif
        call getvr8(nommcf, 'VECT_X', 1, iarg, 3,&
                    vect(1), ibid)
        if (vect(1) .eq. 1.d0) then
            zi(lfsvi+1) = 1
        else if (vect(2).eq.1.d0) then
            zi(lfsvi+1) = 2
        else
            zi(lfsvi+1) = 3
        endif
!
        call wkvect(fsvk, 'G V K8', 3, lfsvk)
        call getvid(nommcf, 'CARA_ELEM', 1, iarg, 1,&
                    zk8(lfsvk), ibid)
        call getvid(nommcf, 'MATER_INT', 1, iarg, 1,&
                    zk8(lfsvk+1), ibid)
        call getvid(nommcf, 'MATER_EXT', 1, iarg, 1,&
                    zk8(lfsvk+2), ibid)
!
        call wkvect(fsvr, 'G V R', 7, lfsvr)
        call getvr8(nommcf, 'RHO_FLUI', 1, iarg, 1,&
                    zr(lfsvr), ibid)
        call getvr8(nommcf, 'VISC_CINE', 1, iarg, 1,&
                    zr(lfsvr+1), ibid)
        call getvr8(nommcf, 'RUGOSITE', 1, iarg, 1,&
                    zr(lfsvr+2), ibid)
        call getvr8(nommcf, 'PDC_MOY_1', 1, iarg, 1,&
                    zr(lfsvr+3), ibid)
        call getvr8(nommcf, 'PDC_DYN_1', 1, iarg, 1,&
                    zr(lfsvr+4), ibid)
        call getvr8(nommcf, 'PDC_MOY_2', 1, iarg, 1,&
                    zr(lfsvr+5), ibid)
        call getvr8(nommcf, 'PDC_DYN_2', 1, iarg, 1,&
                    zr(lfsvr+6), ibid)
!
        call wkvect(fsgm, 'G V K24', 2, lfsgm)
        call getvtx(nommcf, 'GROUP_MA_INT', 1, iarg, 1,&
                    zk24(lfsgm), ibid)
        call getvtx(nommcf, 'GROUP_MA_EXT', 1, iarg, 1,&
                    zk24(lfsgm+1), ibid)
!
    endif
!=======================================================================
! ----- IMPRESSION -----
!       ==========
    call getvis(' ', 'INFO', 0, iarg, 1,&
                niv, ibid)
    if (niv .eq. 2) call tfimpr(nomu)
!
    call jedetr('&&OP0143.TEMP.NBCR')
    call jedetc('G', 'AJGR2.FLAG', 4)
!
    1000 format(1p,'    COEF_MASS_AJOU CALCULE: ',e12.5)
!
    call jedema()
end subroutine
