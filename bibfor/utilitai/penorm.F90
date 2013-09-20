subroutine penorm(resu, modele)
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/calc_coor_elga.h"
#include "asterfort/calc_norm_coef.h"
#include "asterfort/calc_norm_elem.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesred.h"
#include "asterfort/chpchd.h"
#include "asterfort/chsut1.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismlg.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/megeom.h"
#include "asterfort/mesomm.h"
#include "asterfort/nopar2.h"
#include "asterfort/reliem.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utflmd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: modele
    character(len=19) :: resu
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
!     ------------------------------------------------------------------
!     OPERATEUR :  POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR : "NORME"
!     ------------------------------------------------------------------
!
    integer :: nb_cmp_max, nbpmax
    parameter(nbpmax=13,nb_cmp_max=31)
    character(len=8) :: typpar(nbpmax)
    character(len=16) :: nompar(nbpmax)
!
    integer :: ibid, iret, nbmato, nr, nd, np, nc, ni, no, nli, nlo, nb_coef_user
    integer :: jno, jin, j_coef_user, tord(1)
    integer :: nbpar, inum, numo, iresma, nbordr, jlicmp, jlicm1, jma
    integer :: nn
    integer :: jlicm2, i, nncp, nbma, jvalk, jvalr, jvali, ncmpm, ifm, niv
    integer :: nb_cmp_act
    real(kind=8) :: prec, inst, vnorm(1)
    complex(kind=8) :: c16b
    logical :: exiord
    character(len=4) :: tych, ki, exirdm
    character(len=8) :: mailla, k8b, resuco, chamg, typmcl(1), tout
    character(len=8) :: tmpres, nomgd, crit
    character(len=8) :: nopar, infoma
    character(len=16) :: tynorm
    character(len=19) :: knum, kins, lisins, cham2, chamtm, celmod, ligrel
    character(len=19) :: liscoe, field_resu
    character(len=19) :: tmpcha, cham1, chgaus, chgeom, chcalc
    character(len=16) :: optio2, nomcha, valk, mocles(1)
    character(len=24) :: mesmai, mesmaf, valr, vali
    character(len=24) :: coefca, valk2(5), grouma
    character(len=24) :: list_cmp, list_cmp_neut
!     ------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    valr = '&&PENORM.VALR'
    vali = '&&PENORM.VALI'
    valk = '&&PENORM.VALK'
    knum = '&&PENORM.NUME_ORDRE'
    kins = '&&PENORM.INST'
    mesmai = '&&PENORM.MES_MAILLES'
    mesmaf = '&&PENORM.MAIL_FILTRE'
    cham1 = '&&PENORM.CHAM1'
    cham2 = '&&PENORM.CHAM2'
    chamtm = '&&PENORM.CHAMTM'
    ligrel = '&&PENORM.LIGREL'
    coefca = '&&PENORM.CARTE_COEF'
    chcalc = '&&PENORM.CHCALC'
    liscoe = '&&PENORM.COEFMULT'
    list_cmp = '&&PENORM.CMP1'
    list_cmp_neut = '&&PENORM.CMP2'
    field_resu = '&&PENORM.NORM_L2'
    chgaus = '&&PENORM.CHGAUS'
    chgeom = '&&PENORM.CHGEOM'
    exiord=.false.
    nb_cmp_act =0
    ligrel = modele//'.MODELE'
!
! - Geometry
!
    call megeom(modele, chgeom)
!
! --- 1- RECUPERATION DU MAILLAGE ET DU NOMBRE DE MAILLES
!     ===================================================
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                mailla, iret)
    call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbmato,&
                k8b, iret)
!
!
! --- 2- RECUPERATION DU RESULTAT ET DES NUMEROS D'ORDRE
!     ==================================================
    call getvid('NORME', 'RESULTAT', iocc=1, scal=resuco, nbret=nr)
    call getvid('NORME', 'CHAM_GD', iocc=1, scal=chamg, nbret=nd)
!
    call getvr8('NORME', 'PRECISION', iocc=1, scal=prec, nbret=np)
    call getvtx('NORME', 'CRITERE', iocc=1, scal=crit, nbret=nc)
    call getvr8('NORME', 'INST', iocc=1, nbval=0, nbret=ni)
    call getvis('NORME', 'NUME_ORDRE', iocc=1, nbval=0, nbret=no)
    call getvid('NORME', 'LIST_INST', iocc=1, nbval=0, nbret=nli)
    call getvid('NORME', 'LIST_ORDRE', iocc=1, nbval=0, nbret=nlo)
    call getvtx('NORME', 'TYPE_NORM', iocc=1, scal=tynorm, nbret=iret)
    call getvr8('NORME', 'COEF_MULT', iocc=1, nbval=0, nbret=nb_coef_user)
    if (nb_coef_user .ne. 0) then
        nb_coef_user = -nb_coef_user
        call wkvect(liscoe, 'V V R', nb_coef_user, j_coef_user)
        call getvr8('NORME', 'COEF_MULT', iocc=1, nbval=nb_coef_user, vect=zr(j_coef_user),&
                    nbret=iret)
    endif
!
    if (nd .ne. 0) then
!
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jno)
        zi(jno) = 1
        exiord=.true.
!
    else
!
!       -- NUME_ORDRE --
        if (no .ne. 0) then
            exiord=.true.
            nbordr=-no
            call wkvect(knum, 'V V I', nbordr, jno)
            call getvis('NORME', 'NUME_ORDRE', iocc=1, nbval=nbordr, vect=zi(jno),&
                        nbret=iret)
        endif
!
!       -- LIST_ORDRE --
        if (nlo .ne. 0) then
            exiord=.true.
            call getvid('NORME', 'LIST_ORDRE', iocc=1, scal=lisins, nbret=iret)
            call jeveuo(lisins // '.VALE', 'L', jno)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr)
        endif
!
!       -- INST --
        if (ni .ne. 0) then
            nbordr=-ni
            call wkvect(kins, 'V V R', nbordr, jin)
            call getvr8('NORME', 'INST', iocc=1, nbval=nbordr, vect=zr(jin),&
                        nbret=iret)
        endif
!
!       -- LIST_INST --
        if (nli .ne. 0) then
            call getvid('NORME', 'LIST_INST', iocc=1, scal=lisins, nbret=iret)
            call jeveuo(lisins // '.VALE', 'L', jin)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr)
        endif
!
!       -- TOUT_ORDRE --
        nn=nli+ni+no+nlo
        if (nn .eq. 0) then
            exiord=.true.
            call rsutnu(resuco, ' ', 0, knum, nbordr,&
                        prec, crit, iret)
            call jeveuo(knum, 'L', jno)
        endif
!
    endif
!
! --- 3- CREATION DE LA TABLE
!     =======================
    call tbcrsd(resu, 'G')
    if (nr .ne. 0) then
!
        nbpar=8
!
        nompar(1) ='RESULTAT'
        nompar(2) ='NOM_CHAM'
        nompar(3) ='NUME_ORDRE'
        nompar(4) ='INST'
        nompar(5) ='GROUP_MA'
        nompar(6) ='TYPE_MAIL'
        nompar(7) ='TYPE_NORM'
        nompar(8) ='VALE_NORM'
!
        typpar(1) ='K8'
        typpar(2) ='K16'
        typpar(3) ='I'
        typpar(4) ='R'
        typpar(5) ='K24'
        typpar(6) ='K8'
        typpar(7) ='K8'
        typpar(8) ='R'
    else
!
        nbpar=5
!
        nompar(1)='CHAM_GD'
        nompar(2)='GROUP_MA'
        nompar(3)='TYPE_MAIL'
        nompar(4)='TYPE_NORM'
        nompar(5)='VALE_NORM'
!
        typpar(1)='K8'
        typpar(2)='K24'
        typpar(3)='K8'
        typpar(4)='K8'
        typpar(5)='R'
!
    endif
    call tbajpa(resu, nbpar, nompar, typpar)
!
! --- 4- REMPLISSAGE DE LA TABLE
!     ==========================
!
!  -- ON COPIE LE CHAMP POUR NE PAS MODIFIER LA DISCRETISATION
!     DU CHAMP INITIAL
    if (nr .ne. 0) then
        tmpres='TMP_RESU'
        call copisd('RESULTAT', 'V', resuco, tmpres)
    else
        tmpcha='TMP_CHAMP_GD'
        call copisd('CHAMP', 'V', chamg, tmpcha)
    endif
!
!     -- VERIFICATION DE L'UTILISATION DE COEF_MULT
    if (nr .eq. 0 .and. nb_coef_user .ne. 0) then
        call dismoi('C', 'NOM_GD', tmpcha, 'CHAMP', ibid,&
                    nomgd, iret)
        if (nomgd(1:6) .ne. 'NEUT_R') then
            call utmess('F', 'POSTELEM_2')
        endif
    endif
!
!  --- BOUCLE SUR LES NUMEROS D'ORDRE:
!     ----------------------------------
    do 5 inum = 1, nbordr
!
!      -- 4.1 RECUPERATION DU CHAMP --
!
        if (nr .ne. 0) then
!         --  RESULTAT --
            if (exiord) then
!           - ORDRE -
                numo=zi(jno+inum-1)
                call rsadpa(resuco, 'L', 1, 'INST', numo,&
                            0, sjv=jin, styp=k8b)
                inst=zr(jin)
            else
!           - INST -
                inst=zr(jin+inum-1)
                call rsorac(resuco, 'INST', 0, zr(jin+inum-1), k8b,&
                            c16b, prec, crit, tord, nbordr,&
                            iret)
                numo=tord(1)            
            endif
            call getvtx('NORME', 'NOM_CHAM', iocc=1, scal=nomcha, nbret=iret)
            if (iret .eq. 0) then
                call utmess('F', 'POSTELEM_4')
            endif
            call rsexch(' ', tmpres, nomcha, numo, cham2,&
                        iret)
!
        else
!         -- CHAM_GD --
            numo = nbordr
            cham2 = tmpcha
            nomcha= chamg
        endif
!
        call dismoi('C', 'TYPE_CHAMP', cham2, 'CHAMP', ibid,&
                    tych, iret)
        call dismoi('C', 'NOM_GD', cham2, 'CHAMP', ibid,&
                    nomgd, iret)
!
!         ON RESTREINT LE CALCUL DE LA NORME AUX CHAMPS DE DEPLACEMENTS,
!         CONTRAINTES, DEFORMATION, TEMPERATURE, FLUX ...
        if (nomgd(6:6) .eq. 'C') goto 999
        if (nomgd(1:4) .ne. 'DEPL' .and. nomgd(1:4) .ne. 'EPSI' .and. nomgd(1:4) .ne. 'TEMP'&
            .and. nomgd(1:4) .ne. 'FLUX' .and. nomgd(1:4) .ne. 'SIEF' .and. nomgd(1:6) .ne.&
            'NEUT_R') goto 999
!
!      -- 4.2 RECUPERATION DES MAILLES --
!
        call getvtx('NORME', 'TOUT', iocc=1, scal=tout, nbret=iret)
!
        if (iret .ne. 0) then
            mocles(1) = 'TOUT'
            typmcl(1) = 'TOUT'
            grouma = '-'
        else
            mocles(1) = 'GROUP_MA'
            typmcl(1) = 'GROUP_MA'
            call getvtx('NORME', 'GROUP_MA', iocc=1, scal=grouma, nbret=iret)
        endif
!
!       - MAILLES FOURNIES PAR L'UTILISATEUR -
        call reliem(modele, mailla, 'NU_MAILLE', 'NORME', 1,&
                    1, mocles, typmcl, mesmai, nbma)
!
!       - MAILLES EVENTUELLEMENT FILTREES EN FONCTION DE LA DIMENSION
!         GEOMETRIQUE (2D OU 3D)
        call getvtx('NORME', 'TYPE_MAILLE', iocc=1, scal=infoma, nbret=iret)
        if (iret .ne. 0) then
            if (infoma(1:2) .eq. '2D') then
                iresma=2
            else if (infoma(1:2).eq.'3D') then
                iresma=3
            else
                ASSERT(.false.)
            endif
            call utflmd(mailla, mesmai, nbma, iresma, ' ',&
                         nbma, mesmaf)
            call jedetr(mesmai)
            mesmai=mesmaf
        else
            infoma='-'
        endif
!
!       - VERIFICATION SI ON VA TRAITER DES ELEMENTS DE STRUCTURE
        call dismlg('EXI_RDM', mesmai, ibid, exirdm, iret)
        if (exirdm .eq. 'OUI') then
            call utmess('F', 'UTILITAI8_60')
        endif
!
! ----- Convert CHAMNO/CHAMELEM input field to CHAMNO_S/CHAMELEM_S field
!
        if (tych(1:4) .eq. 'NOEU') then
            call cnocns(cham2, 'V', chamtm)
            call jeveuo(chamtm//'.CNSC', 'L', jlicmp)
            call jelira(chamtm//'.CNSC', 'LONMAX', ncmpm)
        else if (tych(1:2).eq.'EL') then
            call celces(cham2, 'V', chamtm)
            call jeveuo(chamtm//'.CESC', 'L', jlicmp)
            call jelira(chamtm//'.CESC', 'LONMAX', ncmpm)
        else
            goto 999
        endif
!
! ----- List of input field components
!
        call jedetr(list_cmp)
        call wkvect(list_cmp, 'V V K8', ncmpm, jlicm1)
        do i = 1, ncmpm
            zk8(jlicm1+i-1) = zk8(jlicmp+i-1)
        enddo
!
! ----- List of <NEUT_R> components
!
        call jedetr(list_cmp)
        call jedetr(list_cmp_neut)
        call wkvect(list_cmp, 'V V K8', ncmpm, jlicm1)
        call wkvect(list_cmp_neut, 'V V K8', ncmpm, jlicm2)
        do i = 1, ncmpm
            call codent(i, 'G', ki)
            zk8(jlicm2+i-1)='X'//ki(1:len(ki))
            zk8(jlicm1+i-1)=zk8(jlicmp+i-1)
        enddo
!
! ----- Transform input field in NEUT_R
!
        call chsut1(chamtm, 'NEUT_R', ncmpm, zk8(jlicm1), zk8(jlicm2),&
                    'V', chamtm)
!
! ----- Construction of <CARTE> of <NEUT_R> by selection of components
!
        call calc_norm_coef(modele, nomgd, nb_cmp_max, ncmpm, tynorm,&
                            'SQUA', list_cmp, nb_coef_user, zr(j_coef_user), coefca,&
                            chcalc, nb_cmp_act)
!
! ----- Convert CHAMNO_S/CHAMELEM_S field to CHAMNO/CHAMELEM field
!
        if (tych(1:4) .eq. 'NOEU') then
            call cnscno(chamtm, ' ', 'OUI', 'V', cham2,&
                        'F', iret)
            call detrsd('CHAM_NO_S', chamtm)
        else if (tych(1:4).eq.'ELNO') then
            optio2 ='TOU_INI_ELNO'
            nopar = nopar2(optio2,'NEUT_R','OUT')
            call cescel(chamtm, ligrel, optio2, nopar, 'OUI',&
                        nncp, 'V', cham2, 'F', ibid)
            call detrsd('CHAM_ELEM_S', chamtm)
        else if (tych(1:4).eq.'ELGA') then
            optio2 ='TOU_INI_ELGA'
            nopar = nopar2(optio2,'NEUT_R','OUT')
            call cescel(chamtm, ligrel, optio2, nopar, 'OUI',&
                        nncp, 'V', cham1, 'F', ibid)
            call detrsd('CHAM_ELEM_S', chamtm)
        else
            goto 999
        endif
!
!      -- 4.4 CHANGEMENT DE DISCRETISATION DU CHAMP --
!
        if (tych(1:4) .ne. 'ELGA') then
            optio2 ='NORME_L2'
            nopar='PCHAMPG'
            celmod = '&&PENORM.CELMOD'
            call alchml(ligrel, optio2, nopar, 'V', celmod,&
                        ibid, ' ')
            if (ibid .ne. 0) then
                valk2(1)=ligrel
                valk2(2)=nopar
                valk2(3)=optio2
                call utmess('F', 'UTILITAI3_23', nk=3, valk=valk2)
            endif
            call chpchd(cham2, 'ELGA', celmod, 'OUI', 'V',&
                        cham1)
            call detrsd('CHAMP', celmod)
        endif
!
!      -- 4.5 REDUCTION DU CHAMP EN FONCTION DES MAILLES --
!
        call celces(cham1, 'V', chamtm)
        call jeveuo(mesmai, 'L', jma)
        call cesred(chamtm, nbma, zi(jma), 0, k8b,&
                    'V', chamtm)
        optio2 ='NORME_L2'
        nopar='PCHAMPG'
        call cescel(chamtm, ligrel, optio2, nopar, 'OUI',&
                    nncp, 'V', cham1, 'F', ibid)
        call detrsd('CHAM_ELEM_S', chamtm)
!
! ----- Compute <CARTE> with informations on Gauss points 
!
        call calc_coor_elga(ligrel, chgeom, chgaus)
!
! ----- Compute Norm_L2 * Norm_L2 by element (integration on finite element)
!
        call calc_norm_elem('L2', ligrel, coefca, chgaus, chcalc,&
                            cham1, field_resu)
!
!      -- 4.8 SOMMATION DE LA NORME SUR LES ELEMENTS DESIRES --
!
        call mesomm(field_resu, 1, ibid, vnorm, c16b,&
                    0, ibid)
!
!      -- 4.9 ON REMPLIT LA TABLE --
!
        if (nompar(1) .eq. 'RESULTAT') then
            call wkvect(valk, 'V V K24', 4, jvalk)
            zk24(jvalk) =resuco
            zk24(jvalk+1)=nomcha
            zk24(jvalk+2)=grouma
            zk24(jvalk+3)=infoma
            zk24(jvalk+4)=tynorm
            call wkvect(valr, 'V V R', 2, jvalr)
            zr(jvalr)=inst
            zr(jvalr+1)=sqrt(vnorm(1))
            call wkvect(vali, 'V V I', 1, jvali)
            zi(jvali)=numo
            call tbajli(resu, nbpar, nompar, zi(jvali), zr(jvalr),&
                        c16b, zk24(jvalk), 0)
        else
            call wkvect(valk, 'V V K24', 3, jvalk)
            zk24(jvalk) =nomcha
            zk24(jvalk+1)=grouma
            zk24(jvalk+2)=infoma
            zk24(jvalk+3)=tynorm
            call wkvect(valr, 'V V R', 1, jvalr)
            zr(jvalr)=sqrt(vnorm(1))
            call tbajli(resu, nbpar, nompar, ibid, zr(jvalr),&
                        c16b, zk24(jvalk), 0)
        endif
!
!      -- 4.10 NETTOYAGE POUR L'OCCURRENCE SUIVANTE --
!
        call detrsd('CHAMP', cham1)
        call detrsd('CHAMP', cham2)
!
        call jedetr(valr)
        call jedetr(vali)
        call jedetr(valk)
        call jedetr(mesmai)
!
!     --- FIN DE LA BOUCLE SUR LES NUMEROS D'ORDRE:
!     ---------------------------------------------
 5  continue
!
!     --- FIN DE LA BOUCLE SUR LES OCCURRENCES DU MOT-CLE NORME
!     ---------------------------------------------------------
999  continue
!
    if (nr .ne. 0) then
        call detrsd('RESULTAT', tmpres)
    else
        call detrsd('CHAMP', tmpcha)
    endif
!
    call jedema()
end subroutine
