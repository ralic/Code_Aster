subroutine mecalc(option, modele, chdepl, chgeom, chmate,&
                  chcara, chtemp, chtref, chtime, chnumc,&
                  chharm, chsig, cheps, chfreq, chmass,&
                  chmeta, charge, typcoe, alpha, calpha,&
                  chdynr, suropt, chelem, chelex, ligrel,&
                  base, ch1, ch2, chvari, compor,&
                  chtese, chdese, nopase, typese, chacse,&
                  chstrx, codret)
! ----------------------------------------------------------------------
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
!     - FONCTION REALISEE : APPEL A "CALCUL"
!                           CALCUL DES CONTRAINTES ELEMENTAIRES
!                           CALCUL DES EFFORTS ELEMENTAIRES
!                           CALCUL DES ENERGIES ELEMENTAIRES
! ----------------------------------------------------------------------
! IN  : OPTION : OPTION DE CALCUL
! IN  : MODELE : NOM DU MODELE
! IN  : CH...  : NOM DES CHAMPS ...
! IN  : CHARGE : NOM D'UNE CHARGE
! IN  : TYPCOE : TYPE DU COEFFICIENT MULTIPLICATIF DE LA CHARGE
!                SI TYPCOE = R ALORS ALPHA  EST CE COEFFICIENT
!                SI TYPCOE = C ALORS C'EST CALPHA LE COEFFICIENT
! IN  : BASE   : BASE OU EST CREE LE CHAMELEM
! IN  : COMPOR : NOM DE LA CARTE DE COMPORTEMENT
! IN  : CHTESE : CHAMP DE LA TEMPERATURE SENSIBLE
! IN  : CHDESE : CHAMP DU DEPLACEMENT SENSIBLE
! IN  : NOPASE : NOM DU PARAMETRE SENSIBLE
! IN  : TYPESE : TYPE DE PARAMETRE SENSIBLE
! OUT : CHELEM : CHAMELEM RESULTAT
! OUT : CHELEX : CHAMELEM RESULTAT POUR L'OPTION 'SIEF_SENO_SEGA'
!                PRODUIT LORS DU CALCUL DE L'OPTION 'SIEF_ELNO'
!                POUR X-FEM UNIQUEMENT
! OUT : CODRET : CODE DE RETOUR (0 SI TOUT VA BIEN)
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
! PARAMETRES D'APPELS
#include "asterfort/ajchca.h"
#include "asterfort/cesvar.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedetc.h"
#include "asterfort/jeexin.h"
#include "asterfort/mecact.h"
#include "asterfort/meceuc.h"
#include "asterfort/mechpo.h"
#include "asterfort/utmess.h"
    character(len=*) :: option, modele, chdepl, chdynr, suropt, chelem, compor
    character(len=*) :: chgeom, chmate, chcara(*), chfreq, chmass, chsig, chtemp
    character(len=*) :: chtref, chtime, chnumc, chharm, charge, cheps, chmeta
    character(len=*) :: typcoe, ligrel, base, ch1, ch2, chvari, chacse, chelex
    character(len=*) :: chdese, chtese, nopase, chstrx
    real(kind=8) :: alpha
    complex(kind=8) :: calpha
    integer :: typese, codret
    character(len=6) :: nompro
    parameter (nompro='MECALC')
!
    integer :: maxin, maxout
    parameter (maxin=65)
    parameter (maxout=2)
!
    character(len=1) :: base2
    character(len=8) :: poux, nomode, lpain(maxin), lpaout(maxout), carel
    character(len=8) :: noma
    character(len=16) :: optio2, vari
    character(len=19) :: canbsp, canbva, chxfem(12)
    character(len=24) :: valk
    character(len=24) :: lchin(maxin), lchout(maxout), chdep2, chele2, chc
    character(len=24) :: chnova
    integer :: iaux, ibid, iret1, iret2, nb, nbin, nbout, ifm, niv, iret
    integer :: ifiss
!
    chdep2 = chdepl
    chele2 = chelem
    chnova = '&&MECALC.NOVARI'
    codret = 0
!
    base2 = base
    nomode = modele
    optio2 = option
    call infniv(ifm, niv)
    do iaux = 1, maxin
        lpain(iaux) = ' '
        lchin(iaux) = ' '
    end do
!
!
    nbin = 1
    lchin(1) = chdep2
    nbout = 1
    lchout(1) = chele2
    poux = 'NON'
!
    if (codret .eq. 0) then
!
!     -- ON DONNE LES INFOS NECESSAIRES POUR CREER UN CHAM_ELEM
!        AYANT DES SOUS-POINTS ET/OU DES CMPS DYN. DE VARI_R
!     ---------------------------------------------------------
        carel = chcara(1)
        if (optio2 .eq. 'VARI_ELNO') then
            call exisd('CARTE', compor, iret2)
            if (iret2 .ne. 1) then
                call utmess('A', 'CALCULEL2_86')
                goto 40
            endif
            canbva = '&&'//nompro//'.NBVAR'
!           LA CARTE COMPOR PEUT CHANGER ENTRE DEUX INSTANTS
!           IL FAUT DONC APPELER CESVAR SYSTEMATIQUEMENT
            call cesvar(carel, compor, ligrel, canbva)
            call copisd('CHAM_ELEM_S', 'V', canbva, chele2)
            call detrsd('CHAM_ELEM_S', canbva)
!
        else if ((optio2.eq.'EPSI_ELGA') .or. (optio2.eq.'SIEF_ELGA')) then
            canbsp = '&&'//nompro//'.NBSP'
            call exisd('CHAM_ELEM_S', canbsp, iret1)
            if (iret1 .ne. 1) call cesvar(carel, ' ', ligrel, canbsp)
            call copisd('CHAM_ELEM_S', 'V', canbsp, chele2)
!
        else if ((optio2.eq.'SIEQ_ELGA') .or. (optio2.eq.'EPEQ_ELGA'))&
        then
            canbsp = '&&'//nompro//'.NBSP'
            call exisd('CHAM_ELEM_S', canbsp, iret1)
            if (iret1 .ne. 1) call cesvar(carel, ' ', ligrel, canbsp)
            call copisd('CHAM_ELEM_S', 'V', canbsp, chele2)
!
        endif
!
! ----------------------------------------------------------------------
        if (optio2 .eq. 'VARI_ELNO') then
            lpain(1) = 'PVARIGR'
            lpaout(1) = 'PVARINR'
        else if (optio2.eq.'INDL_ELGA') then
            call ajchca('PMATERC', chmate, lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PCOMPOR', compor, lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PVARIPR', chdepl, lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PCONTPR', chsig, lpain, lchin, nbin,&
                        maxin, 'N')
            lpaout(1) = 'PINDLOC'
        else if (optio2.eq.'VNOR_ELEM_DEPL') then
            lpain(1) = 'PDEPLAC'
            lpaout(1) = 'PVITNOR'
        else if (optio2.eq.'DETE_ELNO') then
            lpain(1) = 'PTEMPER'
            lpaout(1) = 'PDETENO'
            else if (optio2.eq.'ENEL_ELGA' .or. optio2.eq.'ENEL_ELNO')&
        then
            lpain(1) = 'PDEPLAR'
            lpaout(1) = 'PENERDR'
            else if (optio2.eq.'DISS_ELGA' .or. optio2.eq.'DISS_ELNO')&
        then
            lpaout(1) = 'PDISSDR'
            else if (optio2.eq.'SIEQ_ELNO' .or. optio2.eq.'SIEQ_ELGA')&
        then
            lpaout(1) = 'PCONTEQ'
            else if (optio2.eq.'EPEQ_ELNO' .or. optio2.eq.'EPEQ_ELGA'&
        .or. optio2.eq.'EPMQ_ELNO' .or. optio2.eq.'EPMQ_ELGA') then
            lpain(1) = 'PDEFORR'
            lchin(1) = cheps
            lpaout(1) = 'PDEFOEQ'
        else if (optio2.eq.'SIRO_ELEM') then
            lpain(1) = 'PSIG3D'
            lchin(1) = chsig
            lpain(2) = 'PGEOMER'
            lchin(2) = chgeom
            nbin = 2
            lpaout(1) = 'PPJSIGM'
        else
            lpain(1) = 'PDEPLAR'
            if (optio2 .eq. 'SIEF_ELNO') then
                lpain(1) = 'PDEPLPR'
                lpaout(1) = 'PSIEFNOR'
            else if (optio2.eq.'FLHN_ELGA') then
                lpain(2) = 'PCONTR'
                lchin(2) = chsig
                nbin = 2
                lpaout(1) = 'PFLHN'
            else if (optio2.eq.'DEGE_ELNO') then
                lpaout(1) = 'PDEFOGR'
            else if (optio2.eq.'DEGE_ELGA') then
                lpaout(1) = 'PDEFOPG'
                else if (optio2.eq.'EPSI_ELGA' .or. optio2.eq.'EPSG_ELGA'&
            .or. optio2.eq.'EPME_ELGA' .or. optio2.eq.'EPMG_ELGA'&
            .or. optio2.eq.'EPFP_ELGA' .or. optio2.eq.'EPFD_ELGA')&
            then
                lpaout(1) = 'PDEFOPG'
                else if (optio2.eq.'EPSI_ELNO' .or. optio2.eq.'EPSG_ELNO'&
            .or. optio2.eq.'EPME_ELNO' .or. optio2.eq.'EPMG_ELNO'&
            .or. optio2.eq.'EPFP_ELNO' .or. optio2.eq.'EPFD_ELNO')&
            then
                lpaout(1) = 'PDEFONO'
                else if (optio2.eq.'EPSP_ELNO' .or. optio2.eq.'EPSP_ELGA')&
            then
                lpaout(1) = 'PDEFOPL'
                else if (optio2.eq.'EPVC_ELNO' .or. optio2.eq.'EPVC_ELGA')&
            then
                lpaout(1) = 'PDEFOVC'
                else if (optio2.eq.'EPOT_ELEM' .or. optio2.eq.'ETHE_ELEM')&
            then
                lpaout(1) = 'PENERDR'
            else if (optio2.eq.'ECIN_ELEM') then
                lpaout(1) = 'PENERCR'
                call ajchca('POMEGA2', chfreq, lpain, lchin, nbin,&
                            maxin, 'N')
            else if (optio2.eq.'FLUX_ELGA') then
                lpaout(1) = 'PFLUXPG'
            else if (optio2.eq.'FLUX_ELNO') then
                lpaout(1) = 'PFLUXNO'
            else if (optio2.eq.'SOUR_ELGA') then
                lpaout(1) = 'PSOUR_R'
            else if (optio2.eq.'DURT_ELNO') then
                lpain(1) = 'PPHASIN'
                lchin(1) = chmeta
                lpaout(1) = 'PDURT_R'
                else if (optio2.eq.'EPME_ELNO_DPGE' .or.&
     &               optio2.eq.'EPSI_ELNO_DPGE') then
                lpaout(1) = 'PDEFORR'
                else if (optio2.eq.'SIGM_ELNO_DPGE' .or.&
     &               optio2.eq.'SIEF_ELGA_DPGE') then
                lpaout(1) = 'PCONTRR'
            else if (optio2.eq.'VAEX_ELGA') then
                noma=chgeom(1:8)
                call getvtx(' ', 'NOM_VARI', scal=vari, nbret=ibid)
                call mecact('V', chnova, 'MAILLA', noma, 'NEUT_K24',&
                            ncmp=1, nomcmp='Z1', sk=vari)
!
                lpain(1) = 'PVARIGR'
                lpaout(1) = 'PVARIGS'
                lpain(2) = 'PNOVARI'
                lchin(2) = chnova
                nbin =2
            else if (optio2.eq.'VAEX_ELNO') then
                noma=chgeom(1:8)
                call getvtx(' ', 'NOM_VARI', scal=vari, nbret=ibid)
                call mecact('V', chnova, 'MAILLA', noma, 'NEUT_K24',&
                            ncmp=1, nomcmp='Z1', sk=vari)
                lpain(1) = 'PVARINR'
                lpaout(1) = 'PVARINS'
                lpain(2) = 'PNOVARI'
                lchin(2) = chnova
                nbin =2
            else
! ----------------------------------------------------------------------
! ---          RAJOUT DES POUTRES POUX ---
! ----------------------------------------------------------------------
                call dismoi('EXI_POUX', nomode, 'MODELE', repk=poux)
! ----------------------------------------------------------------------
                if (optio2 .eq. 'SIGM_ELNO' .or. optio2 .eq. 'SIPO_ELNO' .or. optio2 .eq.&
                    'SIPM_ELNO' .or. optio2 .eq. 'SIEF_ELGA' .or. optio2 .eq. 'STRX_ELGA') then
                    lpaout(1) = 'PCONTRR'
                    if (poux .eq. 'OUI') then
                        call mechpo('&&MECHPO', charge, modele, chdep2, chdynr,&
                                    suropt, lpain(nbin+1), lchin(nbin+1), nb, typcoe,&
                                    alpha, calpha)
                        nbin = nbin + nb
                        if (optio2 .eq. 'SIPO_ELNO') then
                            lpaout(1) = 'PCONTPO'
                        endif
                    endif
                    if (optio2 .eq. 'STRX_ELGA') then
                        lpaout(1) = 'PSTRXRR'
                    endif
                else if (optio2.eq.'EFGE_ELNO') then
                    lpaout(1) = 'PEFFORR'
                    if (poux .eq. 'OUI') then
                        call mechpo('&&MECHPO', charge, modele, chdep2, chdynr,&
                                    suropt, lpain(nbin+1), lchin(nbin+1), nb, typcoe,&
                                    alpha, calpha)
                        nbin = nbin + nb
                    endif
                else
                    valk = optio2
                    call utmess('F', 'CALCULEL6_10', sk=valk)
                endif
            endif
        endif
        chc = chgeom(1:8)//'.ABSC_CURV'
        call ajchca('PABSCUR', chc, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PNBSP_I', carel//'.CANBSP', lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PFIBRES', carel//'.CAFIBR', lpain, lchin, nbin,&
                    maxin, 'N')
!
!        SI LE MODELE EST X-FEM, AJOUT DES PARAMETRES POUR LE CALCUL DE
!        'SIEF_ELNO' ET SIEF_ELGA (ET 'SIEF_SENO_SEGA')
        call jeexin(modele(1:8)//'.FISS', ifiss)
        if (ifiss .ne. 0) then
            chxfem(1) = modele(1:8)//'.TOPOSE.PIN'
            chxfem(2) = modele(1:8)//'.TOPOSE.CNS'
            chxfem(3) = modele(1:8)//'.TOPOSE.HEA'
            chxfem(4) = modele(1:8)//'.TOPOSE.LON'
            chxfem(5) = modele(1:8)//'.TOPOSE.PMI'
            chxfem(6) = modele(1:8)//'.BASLOC'
            chxfem(7) = modele(1:8)//'.LNNO'
            chxfem(8) = modele(1:8)//'.LTNO'
            chxfem(9) = modele(1:8)//'.STNO'
            chxfem(10) = modele(1:8)//'.FISSNO'
            chxfem(11) = chelex
            chxfem(12) = modele(1:8)//'.TOPONO.HNO'
            call ajchca('PPINTTO', chxfem(1), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PCNSETO', chxfem(2), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PHEAVTO', chxfem(3), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PLONCHA', chxfem(4), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PPMILTO', chxfem(5), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PBASLOR', chxfem(6), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PLSN', chxfem(7), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PLST', chxfem(8), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PSTANO', chxfem(9), lpain, lchin, nbin,&
                        maxin, 'N')
            call ajchca('PFISNO', chxfem(10), lpain, lchin, nbin,&
                        maxin, 'N')
            if (option .eq. 'SIEF_ELNO') then
                call ajchca('PSIEFSER', chxfem(11), lpaout, lchout, nbout,&
                            maxout, 'N')
            endif
            call ajchca('PHEA_NO', chxfem(12), lpain, lchin, nbin,&
                        maxin, 'N')
        endif
!
        call ajchca('PCAARPO', chcara(9), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCACOQU', chcara(7), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCINFDI', chcara(15), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCADISK', chcara(2), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCADISM', chcara(3), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCAGEPO', chcara(5), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCAGNBA', chcara(11), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCAGNPO', chcara(6), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCAMASS', chcara(12), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCAORIE', chcara(1), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCAORIR', chtemp, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCASECT', chcara(8), lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCOMPOR', compor, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCONCOR', chfreq, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PCONTRR', chsig, lpain, lchin, nbin,&
                    maxin, 'N')
!         ON RECUPERE LE NUME_ORDRE DANS LE NOM DU CHAMP
!         DE DEPLACEMENT : 'NOMUTILI.C00.000000'
        chc = '&&MEGENE.DEF.'//lchin(1) (14:19)
        call ajchca('PDEFORM', chc, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PDEFORR', cheps, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PDEPPLU', chdep2, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PGEOMER', chgeom, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PHARMON', chharm, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PMASDIA', chmass, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PMATERC', chmate, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PPHASIN', chmeta, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PSIEFNOR', chsig, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PVARCRR', ch2, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PVARCPR', ch1, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PVARIGR', chvari, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PVARIGS', chvari, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PSTRXRR', chstrx, lpain, lchin, nbin,&
                    maxin, 'N')
!
!
        if (chtemp .ne. ' ') then
            call ajchca('PTEMPER', chtemp, lpain, lchin, nbin,&
                        maxin, 'N')
        endif
        call ajchca('PTEMPSR', chtime, lpain, lchin, nbin,&
                    maxin, 'N')
        call ajchca('PTEREF', chtref, lpain, lchin, nbin,&
                    maxin, 'N')
!
        call meceuc('C', poux, optio2, carel, ligrel,&
                    nbin, lchin, lpain, nbout, lchout,&
                    lpaout, base2)
        call exisd('CHAMP_GD', lchout(1), iret)
        if (iret .eq. 0) then
            codret = 1
            call utmess('A', 'CALCULEL2_89', sk=optio2)
        endif
!
!       pour 'sief_elno' cas x-fem, on verifie la presence du champ
!       supplementaire "de contraintes aux noeuds par sous elements"
        if (ifiss .ne. 0 .and. option .eq. 'SIEF_ELNO') then
            call exisd('CHAMP_GD', lchout(2), iret)
            if (iret .eq. 0) then
                codret = 1
                call utmess('A', 'CALCULEL6_79', sk=optio2)
            endif
        endif
!
!     MENAGE :
!     -------
        if (poux .eq. 'OUI') call jedetc('V', '&&MECHPO', 1)
        call detrsd('CHAM_ELEM_S', chele2)
!
    endif
 40 continue
!
end subroutine
