subroutine op0019()
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
! ----------------------------------------------------------------------
!
!                O P E R A T E U R    AFFE_CARA_ELEM
!
! ----------------------------------------------------------------------
!
    implicit none
!
! aslint: disable=W1502
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/aceaba.h"
#include "asterfort/aceaca.h"
#include "asterfort/aceaco.h"
#include "asterfort/aceadi.h"
#include "asterfort/aceagb.h"
#include "asterfort/aceama.h"
#include "asterfort/aceamb.h"
#include "asterfort/aceamr.h"
#include "asterfort/aceaor.h"
#include "asterfort/aceapc.h"
#include "asterfort/aceapf.h"
#include "asterfort/aceapo.h"
#include "asterfort/acearm.h"
#include "asterfort/acearp.h"
#include "asterfort/acecel.h"
#include "asterfort/aceinc.h"
#include "asterfort/acevba.h"
#include "asterfort/acevca.h"
#include "asterfort/acevco.h"
#include "asterfort/acevdi.h"
#include "asterfort/acevgb.h"
#include "asterfort/acevma.h"
#include "asterfort/acevmb.h"
#include "asterfort/acevmr.h"
#include "asterfort/acevor.h"
#include "asterfort/acevpc.h"
#include "asterfort/acevpf.h"
#include "asterfort/acevpo.h"
#include "asterfort/acevrm.h"
#include "asterfort/acevrp.h"
#include "asterfort/alcart.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/coqucf.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pmfd00.h"
#include "asterfort/tecart.h"
#include "asterfort/utmess.h"
#include "asterfort/verima.h"
#include "asterfort/wkvect.h"
#include "asterfort/verif_affe.h"
#include "asterfort/detrsd_vide.h"
! --------------------------------------------------------------------------------------------------
!   NBEPO     13  : NOMBRE D'ELEMENTS DE TYPE "POUTRE"
!   NBEDI      8  : NOMBRE D'ELEMENTS DE TYPE "DISCRET"
!   NBECO     29  : NOMBRE D'ELEMENTS DE TYPE "COQUE"
!   NBECA      2  : NOMBRE D'ELEMENTS DE TYPE "CABLE"
!   NBEBA      3  : NOMBRE D'ELEMENTS DE TYPE "BARRE"
!   NBEMA     50  : NOMBRE D'ELEMENTS DE TYPE "MASSIF"
!   NBEGB      6  : NOMBRE D'ELEMENTS DE TYPE "GRILLE"
!   NBEMB      4  : NOMBRE D'ELEMENTS DE TYPE "MEMBRANE"
!   NBTHM1    47  : NOMBRE D'ELEMENTS DE TYPE "MASSIF" THM
!   NBTHM2    28  : NOMBRE D'ELEMENTS DE TYPE "MASSIF" *HH2*
    integer :: nbepo, nbedi, nbeco, nbeca, nbeba, nbema, nbegb, nbemb
    integer :: nbtel, nbmcf, nbel1, nbel2, nbel3, nbel4, nbel5
    integer :: nbthm1, nbthm2
    parameter  (      nbepo=13,nbedi=8,nbeco=29,nbeca=2,nbeba=3)
    parameter  (nbel1=nbepo   +nbedi  +nbeco   +nbeca  +nbeba)
    parameter  (      nbema=50,nbegb=6,nbemb=4,nbthm1=47,nbthm2=28)
    parameter  (nbel2=nbema)
    parameter  (nbel3=nbegb+nbemb)
    parameter  (nbel4=nbthm1)
    parameter  (nbel5=nbthm2)
    parameter  (nbtel=nbel1+nbel2+nbel3+nbel4+nbel5)
!   NBMCF  : NOMBRE DE MOTS CLES FACTEUR DE L'OPERATEUR
    parameter  (nbmcf=15)
! --------------------------------------------------------------------------------------------------
    integer :: nbmcle(nbmcf), nbocc(nbmcf), ivr(3), nbcart, iret, i, jadr
    integer :: ntyele(nbtel), nbver, nlm, nlg, lxc, lxo, nln, nlj, lxa
    integer :: lxk, lxb, lxm, lxpf, lxgb, lxmb, lmax, ifm, niv, lxp, nbvm
    integer :: lxd, nboccd, lxrp, noemaf, lxrm, noemf2, nbmail, nbmtrd
    integer :: lxmr, noemf3
    integer :: npoutr, ndiscr, ncoque, ncable, nbarre, nmassi, ngrill
    integer :: ngribt, nmembr, iclf, ioc, icle, ng
    integer :: depart, jdnm, ixnw, jdln, jdlm, jdls
    aster_logical :: locaco, locagb, locamb
    character(len=6) :: kioc
    character(len=8) :: ver(3), nomu, nomo, noma, lpain(3), lpaout(1)
    character(len=16) :: concep, cmd, mclf(nbmcf), mcle(4), k16bid
    character(len=16) :: nomele(nbtel), nomel1(nbel1), nomel2(nbel2)
    character(len=16) :: nomel3(nbel3), nomel4(nbel4), nomel5(nbel5)
    character(len=19) :: cartcf, ligrmo, lchin(3), lchout(1)
    character(len=24) :: mlgnma, modnom, modnem, tmplst, tmplma, tmplno, tmpncf
    data mcle   /  'GROUP_MA        ','MAILLE          ',&
                   'GROUP_NO        ','NOEUD           '/
!
    data mclf   /  'POUTRE          ','COQUE           ',&
                   'DISCRET         ','ORIENTATION     ',&
                   'DEFI_ARC        ','CABLE           ',&
                   'BARRE           ','MASSIF          ',&
                   'POUTRE_FLUI     ','RIGI_PARASOL    ',&
                   'GRILLE          ','RIGI_MISS_3D    ',&
                   'DISCRET_2D      ','MEMBRANE        ',&
                   'MASS_AJOU       '/
!   NOMEL1 : POUTRE(13) DISCRET(8) COQUE(29) CABLE(2) BARRE(3)
    data nomel1 /   'MECA_POU_D_T    ','MECA_POU_D_E    ','MECA_POU_D_T_GD ',&
                    'MECA_POU_C_T    ','MEFS_POU_D_T    ','MECA_POU_D_TG   ',&
                    'MECA_POHO_HEXA8 ','MECA_POHO_HEXA20','MET3SEG3        ',&
                    'MET6SEG3        ','MET3SEG4        ','MECA_POU_D_EM   ',&
                    'MECA_POU_D_TGM  ','MECA_DIS_T_N    ','MECA_DIS_T_L    ',&
                    'MECA_DIS_TR_N   ','MECA_DIS_TR_L   ','MECA_2D_DIS_T_N ',&
                    'MECA_2D_DIS_T_L ','MECA_2D_DIS_TR_N','MECA_2D_DIS_TR_L',&
                    'THCOTR3         ','THCOTR6         ','THCOQU4         ',&
                    'THCOQU8         ','THCOTR7         ','THCOQU9         ',&
                    'MEDKTR3         ','MEDSTR3         ','MET3TR3         ',&
                    'MEDKQU4         ','MEDSQU4         ','MEQ4QU4         ',&
                    'MECXSE3         ','MEDKTG3         ','MEDKQG4         ',&
                    'MEQ4GG4         ','MET3GG3         ','METCSE3         ',&
                    'METDSE3         ','THCASE3         ','THCPSE3         ',&
                    'MEC3QU9H        ','MEC3TR7H        ','MEBODKT         ',&
                    'MEBODST         ','MEBOQ4G         ','MEBOCQ3         ',&
                    'THCOSE3         ','THCOSE2         ','MECABL2         ',&
                    'MEPOULI         ','MECA_BARRE      ','MECA_2D_BARRE   ',&
                    'MECGSEG3'/
!   NOMEL2 : MASSIF(50)
    data nomel2 /   'MECA_HEXA8      ','MECA_PENTA6     ','MECA_PENTA18    ',&
                    'MECA_TETRA4     ','MECA_HEXA27     ','MECA_HEXA20     ',&
                    'MECA_PENTA15    ','MECA_TETRA10    ','MECA_TETRS10    ',&
                    'MECA_PYRAM5     ','MECA_PYRAM13    ','MECA_HEXS8      ',&
                    'MECA_HEXS20     ','MEAXTR3         ','MEAXQU4         ',&
                    'MEAXTR6         ','MEAXQU8         ','MEAXQU9         ',&
                    'MEDPTR3         ','MEDPQU4         ','MEDPTR6         ',&
                    'MEDPQU8         ','MEDPQU9         ','MECPTR3         ',&
                    'MECPQU4         ','MECPTR6         ','MECPQU8         ',&
                    'MECPQU9         ','THER_HEXA8      ','THER_PENTA6     ',&
                    'THER_TETRA4     ','THER_PYRAM5     ','THER_HEXA27     ',&
                    'THER_HEXA20     ','THER_PENTA15    ','THER_TETRA10    ',&
                    'THER_PYRAM13    ','THAXTR3         ','THAXQU4         ',&
                    'THAXTR6         ','THAXQU8         ','THAXQU9         ',&
                    'THPLTR3         ','THPLQU4         ','THPLTR6         ',&
                    'THPLQU8         ','THPLQU9         ','MET3SEG3        ',&
                    'MET6SEG3        ','MET3SEG4        '/
!   NOMEL3 : GRILLE(6) MEMBRANE(4)
    data nomel3 /   'MEGCQU4         ','MEGMTR3         ','MEGMQU4         ',&
                    'MEGMTR6         ','MEGMQU8         ','MEGCTR3         ',&
                    'MEMBTR3         ','MEMBTR6         ','MEMBQU4         ',&
                    'MEMBQU8         '/
!   NOMEL4 : MASSIF(47)
    data nomel4 /   'HM_DPQ8S        ','HM_AXIS_QU8S    ','HM_DPTR6S       ',&
                    'HM_AXIS_TR6S    ','HM_HEXA20S      ','HM_PENTA15S     ',&
                    'HM_TETRA10S     ','THM_DPQ8S       ','THM_AXIS_QU8S   ',&
                    'THM_DPTR6S      ','THM_AXIS_TR6S   ','THM_HEXA20S     ',&
                    'THM_PENTA15S    ','THM_TETRA10S    ','H_DPQ8S         ',&
                    'H_DPTR6S        ','H_HEXA20S       ','H_PENTA15S      ',&
                    'H_TETRA10S      ','THHM_DPQ8S      ','THHM_AXIS_QU8S  ',&
                    'THHM_DPTR6S     ','THHM_AXIS_TR6S  ','THHM_HEXA20S    ',&
                    'THHM_PENTA15S   ','THHM_TETRA10S   ','HHM_DPQ8S       ',&
                    'HHM_AXIS_QU8S   ','HHM_DPTR6S      ','HHM_AXIS_TR6S   ',&
                    'HHM_HEXA20S     ','HHM_PENTA15S    ','HHM_TETRA10S    ',&
                    'THH_DPQ8S       ','THH_AXIS_QU8S   ','THH_DPTR6S      ',&
                    'THH_AXIS_TR6S   ','THH_HEXA20S     ','THH_PENTA15S    ',&
                    'THH_TETRA10S    ','HH_DPQ8S        ','HH_AXIS_QU8S    ',&
                    'HH_DPTR6S       ','HH_AXIS_TR6S    ','HH_HEXA20S      ',&
                    'HH_PENTA15S     ','HH_TETRA10S     '/
!   NOMEL5 : MASSIF(28)
    data nomel5 /   'THH2M_DPQ8S     ','THH2M_AXIS_QU8S ','THH2M_DPTR6S    ',&
                    'THH2M_AXIS_TR6S ','THH2M_HEXA20S   ','THH2M_PENTA15S  ',&
                    'THH2M_TETRA10S  ','HH2M_DPQ8S      ','HH2M_AXIS_QU8S  ',&
                    'HH2M_DPTR6S     ','HH2M_AXIS_TR6S  ','HH2M_HEXA20S    ',&
                    'HH2M_PENTA15S   ','HH2M_TETRA10S   ','THH2_DPQ8S      ',&
                    'THH2_AXIS_QU8S  ','THH2_DPTR6S     ','THH2_AXIS_TR6S  ',&
                    'THH2_HEXA20S    ','THH2_PENTA15S   ','THH2_TETRA10S   ',&
                    'HH2_DPQ8S       ','HH2_AXIS_QU8S   ','HH2_DPTR6S      ',&
                    'HH2_AXIS_TR6S   ','HH2_HEXA20S     ','HH2_PENTA15S    ',&
                    'HH2_TETRA10S    '/
!
    data nbmcle / 2,2,4,4,2,2,2,2,2,1,2,0,4,2,1/
! --------------------------------------------------------------------------------------------------
    call jemarq()
!   CALL ONERRF('ABORT', K16BID, IRET)
    iret=0
! --------------------------------------------------------------------------------------------------
!   INITIALISATION DE  NOMELE
    do i = 1, nbel1
        nomele(i) = nomel1(i)
    enddo
    do i = 1, nbel2
        nomele(i+nbel1) = nomel2(i)
    enddo
    do i = 1, nbel3
        nomele(i+nbel1+nbel2) = nomel3(i)
    enddo
!
    do i = 1, nbel4
        nomele(i+nbel1+nbel2+nbel3) = nomel4(i)
    enddo
!
    do i = 1, nbel5
        nomele(i+nbel1+nbel2+nbel3+nbel4) = nomel5(i)
    enddo
! --------------------------------------------------------------------------------------------------
!   RECUPERATION DES ARGUMENTS  DE LA COMMANDE
    call getres(nomu, concep, cmd)
! --------------------------------------------------------------------------------------------------
!   VERIFICATIONS SUPPLEMENTAIRES DE SYNTAXE
    call getvtx(' ', 'VERIF', nbval=2, vect=ver, nbret=nbver)
    do i = 1, 3
        ivr(i) = 0
    enddo
    if (nbver .gt. 0) then
        do i = 1, nbver
            if (ver(i) .eq. 'MAILLE  ') ivr(1) = 1
            if (ver(i) .eq. 'NOEUD   ') ivr(2) = 1
        enddo
    else if (nbver.lt.0) then
        call utmess('F', 'MODELISA5_55')
    endif
!
    do i = 1, nbmcf
        call getfac(mclf(i), nbocc(i))
    enddo
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS POUTRE
    lxp = 0
    if (nbocc(1) .ne. 0) then
        call acevpo(nbocc(1), nlm, nlg, iret)
        lxp = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS COQUE
    lxc = 0
    if (nbocc(2) .ne. 0) then
        call acevco(nbocc(2), nlm, nlg, iret)
        lxc = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ORIENTATIONS DES ELEMENTS
    lxo = 0
    if (nbocc(4) .ne. 0) then
        call acevor(nbocc(4), nlm, nlg, nln, nlj, iret)
        lxo = max(nlm,nln,nlj,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES POUTRES COURBES
    lxa = 0
    if (nbocc(5) .ne. 0) then
        call acevpc(nbocc(5), nlm, nlg, iret)
        lxa = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS CABLE
    lxk = 0
    if (nbocc(6) .ne. 0) then
        call acevca(nbocc(6), nlm, nlg, iret)
        lxk = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS BARRE
    lxb = 0
    if (nbocc(7) .ne. 0) then
        call acevba(nbocc(7), nlm, nlg, iret)
        lxb = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS MASSIF :
    lxm = 0
    if (nbocc(8) .ne. 0) then
        call acevma(nbocc(8), nlm, nlg)
        lxm = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS POUTRE_FLUI
    lxpf = 0
    if (nbocc(9) .ne. 0) then
        if (nbocc(1) .eq. 0) then
            call utmess('F', 'MODELISA5_56')
        endif
        call acevpf(nbocc(9), nlm, nlg)
        lxpf = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS "GRILLE"
    lxgb = 0
    if (nbocc(11) .ne. 0) then
        call acevgb(nbocc(11), nlm, nlg)
        lxgb = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS "MEMBRANE"
    lxmb = 0
    if (nbocc(14) .ne. 0) then
        call acevmb(nbocc(14), nlm, nlg)
        lxmb = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   LONGUEUR MAXIMUM D UNE LISTE DE MAILLE/NOEUD/GROUP_MA/GROUP_NO
    lmax = max(1,lxp,lxc,lxo,lxa,lxk,lxb,lxm,lxpf,lxgb,lxmb)
! --------------------------------------------------------------------------------------------------
!   RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
    if (niv .eq. 2) ivr(3) = 1
! --------------------------------------------------------------------------------------------------
!   MODELE
    call getvid(' ', 'MODELE', scal=nomo, nbret=nbvm)
!   Enregistre le nom du modèle dans la SD de AFFE_CARA_ELEM
    call wkvect(nomu//'.MODELE', 'G V K8', 1, jadr)
    zk8(jadr) = nomo
!   reconstruction des noms jeveux du concept modele
    modnom = nomo//'.MODELE    .LGRF'
    modnem = nomo//'.MODELE    .NEMA'
!   récupération du nom du maillage associé
    call jeveuo(modnom, 'L', jdnm)
    noma = zk8(jdnm)
! --------------------------------------------------------------------------------------------------
!   RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ASSOCIE
    mlgnma = noma//'.NOMMAI'
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS DISCRET
    lxd = 0
    if (nbocc(3) .ne. 0 .or. nbocc(13) .ne. 0) then
        nboccd = nbocc(3) + nbocc(13)
        if (nbocc(3) .ne. 0) k16bid = mclf(3)
        if (nbocc(13) .ne. 0) k16bid = mclf(13)
        call acevdi(nboccd, noma, nomo, k16bid, nlm,&
                    nlg, nln, nlj, iret)
        lxd = max(nlm,nln,nlg,nlj)
        lmax = max(lmax,lxd)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA DIMENSION DES RAIDEURS REPARTIES
    lxrp = 0
    if (nbocc(10) .ne. 0) then
        call acevrp(nbocc(10), noma, lxrp, noemaf)
        lmax = max(lmax,lxrp)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA DIMENSION DES RAIDEURS MISS
    lxrm = 0
    if (nbocc(12) .ne. 0) then
        call acevrm(nbocc(12), noma, lxrm, noemf2)
        lmax = max(lmax,lxrm)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA DIMENSION DES MASSES REPARTIES
    lxmr = 0
    if (nbocc(15) .ne. 0) then
        call acevmr(nbocc(15), noma, lxmr, noemf3)
        lmax = max(lmax,lxmr)
    endif
! --------------------------------------------------------------------------------------------------
!   RECUPERATION DU NB DE MAILLES INITIALES (MAILLAGE)
    call jelira(mlgnma, 'NOMMAX', nbmail)
! --------------------------------------------------------------------------------------------------
!   RECUPERATION DU NB DE MAILLES TARDIVES  (MODELE)
    tmplst = nomu//'.LISTE'
    tmplma = nomu//'.AFFEMAI'
    tmplno = nomu//'.AFFENOE'
    nbmtrd = 0
    call jeexin(modnem, ixnw)
    if (ixnw .ne. 0) then
        call jelira(modnem, 'NMAXOC', nbmtrd)
        call wkvect(tmplno, 'V V I', nbmtrd, jdln)
    endif
    call wkvect(tmplma, 'V V I', nbmail, jdlm)
    call wkvect(tmplst, 'V V K24', lmax, jdls)
! --------------------------------------------------------------------------------------------------
!   RECUPERATION DES NUMEROS DES TYPES ELEMENTS
    do i = 1, nbtel
        call jenonu(jexnom('&CATA.TE.NOMTE', nomele(i)), ntyele(i))
    enddo
! --------------------------------------------------------------------------------------------------
!   COMPTEUR D'ELEMENTS ET VERIFICATION COHERENCE DES AFFECTATIONS
    call acecel(noma, nomo, nbocc, nbepo, nbedi,&
                nbeco, nbeca, nbeba, nbema, nbegb,&
                nbemb, nbthm1, nbthm2, ntyele, npoutr,&
                ndiscr, ncoque, ncable, nbarre, nmassi,&
                ngrill, ngribt, nmembr, jdlm, jdln, iret)
    if (iret .ne. 0) then
        call utmess('F', 'MODELISA5_57')
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE L'EXISTENCE DES MAILLES/NOEUDS/GROUPES DECLARES
    do iclf = 1, nbmcf
        do ioc = 1, nbocc(iclf)
            call codent(ioc, 'G', kioc)
            do icle = 1, nbmcle(iclf)
                call getvtx(mclf(iclf), mcle(icle), iocc=ioc, nbval=lmax, vect=zk24(jdls),&
                            nbret=ng)
                call verima(noma, zk24(jdls), ng, mcle(icle))
            enddo
        enddo
    enddo
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA BONNE  AFFECTATION  DES  CARACTERISTIQUES
!     POUR TOUTES LES MAILLES ET NOEUDS AFFECTES , IMPR SI DEMANDE
!     INCREMENTATION DES COMPTEURS D APPELS A NOCART (DISCRET,COQUE,
!     DEFI_ARC,CABLE,POUTRE,BARRE)
    iret=0
    call aceinc(noma, nomo, nbmcf, mclf, ntyele,&
                nbocc, ivr, nbepo, nbedi, nbeco,&
                nbeca, nbeba, nbema, nbegb, nbemb,&
                nbtel, locaco, locagb, locamb, jdlm,&
                jdln, lmax, iret)
    if (iret .ne. 0) then
        call utmess('F', 'MODELISA5_59')
    endif
!     FABRICATION DE LA CARTE COMMUNE A TOUS LES ELEMENTS LINEIQUE
!     S'IL Y EN A D'AFFECTE
    nbcart = 0
    if (nbocc(1) + nbocc(7) + nbocc(6) .ne. 0) then
        nbcart = npoutr + nbarre + ncable
        if (nbcart .gt. 0) then
            cartcf = nomu//'.CVENTCXF'
            call alcart('G', cartcf, noma, 'VENTCX_F')
        endif
    endif
    if ((nbocc(1).eq.0) .and. (npoutr.ne.0)) then
        call utmess('A', 'MODELISA5_60')
    endif
    if ((nbocc(7).eq.0) .and. (nbarre.ne.0)) then
        call utmess('A', 'MODELISA5_61')
    endif
    if ((nbocc(6).eq.0) .and. (ncable.ne.0)) then
        call utmess('A', 'MODELISA5_62')
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES ORIENTATIONS AUX ELEMENTS POUTRES ET DISCRETS  ET
!     BARRES ET AFFECTATION DE LA CARTE ORIENTATION
    if (nbocc(1).ne.0 .or. nbocc(3).ne.0 .or. nbocc(13).ne.0 .or. nbocc(7).ne.0 .or. &
        nbocc(10).ne.0) then
        call aceaor(noma, nomo, lmax, nbepo, nbedi,&
                    nbel1+nbel2+nbel3, ntyele, nomele, ivr, ifm, nbocc)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES CARACTERISTIQUES AUX ELEMENTS POUTRES
    if (nbocc(1) .ne. 0) then
!        NBEPO + NBEDI + NBECO + NBECA + NBEBA + NBEMA + NBEGB
        depart = 1
        call aceapo(noma, nomo, lmax, npoutr, nbocc(1),&
                    mclf(1), nbepo, ntyele(depart), ivr, ifm, jdlm)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES EPAISSEURS/COURBURES/ANGLES AUX ELEMENTS COQUES
    if (nbocc(2) .ne. 0) then
        call aceaco(nomu, noma, lmax, locagb, locamb, nbocc(2))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES MATRICES AUX ELEMENTS DISCRETS
    if (nbocc(3) .ne. 0 .or. nbocc(13) .ne. 0) then
        nboccd = nbocc(3) + nbocc(13)
        if (nbocc(3) .ne. 0) k16bid = mclf(3)
        if (nbocc(13) .ne. 0) k16bid = mclf(13)
        call aceadi(noma, nomo, k16bid, lmax, nboccd, ivr, ifm)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES COURBURES AUX ELEMENTS POUTRES COURBES
    if (nbocc(5) .ne. 0) then
        call aceapc(nomu, noma, lmax, nbocc(5))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES SECTIONS AUX ELEMENTS CABLE :
    if (nbocc(6) .ne. 0) then
        call aceaca(nomu, noma, lmax, nbocc(6))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES CARACTERISTIQUES AUX ELEMENTS BARRE
    if (nbocc(7) .ne. 0) then
!        NBEPO + NBEDI + NBECO + NBECA + NBEBA + NBEMA + NBEGB
        depart = nbepo + nbedi + nbeco + nbeca + 1
        call aceaba(noma, nomo, lmax, nbarre, nbocc(7),&
                    mclf(7), nbeba, ntyele(depart), ivr, ifm, jdlm)
    endif

! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES REPERES AUX ELEMENTS THERMIQUES ET MECANIQUES
    if (nbocc(8) .ne. 0) then
        call aceama(nomu, noma, lmax, nbocc(8))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES REPERES AUX ELEMENTS POUTRE_FLUI
    if (nbocc(9) .ne. 0) then
        call aceapf(nomu, noma, lmax, nbocc(9))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES MATRICES AUX RAIDEURS REPARTIES
    if (nbocc(10) .ne. 0) then
        call acearp(noma, nomo, lmax, noemaf, nbocc(10), ivr, ifm)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT "GRILLE"
    if (nbocc(11) .ne. 0) then
        call aceagb(nomu, noma, lmax, locamb, nbocc(11))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES MATRICES AUX RAIDEURS MISS
    if (nbocc(12) .ne. 0) then
        call acearm(noma, nomo, lmax, noemf2, nbocc(12), ivr, ifm)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT "MEMBRANE"
    if (nbocc(14) .ne. 0) then
        call aceamb(nomu, noma, lmax, nbocc(14))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES MATRICES AUX MASSES REPARTIES
    if (nbocc(15) .ne. 0) then
        call aceamr(noma, nomo, lmax, noemf3, nbocc(15), ivr, ifm)
    endif
! --------------------------------------------------------------------------------------------------
!   COMPACTAGE DE LA CARTE : '.CVENTCXF'
    if (nbcart .gt. 0) then
!        PAS APPELE POUR UNE SURCHARGE "FINE" MAIS POUR LE COMPACTAGE
        call tecart(cartcf)
!        DESTRUCTION DES CHAMPS
        tmpncf = cartcf//'.NCMP'
        call jedetr(tmpncf)
        tmpncf = cartcf//'.VALV'
        call jedetr(tmpncf)
    endif
!
!     POUR LES COQUES, GRILLES IL PEUT EXISTER UNE CARTE FONCTION
!     IL FAUT L'EVALUER ET METTRE LE RESULTAT DANS LA CARTE DES REELS
    if ((nbocc(2).ne.0) .or. (nbocc(11).ne.0)) then
        call coqucf(nomu)
    endif
!
! --------------------------------------------------------------------------------------------------
!   TRAITEMENT DES MOTS CLES
!           MULTIFIBRE  /  GEOM_FIBRE
!           COQUE       /  COQUE_NCOU
!           GRILLE      /  COQUE_NCOU
!           MEMBRANE    /  COQUE_NCOU
!           POUTRE      /  TUYAU_NCOU
!           POUTRE      /  TUYAU_NSEC
    call pmfd00()
! --------------------------------------------------------------------------------------------------
!   APPEL DE L'OPTION DE VERIFICATION VERI_CARA_ELEM :
    lpain(1)='PCACOQU'
    lchin(1)=nomu//'.CARCOQUE'
    lpaout(1)='PBIDON'
    lchout(1)='&&OP0019.BIDON'
    ligrmo=nomo//'.MODELE'
    call calcul('C', 'VERI_CARA_ELEM', ligrmo, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V', 'OUI')
! --------------------------------------------------------------------------------------------------
!   Certaines cartes peuvent etre vides : il faut les detruire :
    call detrsd_vide('CARTE',nomu//'.CARDISCA')


! - Audit assignments :
    call verif_affe(modele=nomo,sd=nomu)


    call jedema()
end subroutine
