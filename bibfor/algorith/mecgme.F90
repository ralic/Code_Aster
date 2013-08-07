subroutine mecgme(modelz, carelz, mate, lischa, instap,&
                  depmoi, depdel, instam, compor, carcri,&
                  mesuiv)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxliis.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/u2mess.h"
    character(len=*) :: modelz, carelz
    character(len=*) :: mate
    real(kind=8) :: instap, instam
    character(len=24) :: compor, carcri
    character(len=19) :: lischa
    character(len=19) :: mesuiv, depdel, depmoi
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES MATRICES ELEMENTAIRES DES CHARGEMENTS MECANIQUES
! DEPENDANT DE LA GEOMETRIE (SUIVEURS)
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE  : NOM DU MODELE
! IN  LISCHA  : SD L_CHARGES
! IN  CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE    : CHAMP DE MATERIAU
! IN  INSTAP  : INSTANT DU CALCUL
! IN  DEPMOI  : DEPLACEMENT A L'INSTANT MOINS
! IN  DEPDEL  : INCREMENT DE DEPLACEMENT AU COURS DES ITERATIONS
! IN  INSTAM  : INSTANT MOINS
! IN  COMPOR  : COMPORTEMENT
! IN  CARCRI  : CRITERES DE CONVERGENCE (THETA)
! OUT MESUIV  : MATRICES ELEMENTAIRES
!               POSITION 7-8  : NUMERO DE LA CHARGE
!                               VAUT 00 SI PAS DE CHARGE
!               POSITION 12-14: NUMERO DU VECTEUR ELEMENTAIRE / CHARGE
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=16)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=24) :: modele, carele
    character(len=24) :: charge, infcha
    character(len=8) :: nomcha, k8bid
    character(len=8) :: affcha
    character(len=16) :: option
    character(len=24) :: chtim2
    character(len=24) :: chgeom, chcara(18), chtime, ligrel
    character(len=24) :: ligrmo, ligrch, evolch
    integer :: ibid, iret, ierd, ier, i, k, icha, inum
    integer :: somme
    logical :: lbid, prem
    integer :: jchar, jinf, jlme
    integer :: nchar, numchm, nbchme
    complex(kind=8) :: c16bid
    integer :: ifm, niv
!
    integer :: nbchmx
    parameter (nbchmx=5)
    integer :: nbopt(nbchmx), tab(nbchmx)
    character(len=6) :: nomlig(nbchmx), nompaf(nbchmx), nomopf(nbchmx)
    character(len=6) :: nompar(nbchmx), nomopr(nbchmx)
    data nomlig/'.ROTAT','.PESAN','.PRESS','.FCO3D','.EFOND'/
    data nomopf/'??????','??????','PRSU_F','SFCO3D','EFON_F'/
    data nompaf/'??????','??????','PRESSF','FFCO3D','PEFOND'/
    data nomopr/'RO    ','??????','PRSU_R','SRCO3D','EFON_R'/
    data nompar/'ROTATR','PESANR','PRESSR','FRCO3D','PEFOND'/
    data nbopt/10,15,9,15,16/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    modele = modelz
    carele = carelz
    ligrmo = modele(1:8)//'.MODELE'
    charge = lischa(1:19)//'.LCHA'
    infcha = lischa(1:19)//'.INFC'
    chtime = '&&MECHME.CH_INST_R'
    chtim2 = '&&MECHME.CH_INST_M'
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- ACCES AUX CHARGEMENTS
!
    call jeexin(charge, iret)
    if (iret .eq. 0) then
        nchar = 0
        goto 99
    else
        call jelira(charge, 'LONMAX', nchar)
        call jeveuo(charge, 'L', jchar)
        call jeveuo(infcha, 'L', jinf)
    endif
!
! --- PREPARATION DES MATR_ELEM
!
    call jeexin(mesuiv//'.RELR', iret)
    if (iret .eq. 0) then
        call memare('V', mesuiv, modele(1:8), mate, carele,&
                    'CHAR_MECA')
        call reajre(mesuiv, ' ', 'V')
        prem = .true.
    else
        prem = .false.
        call jelira(mesuiv//'.RELR', 'LONUTI', nbchme)
        if (nbchme .gt. 0) call jeveuo(mesuiv//'.RELR', 'L', jlme)
    endif
!
! --- CHAMP DE GEOMETRIE
!
    call megeom(modele(1:8), chgeom)
!
! --- CHAMP DE CARACTERISTIQUES ELEMENTAIRES
!
    call mecara(carele(1:8), lbid, chcara)
!
! --- CHAMP POUR LES INSTANTS
!
    call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R  ',&
                1, 'INST   ', ibid, instap, c16bid,&
                k8bid)
    call mecact('V', chtim2, 'MODELE', ligrmo, 'INST_R  ',&
                1, 'INST   ', ibid, instam, c16bid,&
                k8bid)
!
! --- REMPLISSAGE DES CHAMPS
!
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom(1:19)
    lpain(3) = 'PTEMPSR'
    lchin(3) = chtime(1:19)
    lpain(4) = 'PMATERC'
    lchin(4) = mate(1:19)
    lpain(5) = 'PCACOQU'
    lchin(5) = chcara(7)(1:19)
    lpain(6) = 'PCAGNPO'
    lchin(6) = chcara(6)(1:19)
    lpain(7) = 'PCADISM'
    lchin(7) = chcara(3)(1:19)
    lpain(8) = 'PDEPLMR'
    lchin(8) = depmoi
    lpain(9) = 'PDEPLPR'
    lchin(9) = depdel
    lpain(10) = 'PCAORIE'
    lchin(10) = chcara(1)(1:19)
    lpain(11) = 'PCACABL'
    lchin(11) = chcara(10)(1:19)
    lpain(12) = 'PCARCRI'
    lchin(12) = carcri(1:19)
    lpain(13) = 'PINSTMR'
    lchin(13) = chtim2(1:19)
    lpain(14) = 'PCOMPOR'
    lchin(14) = compor(1:19)
    lpain(15) = 'PINSTPR'
    lchin(15) = chtime(1:19)
!
! --- CHAMP DE SORTIE
!
    lpaout(1) = 'PMATUUR'
!
    if (prem) then
        do icha = 1, nchar
            inum = 0
            lchout(1) = mesuiv(1:8)//'. '
            nomcha = zk24(jchar+icha-1) (1:8)
            ligrch = nomcha//'.CHME.LIGRE'
            numchm = zi(jinf+nchar+icha)
            call dismoi('F', 'TYPE_CHARGE', zk24(jchar+icha-1), 'CHARGE', ibid,&
                        affcha, ierd)
!
            if (numchm .eq. 4) then
!
! ---- BOUCLES SUR LES TOUS LES TYPES DE CHARGE POSSIBLES SAUF LAPLACE)
                somme = 0
                ligrel = ligrmo
                do k = 1, nbchmx
                    lchin(1) = ligrch(1:13)//nomlig(k)//'.DESC'
                    call exisd('CHAMP_GD', lchin(1), iret)
                    tab(k) = iret
!
                    if (iret .ne. 0) then
!
                        if ((k.ne.2)) then
                            if (affcha(5:7) .eq. '_FO') then
                                option = 'RIGI_MECA_'//nomopf(k)
                                lpain(1) = 'P'//nompaf(k)
                            else
                                option = 'RIGI_MECA_'//nomopr(k)
                                lpain(1) = 'P'//nompar(k)
                            endif
!
! ------------------------- For EFFE_FOND: you need two <CARTE>
!
                            if (option .eq. 'CHAR_MECA_EFON_R') then
                                lpain(16) = 'PPREFFR'
                                lchin(16) = nomcha//'.CHME.PREFF'
                                lpain(1)     = 'PEFOND'
                                lchin(1)     = nomcha//'.CHME.EFOND'
                            endif
                            if (option .eq. 'CHAR_MECA_EFON_F') then
                                lpain(16) = 'PPREFFF'
                                lchin(16) = nomcha//'.CHME.PREFF'
                                lpain(1)     = 'PEFOND'
                                lchin(1)     = nomcha//'.CHME.EFOND'
                            endif
                            lchout(1) (10:10) = 'G'
                            inum = inum + 1
                            call codent(icha, 'D0', lchout(1) (7:8))
                            call codent(inum, 'D0', lchout(1) (12:14))
!
!               POUR UNE MATRICE NON SYMETRIQUE EN COQUE3D (VOIR TE0486)
                            if (k .eq. 4) lpaout(1) = 'PMATUNS'
                            if (k .eq. 3) lpaout(1) = 'PMATUNS'
!
                            call calcul('S', option, ligrel, nbopt(k), lchin,&
                                        lpain, 1, lchout, lpaout, 'V',&
                                        'OUI')
                            call reajre(mesuiv, lchout(1), 'V')
                        endif
                    endif
                    evolch= nomcha//'.CHME.EVOL.CHAR'
                    call jeexin(evolch, ier)
                    if ((tab(k).eq.1) .or. (ier.gt.0)) then
                        somme = somme + 1
                    endif
                enddo
                if (somme .eq. 0) then
                    call u2mess('F', 'MECANONLINE2_4')
                endif
            endif
        enddo
    else
!
! ----- LES MATR_ELEM EXISTENT DEJA, ON REGARDE S'ILS DEPENDENT DE
! ----- LA GEOMETRIE
!
        ligrel = ligrmo
!
        do i = 1, nbchme
            if (zk24(jlme-1+i) (10:10) .eq. 'G') then
                call lxliis(zk24(jlme-1+i) (7:8), icha, ier)
                nomcha = zk24(jchar+icha-1) (1:8)
                ligrch = nomcha//'.CHME.LIGRE'
!
! ---- BOUCLES SUR LES TOUS LES TYPES DE CHARGE POSSIBLES SAUF LAPLACE
!
                call dismoi('F', 'TYPE_CHARGE', zk24(jchar+icha-1), 'CHARGE', ibid,&
                            affcha, ierd)
                do k = 1, nbchmx
                    lchin(1) = ligrch(1:13)//nomlig(k)//'.DESC'
                    call exisd('CHAMP_GD', lchin(1), iret)
                    if (iret .ne. 0) then
                        if (k .ne. 2) then
                            lchout(1) = zk24(jlme-1+i)(1:19)
                            if (affcha(5:7) .eq. '_FO') then
                                option = 'RIGI_MECA_'//nomopf(k)
                                lpain(1) = 'P'//nompaf(k)
                            else
                                option = 'RIGI_MECA_'//nomopr(k)
                                lpain(1) = 'P'//nompar(k)
                            endif
!
! ------------------------- For EFFE_FOND: you need two <CARTE>
!
                            if (option .eq. 'CHAR_MECA_EFON_R') then
                                lpain(16) = 'PPRESSR'
                                lchin(16) = nomcha//'.CHME.PRESS'
                            endif
                            if (option .eq. 'CHAR_MECA_EFON_F') then
                                lpain(16) = 'PPRESSF'
                                lchin(16) = nomcha//'.CHME.PRESS'
                            endif
!               POUR UNE MATRICE NON SYMETRIQUE EN COQUE3D (VOIR TE0486)
                            if (k .eq. 4) lpaout(1) = 'PMATUNS'
                            if (k .eq. 3) lpaout(1) = 'PMATUNS'
!
                            call calcul('S', option, ligrel, nbopt(k), lchin,&
                                        lpain, 1, lchout, lpaout, 'V',&
                                        'OUI')
                        endif
                    endif
                enddo
            endif
        enddo
    endif
!
    call jelira(mesuiv//'.RELR', 'LONUTI', nbchme)
!
!
99  continue
!
    call jedema()
end subroutine
