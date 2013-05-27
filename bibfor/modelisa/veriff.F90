subroutine veriff(nbfonc, nomfon, nbp1, nbp2, long)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     VERIFICATION DE LA COHERENCE DES DISCRETISATIONS SPATIALES DES
!     FONCTIONS DE FORME
!     CARACTERISATION DES DISCRETISATIONS
!     APPELANT : SPECFF
!-----------------------------------------------------------------------
! IN  : NBFONC : NOMBRE DE TABLE_FONCTIONS
! IN  : NOMFON : LISTE DES NOMS DES CONCEPTS DE TYPE TABLE_FONCTION
! OUT : NBP1   : NOMBRE DE POINTS DE DISCRETISATION DES FONCTIONS DE
!                FORME SUR LA PREMIERE DIRECTION D'ESPACE DU REPERE
! OUT : NBP2   : NOMBRE DE POINTS DE DISCRETISATION DES FONCTIONS DE
!                FORME SUR LA DEUXIEME DIRECTION D'ESPACE DU REPERE
! OUT : LONG   : LONGUEUR EXCITEE  LONG = L
!
!
! REMARQUE : DICRETISATION ATTENDUE DES FONCTIONS
!
! LA TABLE FONCTION DOIT CONTENIR DEUX FONCTIONS DE FORME, CHACUNE
! DISCRETISEE SUR UN INTERVALLE 0,L
!
! LA PREMIERE FONCTION DE FORME EST SELON LA PREMIERE DIRECTION D'ESPACE
!  DU REPERE GLOBAL ORTHOGONALE A LA POUTRE (SENS DIRECT).
!
! LA SECONDE EST SELON LA SECONDE DIRECTION D'ESPACE DU REPERE GLOBAL
! ORTHOGONALE A LA POUTRE (SENS DIRECT).
!
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbfonc, nbp1, nbp2
    character(len=8) :: nomfon(nbfonc)
    real(kind=8) :: long
!
    character(len=8) :: k8bid
    character(len=19) :: tbfonc, fonc1, fonc2
    character(len=24) :: lstfon, tbnp, tblp, vale1, vale2
    real(kind=8) :: l1, l2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ifo, ilexc, inbp1, inbp2, ivale1, ivale2
    integer :: itbnp, itblp, ilfon, vali(1)
!-----------------------------------------------------------------------
    call jemarq()
!
!-----VECTEURS DE TRAVAIL POUR LE STOCKAGE DES VALEURS DE L, NBP1 ET
!-----NBP2 POUR CHAQUE FONCTION
!
    call wkvect('&&VERIFF.TEMP.LEXC', 'V V R', nbfonc, ilexc)
    call wkvect('&&VERIFF.TEMP.NBP1', 'V V I', nbfonc, inbp1)
    call wkvect('&&VERIFF.TEMP.NBP2', 'V V I', nbfonc, inbp2)
!
!-----BOUCLE SUR LE NOMBRE DE TABLE_FONCTIONS DE FORME
!
    do 10 ifo = 1, nbfonc
!
        tbfonc = nomfon(ifo)
        tbnp = tbfonc//'.TBNP'
        call jeveuo(tbnp, 'L', itbnp)
!
!       --- VERIFICATION QUE LA TABLE DES FONCTIONS DE FORME EST
!         - VALIDE
        vali(1) = ifo
        if ((zi(itbnp).ne.1) .and. (zi(itbnp+1).ne.2)) then
            call u2mesi('F', 'MODELISA7_67', 1, vali)
        endif
!
        tblp = tbfonc//'.TBLP'
        call jeveuo(tblp, 'L', itblp)
        if (zk24(itblp)(1:8) .ne. 'FONCTION') then
            call u2mesi('F', 'MODELISA7_68', 1, vali)
        endif
!
        lstfon = zk24(itblp+2)
        call jeveuo(lstfon, 'L', ilfon)
        fonc1 = zk8(ilfon) //'           '
        fonc2 = zk8(ilfon+1)//'           '
!
        call jelibe(lstfon)
        call jelibe(tblp)
        call jelibe(tbnp)
!
        vale1 = fonc1//'.VALE'
        vale2 = fonc2//'.VALE'
!
        call jelira(vale1, 'LONUTI', nbp1, k8bid)
        nbp1 = nbp1/2
!
        call jelira(vale2, 'LONUTI', nbp2, k8bid)
        nbp2 = nbp2/2
!
!-------ON VERIFIE QUE L'ON A AU MOINS 2 VALEURS DANS CHAQUE FONCTION
!
        if ((nbp1.lt.2) .or. (nbp2.lt.2)) then
            call u2mesi('F', 'MODELISA7_67', 1, vali)
        endif
!
        call jeveuo(vale1, 'L', ivale1)
        call jeveuo(vale2, 'L', ivale2)
!
!-------ON VERIFIE QUE LA PREMIERE VALEUR DU PARAMETRE EST 0
!
        if ((zr(ivale1).ne.0.d0) .or. (zr(ivale1).ne.0.d0)) then
            call u2mesi('F', 'MODELISA7_69', 1, vali)
        endif
!
!       --- ON VERIFIE QUE LE PARAMETRE L EST LE MEME DS LES 2 FONCTIONS
        l1 = zr(ivale1+nbp1-1)
        l2 = zr(ivale2+nbp2-1)
        if (l1 .ne. l2) then
            call u2mesi('F', 'MODELISA7_70', 1, vali)
        else
            zi(inbp1+ifo-1) = nbp1
            zi(inbp2+ifo-1) = nbp2
            zr(ilexc+ifo-1) = l1
        endif
!
        call jelibe(vale1)
        call jelibe(vale2)
!
!
10  end do
!
!-----VERIFICATION DE LA COHERENCE DES DISCRETISATIONS DE TOUTES LES
!-----FONCTIONS :  - LA LONGUEUR EXCITEE L DOIT ETRE COMMUNE
!-----             - NBP1 ET NBP2 DOIVENT ETRE COMMUNS
!
    if (nbfonc .gt. 1) then
        do 30 ifo = 1, nbfonc-1
            if (zr(ilexc+ifo-1) .ne. zr(ilexc+ifo)) then
                call u2mess('F', 'MODELISA7_71')
            endif
            if (zi(inbp1+ifo-1) .ne. zi(inbp1+ifo) .or. zi(inbp2+ifo-1) .ne. zi(inbp2+ifo)) then
                call u2mess('F', 'MODELISA7_72')
            endif
30      continue
    endif
!
!-----ON RENVOIE LES GRANDEURS CARACTERISTIQUES COMMUNES AUX
!-----DISCRETISATIONS DES FONCTIONS
!
    long = zr(ilexc)
    nbp1 = zi(inbp1)
    nbp2 = zi(inbp2)
!
    call jedetr('&&VERIFF.TEMP.LEXC')
    call jedetr('&&VERIFF.TEMP.NBP1')
    call jedetr('&&VERIFF.TEMP.NBP2')
    call jedema()
end subroutine
