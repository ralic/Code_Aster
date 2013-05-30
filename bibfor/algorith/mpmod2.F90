subroutine mpmod2(basemo, nommes, nbmesu, nbmtot, basepr,&
                  vnoeud, vrange, vcham)
!
!     ------------------------------------------------------------------
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
!
!     PROJ_MESU_MODAL : PROJECTION DE LA MATRICE MODALE SUR LES CMP
!                       DES NOEUDS MESURE
!
!     IN  : BASEMO : NOM DE LA BASE DE PROJECTION
!     IN  : NOMMES : NOM DE LA MESURE
!     IN  : NBMESU : NOMBRE DE MESURE (DATASET 58)
!     IN  : NBMTOT : NOMBRE DE VECTEURS DE BASE
!     OUT  : BASEPR : NOM BASE PROJETEE SUIVANT DIRECTION MESURE
!     IN  : VNOEUD : NOM RANGEMENT NOEUD MESURE
!     IN  : VRANGE : NOM CORRESPONDANCE CMP SUIVANT VNOEUD
!     IN  : VCHAM : NOM CORRESPONDANCE NOMCHAMP SUIVANT VNOEUD
!
    implicit none
!     ------------------------------------------------------------------
!
!
!
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8prem.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnsprj.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mpjeft.h'
    include 'asterfort/mpmod3.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: basemo, nommes
    character(len=24) :: vnoeud, vrange, basepr, vcham
    integer :: nbmesu, nbmtot
!
    character(len=8) :: nomres, k8bid
    character(len=16) :: nomcha, corres, nomch, typres, k16bid, nomchm
    character(len=19) :: chamno, ch1s, ch2s
    character(len=24) :: vorien
!
    integer :: lord, lred, lori, lrange
    integer :: imesu, ii, imode, iret
    integer :: iposd, icmp, ino
    integer :: lnoeud, nnoema, ncmpma
    integer :: jcnsd, jcnsc, jcnsv, jcnsl, jcnsk
    integer :: ibid, nbcmpi, nbcham, lch, ich, lcham
!
    real(kind=8) :: vori(3)
    real(kind=8) :: val, vect(3)
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! RECUPERATION DU NOM DU CONCEPT RESULTAT
    call getres(nomres, typres, k16bid)
!
! =============================================
! RECUPERATION DES OBJETS LIES A LA MESURE
! =============================================
!
    call mpmod3(basemo, nommes, nbmesu, nbmtot, vcham,&
                vnoeud, vrange, vorien, nnoema, ncmpma)
!
    call jeveuo(vnoeud, 'L', lnoeud)
    call jeveuo(vrange, 'L', lrange)
    call jeveuo(vcham, 'L', lcham)
    call jeveuo(vorien, 'L', lori)
!
! RECUPERATION DES NOMS DU CHAMP MESURE
!
    call getvtx('MODELE_MESURE', 'NOM_CHAM', 1, iarg, 0,&
                nomcha, nbcham)
    if (nbcham .ne. 0) then
        nbcham = -nbcham
    else
        call u2mess('A', 'ALGORITH10_93')
    endif
    call wkvect('&&LISTE_CHAMP', 'V V K16', nbcham, lch)
    call getvtx('MODELE_MESURE', 'NOM_CHAM', 1, iarg, nbcham,&
                zk16(lch), ibid)
!
!     -> OBJET MATRICE MODALE REDUITE SUIVANT DIRECTION DE MESURE
!
    basepr = nomres//'.PROJM    .PJMBP'
!
    call wkvect(basepr, 'G V R', nnoema*ncmpma*nbmtot, lred)
!
!     INITIALISATION DE BASEPR
    do 152 ii = 1, nnoema*ncmpma*nbmtot
        zr(lred-1 + ii) = 0.d0
152  end do
!
! RECUPERATION SD CORRESPONDANCE ENTRE MAILLAGE MODELE/MESURE
!
    corres = '&&PJEFTE.CORRESP'
    call mpjeft(corres)
!
! BOUCLE SUR LES CHAMPS MESURES
! POUR L'INSTANT ON NE RAJOUTE PAS DE PONDERATION SUR LES MESURE
! A FAIRE EVENTUELLEMENT : EN FONCTION DU TYPE DE CHAMP
!
    do 151 ich = 1, nbcham
        nomcha = zk16(lch-1 +ich)
        nomch = nomcha
! MEMES VECTEURS DE BASE POUR : DEPL, VITE ET ACCE
        if (nomch .eq. 'VITE' .or. nomch .eq. 'ACCE') nomch = 'DEPL'
! MEMES VECTEURS DE BASE POUR LES CONTRAINTES
        if (nomch(1:4) .eq. 'SIEF') nomch = 'SIGM_NOEU'
! MEMES VECTEURS DE BASE POUR LES DEFORMATIONS
        if (nomch(1:4) .eq. 'EPSI') nomch = 'EPSI_NOEU'
!
        call jeveuo(basemo//'           .ORDR', 'L', lord)
!
        ch1s='&&PJEFPR.CH1S'
        ch2s='&&PJEFPR.CH2S'
!
! BOUCLE SUR TOUS LES MODES
!
        do 10 imode = 1, nbmtot
!
            call rsexch('F', basemo, nomch, zi(lord-1+imode), chamno,&
                        iret)
!
!       2-1 : TRANSFORMATION DE CHAMNO EN CHAM_NO_S : CH1S
            call detrsd('CHAM_NO_S', ch1s)
            call cnocns(chamno, 'V', ch1s)
!
!       2-2 : PROJECTION DU CHAM_NO_S : CH1S -> CH2S
            call detrsd('CHAM_NO_S', ch2s)
            call cnsprj(ch1s, corres, 'V', ch2s, iret)
            if (iret .gt. 0) call u2mess('F', 'ALGORITH6_25')
!
            call jeveuo(ch2s//'.CNSK', 'L', jcnsk)
            call jeveuo(ch2s//'.CNSD', 'L', jcnsd)
            call jeveuo(ch2s//'.CNSC', 'L', jcnsc)
            call jeveuo(ch2s//'.CNSV', 'L', jcnsv)
            call jeveuo(ch2s//'.CNSL', 'L', jcnsl)
!
            nbcmpi = zi(jcnsd-1 + 2)
!
! BOUCLE SUR LES POINTS DE MESURE
!
            do 20 imesu = 1, nbmesu
!
                nomchm = zk16(lcham-1+imesu)
! MEMES VECTEURS DE BASE POUR : DEPL, VITE ET ACCE
                if (nomchm .eq. 'VITE' .or. nomchm .eq. 'ACCE') nomchm = 'DEPL'
! MEMES VECTEURS DE BASE POUR LES CONTRAINTES
                if (nomchm(1:4) .eq. 'SIEF') nomchm = 'SIGM_NOEU'
! MEMES VECTEURS DE BASE POUR LES DEFORMATIONS
                if (nomchm(1:4) .eq. 'EPSI') nomchm = 'EPSI_NOEU'
!
! NUMERO DU NOEUD ASSOCIE A IMESU : INO
                ino = zi(lnoeud-1 + imesu)
!
                if ((nomch(1:4) .eq. 'DEPL') .and. (nomchm(1:4) .eq. 'DEPL')) then
!
! RECUPERATION DIRECTION DE MESURE (VECTEUR DIRECTEUR)
                    do 21 ii = 1, 3
                        vori(ii) = zr(lori-1 + (imesu-1)*3 +ii)
21                  continue
!
! NORMALISATION DU VECTEUR DIRECTEUR
                    val = 0.d0
                    do 22 ii = 1, 3
                        val = val + vori(ii)*vori(ii)
22                  continue
                    val = sqrt(val)
                    if (val .lt. r8prem()) then
                        call u2mess('F', 'ALGORITH6_26')
                    endif
                    do 23 ii = 1, 3
                        vori(ii) = vori(ii)/val
23                  continue
!
! RECUPERATION DU CHAMP AU NOEUD (BASE)
!
                    do 101 icmp = 1, nbcmpi
                        if (zk8(jcnsc-1 +icmp) .eq. 'DX') vect(1) = zr(&
                                                                    jcnsv-1 +(ino-1)*nbcmpi+icmp)
                        if (zk8(jcnsc-1 +icmp) .eq. 'DY') vect(2) = zr(&
                                                                    jcnsv-1 +(ino-1)*nbcmpi+icmp)
                        if (zk8(jcnsc-1 +icmp) .eq. 'DZ') vect(3) = zr(&
                                                                    jcnsv-1 +(ino-1)*nbcmpi+icmp)
101                  continue
!
! CALCUL DE LA BASE RESTREINTE
!
                    iposd = (imode-1)*nbmesu + imesu
                    zr(lred-1 + iposd) = 0.d0
!
                    do 300 ii = 1, 3
                        zr(lred-1 + iposd) = zr(lred-1 + iposd) + vect(ii) * vori(ii)
300                  continue
!
                    else if ( (nomch(1:14) .eq. 'EPSI_NOEU') .and.&
                (nomchm(1:14) .eq. 'EPSI_NOEU') ) then
!
                    iposd = (imode-1)*nbmesu + imesu
                    do 401 icmp = 1, nbcmpi
                        if (zk8(jcnsc-1 +icmp) .eq. zk8(lrange-1 + imesu)) zr(lred-1 +iposd) = &
                                                                           zr(&
                                                                           jcnsv-1 +(ino- 1&
                                                                           )*nbcmpi+icmp&
                                                                           )
401                  continue
!
                    else if ( (nomch(1:14) .eq. 'SIGM_NOEU') .and.&
                (nomchm(1:14) .eq. 'SIGM_NOEU') ) then
!
                    iposd = (imode-1)*nbmesu + imesu
                    do 501 icmp = 1, nbcmpi
                        if (zk8(jcnsc-1 +icmp) .eq. zk8(lrange-1 + imesu)) zr(lred-1 +iposd) = &
                                                                           zr(&
                                                                           jcnsv-1 +(ino- 1&
                                                                           )*nbcmpi+icmp&
                                                                           )
501                  continue
!
                endif
!
! FIN DE LA BOUCLE SUR LES POINTS DE MESURE
!
20          continue
!
! FIN DE LA BOUCLE SUR LES MODES
!
10      continue
!
! FIN BOUCLE SUR LES NOMCHA
!
151  end do
!
    call jeecra(basepr, 'LONUTI', nbmesu*nbmtot, k8bid)
!
! DESTRUCTION DES VECTEURS DE TRAVAIL
!
    call detrsd('CHAM_NO_S', ch1s)
    call detrsd('CHAM_NO_S', ch2s)
    call detrsd('CORRESP_2_MAILLA', corres)
!
    call jedema()
!
end subroutine
