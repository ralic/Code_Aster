subroutine cafono(char, ligrcz, igrel, inema, noma,&
                  ligrmz, fonree)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/r8dgrd.h'
    include 'asterfort/affono.h'
    include 'asterfort/alcart.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nocart.h'
    include 'asterfort/noligr.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: igrel, inema
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrcz, ligrmz
!     -----------------------------------------------------------------
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
!     REMPLIR LA CARTE .FORNO, ET LE LIGREL POUR FORC_NO
!     -----------------------------------------------------------------
!   ARGUMENTS D'ENTREE:
!      CHAR  : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRCZ: NOM DU LIGREL DE CHARGE
!      IGREL : NUMERO DU GREL DE CHARGE
!      INEMA : NUMERO  DE LA DERNIERE MAILLE TARDIVE DANS LIGRCH
!      NBTOUT: NOMBRE TOTAL DE GROUPES, NOEUDS,.. DANS LES OCCURENCES
!      NOMA  : NOM DU MAILLAGE
!      LIGRMZ: NOM DU LIGREL DE MODELE
!      FONREE  : 'FONC' OU 'REEL'
!     -----------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: nmocl, nfono, n2dl, n3dl, n6dl, ncoq2d, nbcomp
!-----------------------------------------------------------------------
    integer :: i, idgex, ierd, ii, in, ino, iret
    integer :: j, jdesgi, jj, jl, jnbno, jncmp, jno
    integer :: jnono, jprnm, jval, jvalv, nangl, nbec, nbecf
    integer :: nbno, nbnoeu, nsurch, numel
!-----------------------------------------------------------------------
    parameter     (nmocl=10)
    integer :: ntypel(nmocl), forimp(nmocl)
    real(kind=8) :: dgrd, valfor(nmocl)
    logical :: verif
    character(len=1) :: k1bid
    character(len=8) :: k8bid, nomn, typmcl(2), typlag, valfof(nmocl)
    character(len=16) :: motcle(nmocl), motclf, motcls(2)
    character(len=19) :: carte, ligrmo, ligrch
    character(len=24) :: liel, nomnoe, nomele, mesnoe
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'FORCE_NODALE'
    call getfac(motclf, nfono)
    if (nfono .eq. 0) goto 9999
!
    ligrch = ligrcz
    typlag(1:2) = '12'
!
    verif = .true.
!
    call jenonu(jexnom('&CATA.TE.NOMTE', 'FORCE_NOD_2DDL' ), n2dl)
    call jenonu(jexnom('&CATA.TE.NOMTE', 'FORCE_NOD_3DDL' ), n3dl)
    call jenonu(jexnom('&CATA.TE.NOMTE', 'FORCE_NOD_6DDL' ), n6dl)
    call jenonu(jexnom('&CATA.TE.NOMTE', 'FORCE_NOD_COQ2D'), ncoq2d)
    ntypel(1) = n2dl
    ntypel(2) = n2dl
    ntypel(3) = n3dl
    ntypel(4) = n6dl
    ntypel(5) = n6dl
    ntypel(6) = n6dl
!
! ---------------------------------------------------
!     RECUPERATION DES MOTS-CLES DDL POSSIBLES SOUS FORCE_NODALE
! ---------------------------------------------------
    motcle(1) = 'FX'
    motcle(2) = 'FY'
    motcle(3) = 'FZ'
    motcle(4) = 'MX'
    motcle(5) = 'MY'
    motcle(6) = 'MZ'
    motcle(7) = 'REP'
    motcle(8) = 'ALPHA'
    motcle(9) = 'BETA'
    motcle(10) = 'GAMMA'
    nbcomp = 10
!
! ---------------------------------------------------
! *** RECUPERATION DU DESCRIPTEUR GRANDEUR .PRNM
! *** DU MODELE
! ---------------------------------------------------
!
    call dismoi('F', 'NB_EC', 'FORC_R', 'GRANDEUR', nbecf,&
                k8bid, ierd)
    if (nbecf .gt. 10) then
        call u2mess('F', 'MODELISA2_65')
    else
        ligrmo = ligrmz
        call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    endif
!
    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nbec,&
                k8bid, ierd)
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    endif
!
    call jeveuo(ligrch//'.NBNO', 'E', jnbno)
    nomnoe = noma//'.NOMNOE'
    call jelira(nomnoe, 'NOMMAX', nbnoeu, k1bid)
!
    mesnoe = '&&CAFONO.MES_NOEUDS'
    motcls(1) = 'GROUP_NO'
    motcls(2) = 'NOEUD'
    typmcl(1) = 'GROUP_NO'
    typmcl(2) = 'NOEUD'
!
! ---------------------------------------------------
!     ALLOCATION DE TABLEAUX DE TRAVAIL
! ---------------------------------------------------
!   OBJETS INTERMEDIAIRES PERMETTANT D'APPLIQUER LA REGLE DE SURCHARGE
!        -  VECTEUR (K8) CONTENANT LES NOMS DES NOEUDS
!        -  TABLEAU DES VALEURS DES DDLS DES FORCES IMPOSEES
!                         DIM NBNOEU * NBCOMP
!        -  VECTEUR (IS) CONTENANT LE DESCRIPTEUR GRANDEUR ASSOCIE AUX
!                         FORCES IMPOSEES PAR NOEUD
!
    call wkvect('&&CAFONO.NOMS_NOEUDS', 'V V K8', nbnoeu, jnono)
    if (fonree .eq. 'REEL') then
        call wkvect('&&CAFONO.VALDDLR', 'V V R', nbcomp*nbnoeu, jval)
    else
        call wkvect('&&CAFONO.VALDDLF', 'V V K8', nbcomp*nbnoeu, jval)
    endif
    call wkvect('&&CAFONO.DESGI', 'V V I', nbnoeu, jdesgi)
!
    dgrd = r8dgrd()
    if (fonree .eq. 'FONC') then
        do 10 i = 1, nbcomp*nbnoeu
            zk8(jval-1+i) = '&FOZERO'
10      continue
    endif
    nsurch = 0
!
! --------------------------------------------------------------
!     BOUCLE SUR LES OCCURENCES DU MOT-CLE FACTEUR FORCE_NODALE
! --------------------------------------------------------------
!
    do 110 i = 1, nfono
        do 20 ii = 1, nbcomp
            forimp(ii) = 0
20      continue
!
        if (fonree .eq. 'REEL') then
            do 30 j = 1, 6
                call getvr8(motclf, motcle(j), i, iarg, 1,&
                            valfor(j), forimp(j))
30          continue
!
            call getvr8(motclf, 'ANGL_NAUT', i, iarg, 3,&
                        valfor(8), nangl)
            if (nangl .ne. 0) then
!              --- REPERE UTILISATEUR ---
                valfor(7) = -1.d0
                forimp(7) = 1
                do 40 ii = 1, min(3, abs(nangl))
                    valfor(7+ii) = valfor(7+ii)*dgrd
                    forimp(7+ii) = 1
40              continue
            else
!              --- REPERE GLOBAL ---
                valfor(7) = 0.d0
            endif
!
        else if (fonree.eq.'FONC') then
            do 50 ii = 1, nbcomp
                valfof(ii) = '&FOZERO'
50          continue
            do 60 j = 1, 6
                call getvid(motclf, motcle(j), i, iarg, 1,&
                            valfof(j), forimp(j))
60          continue
!
            call getvid(motclf, 'ANGL_NAUT', i, iarg, 3,&
                        valfof(8), nangl)
            if (nangl .ne. 0) then
!              --- REPERE UTILISATEUR ---
                valfof(7) = 'UTILISAT'
                forimp(7) = 1
                do 70 ii = 1, min(3, abs(nangl))
                    forimp(7+ii) = 1
70              continue
            else
!              --- REPERE GLOBAL ---
                valfof(7) = 'GLOBAL'
            endif
        endif
        if (nangl .lt. 0) then
            call u2mess('A', 'MODELISA2_66')
        endif
!
!       ---------------------------
!       CAS DE GROUP_NO ET DE NOEUD
!       ---------------------------
!
        call reliem(' ', noma, 'NO_NOEUD', motclf, i,&
                    2, motcls, typmcl, mesnoe, nbno)
        if (nbno .eq. 0) goto 110
        call jeveuo(mesnoe, 'L', jno)
!
        do 100 jj = 1, nbno
            call jenonu(jexnom(nomnoe, zk8(jno-1+jj)), ino)
            zk8(jnono-1+ino) = zk8(jno-1+jj)
            call affono(zr(jval), zk8(jval), zi(jdesgi+ino-1), zi(jprnm- 1+(ino-1)*nbec+1),&
                        nbcomp, fonree, zk8(jno-1+jj), ino, nsurch,&
                        forimp, valfor, valfof, motcle, verif,&
                        nbec)
100      continue
!
        call jedetr(mesnoe)
!
110  end do
!
!     -----------------------------------------------
!     AFFECTATION DU LIGREL ET STOCKAGE DANS LA CARTE
!              DIMENSIONS AUX VRAIES VALEURS
!     -----------------------------------------------
!
    liel = ligrch//'.LIEL'
    carte = char//'.CHME.FORNO'
!
    call jeexin(carte//'.DESC', iret)
!
    if (iret .eq. 0) then
        if (fonree .eq. 'REEL') then
            call alcart('G', carte, noma, 'FORC_R')
        else if (fonree.eq.'FONC') then
            call alcart('G', carte, noma, 'FORC_F')
        else
            call u2mesk('F', 'MODELISA2_37', 1, fonree)
        endif
    endif
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
    zk8(jncmp-1+1) = 'FX'
    zk8(jncmp-1+2) = 'FY'
    zk8(jncmp-1+3) = 'FZ'
    zk8(jncmp-1+4) = 'MX'
    zk8(jncmp-1+5) = 'MY'
    zk8(jncmp-1+6) = 'MZ'
    zk8(jncmp-1+7) = 'REP'
    zk8(jncmp-1+8) = 'ALPHA'
    zk8(jncmp-1+9) = 'BETA'
    zk8(jncmp-1+10) = 'GAMMA'
!
    call jeveuo(ligrch//'.NBNO', 'E', jnbno)
!
!     -----------------------------------------------
!     BOUCLE SUR TOUS LES NOEUDS DU MAILLAGE
!     -----------------------------------------------
!
    do 150 ino = 1, nbnoeu
!
        if (zi(jdesgi-1+ino) .ne. 0) then
!
            nomn = zk8(jnono-1+ino)
            call jenonu(jexnom(nomnoe, nomn), in)
            idgex = jprnm - 1 + (in-1)*nbec + 1
!
            do 120 i = 1, 6
                if (exisdg(zi(idgex),i)) then
                    numel = ntypel(i)
                endif
120          continue
            if ((exisdg(zi(idgex),6)) .and. (.not. (exisdg(zi(idgex), 4)))) then
                numel = ncoq2d
            endif
!
            igrel = igrel + 1
            call jenuno(jexnum('&CATA.TE.NOMTE', numel), nomele)
            call noligr(ligrch, igrel, numel, 1, in,&
                        ' ', 1, 1, inema, zi(jnbno),&
                        typlag)
!
            call jeveuo(jexnum(liel, igrel), 'E', jl)
            if (fonree .eq. 'REEL') then
                do 130 i = 1, nbcomp
                    zr(jvalv-1+i) = zr(jval-1+nbcomp* (ino-1)+i)
130              continue
            else
                do 140 i = 1, nbcomp
                    zk8(jvalv-1+i) = zk8(jval-1+nbcomp* (ino-1)+i)
140              continue
            endif
!
!   ON CREE UNE CARTE POUR CHAQUE NOEUD AFFECTE ET ON NOTE TOUTES
!   LES COMPOSANTES (NBCOMP)
!
            call nocart(carte, -3, ' ', 'NUM', 1,&
                        ' ', zi(jl), liel, nbcomp)
!
        endif
!
150  end do
!
    call jedetr('&&CAFONO.NOMS_NOEUDS')
    call jedetr('&&CAFONO.DESGI')
    if (fonree .eq. 'REEL') then
        call jedetr('&&CAFONO.VALDDLR')
    else if (fonree.eq.'FONC') then
        call jedetr('&&CAFONO.VALDDLF')
    endif
9999  continue
    call jedema()
end subroutine
