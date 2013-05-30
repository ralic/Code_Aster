subroutine xtopoc(modele)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/cescre.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/dbgcal.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/inical.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=8) :: modele
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - PREPARATION)
!
! AJOUTER À LA SD FISS_XFEM LES DONNÉES TOPOLOGIQUES CONCERNANT
! LES FACETTES DE CONTACT
!
! ----------------------------------------------------------------------
!
!
!  IN  MODELE : NOM DE L'OBJET MODELE
!
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=7, nbin=6)
    character(len=8) :: lpaout(nbout), lpain(nbin), noma, licmp(2), kbid
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: jnoma
    character(len=19) :: ligrel, chgeom
    character(len=19) :: lnno, grlnno, ltno, grltno, fissco, champ(7)
    logical :: debug
    character(len=16) :: option
    integer :: ifmdbg, nivdbg
    integer :: jcesd, jcesv, jcesl, iad, i, iret, nbma, ima, jnbsp, ibid
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    ligrel = modele//'.MODELE'
    call jeveuo(modele//'.MODELE    .LGRF', 'L', jnoma)
    noma = zk8(jnoma)
    chgeom = noma//'.COORDO'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    option = 'TOPOFA'
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- RECUPERATION DES DONNEES XFEM
!
    lnno = modele(1:8)//'.LNNO'
    ltno = modele(1:8)//'.LTNO'
    grlnno = modele(1:8)//'.GRLNNO'
    grltno = modele(1:8)//'.GRLTNO'
    fissco = modele(1:8)//'.FISSCO'
    champ(1) = modele(1:8)//'.TOPOFAC.PI'
    champ(2) = modele(1:8)//'.TOPOFAC.AI'
    champ(3) = modele(1:8)//'.TOPOFAC.CF'
    champ(4) = modele(1:8)//'.TOPOFAC.LO'
    champ(5) = modele(1:8)//'.TOPOFAC.BA'
    champ(6) = modele(1:8)//'.TOPOFAC.OE'
    champ(7) = modele(1:8)//'.TOPOFAC.HE'
!
! --- POUR LE MULTI-HEAVISIDE, TOUS LES CHAMPS DE SORTIE SONT
! --- DUPLIQUÉS PAR LE NOMBRE DE FISSURES VUES
!
    call jeveuo('&&XTYELE.NBSP', 'L', jnbsp)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                kbid, iret)
    licmp(1) = 'NPG_DYN'
    licmp(2) = 'NCMP_DYN'
!
    do 20 i = 1, 7
        call cescre('V', champ(i), 'ELEM', noma, 'DCEL_I',&
                    2, licmp, ibid, -1, -2)
        call jeveuo(champ(i)//'.CESD', 'L', jcesd)
        call jeveuo(champ(i)//'.CESV', 'E', jcesv)
        call jeveuo(champ(i)//'.CESL', 'E', jcesl)
!
! --- REMPLISSAGE DES SOUS-POINTS DE CHAMP(I)
!
        do 10 ima = 1, nbma
            call cesexi('S', jcesd, jcesl, ima, 1,&
                        1, 1, iad)
            zl(jcesl-1-iad) = .true.
            zi(jcesv-1-iad) = zi(jnbsp-1+ima)
            if (i .eq. 7) zi(jcesv-1-iad) = zi(jnbsp-1+ima)**2
10      continue
!
20  end do
!
! --- CREATION DES LISTES DES CHAMPS IN
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PLSN'
    lchin(2) = lnno
    lpain(3) = 'PLST'
    lchin(3) = ltno
    lpain(4) = 'PGRADLN'
    lchin(4) = grlnno
    lpain(5) = 'PGRADLT'
    lchin(5) = grltno
    lpain(6) = 'PFISCO'
    lchin(6) = fissco
!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PPINTER'
    lchout(1) = champ(1)
    lpaout(2) = 'PAINTER'
    lchout(2) = champ(2)
    lpaout(3) = 'PCFACE'
    lchout(3) = champ(3)
    lpaout(4) = 'PLONCHA'
    lchout(4) = champ(4)
    lpaout(5) = 'PBASECO'
    lchout(5) = champ(5)
    lpaout(6) = 'PGESCLA'
    lchout(6) = champ(6)
    lpaout(7) = 'PHEAVFA'
    lchout(7) = champ(7)
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
    call calcul('C', option, ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'G',&
                'OUI')
!
    call jedema()
end subroutine
