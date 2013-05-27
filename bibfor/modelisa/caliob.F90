subroutine caliob(fonree, charge)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8dgrd.h'
    include 'asterfort/aflrch.h'
    include 'asterfort/afrela.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxcadr.h'
    include 'asterfort/lxcaps.h'
    include 'asterfort/matrot.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=4) :: fonree
    character(len=8) :: charge
! ---------------------------------------------------------------------
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
!
!     CREER LES CARTES CHAR.CHME.CMULT ET CHAR.CHME.CIMPO
!          ET REMPLIR LIGRCH, POUR LE MOT-CLE 'LIAISON_OBLIQUE'
!
! IN  : FONREE : 'REEL' OU 'FONC'
! IN  : CHARGE : NOM UTILISATEUR DU RESULTAT DE CHARGE
! ---------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i1, ibid, ier, igr, in, ino
    integer :: iocc, iret, j, jcmu, jcmuc, jddl, jgr0
    integer :: jjj, jliste, jnoma, k, n, n1, na, jlist2
    integer :: nbddl, nbem, nbet, nbgm, nbno, ndim, ndimmo
    integer :: nent, ng, ngr, ngro, nliai, nno
    real(kind=8) :: beta
!-----------------------------------------------------------------------
    parameter (nbddl=6)
    integer :: imp
    complex(kind=8) :: betac
    character(len=2) :: typlag
    character(len=4) :: typcoe
    character(len=8) :: k8bid, motcle, kbeta, mogrou, mod, noma
    character(len=8) :: nomnoe
    character(len=16) :: motfac
    character(len=19) :: lisrel
    character(len=24) :: trav, grouma, noeuma
    character(len=24) :: valk(3)
    character(len=19) :: ligrmo
    real(kind=8) :: direct(3)
    real(kind=8) :: mat(3, 3)
    real(kind=8) :: dgrd, zero
    real(kind=8) :: angl(3), reel
    character(len=8) :: kddl(nbddl), kfonc
    character(len=1) :: k1bid
    integer :: iarg
! ----------------------------------------------------------------------
    data kddl/'DX','DY','DZ','DRX','DRY','DRZ'/
! ----------------------------------------------------------------------
!
    call jemarq()
    lisrel = '&&CALIOB.RLLISTE'
    motfac = 'LIAISON_OBLIQUE '
    typcoe = 'REEL'
    if (fonree .eq. 'COMP') then
        typcoe = 'COMP'
    endif
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 110
!
    dgrd = r8dgrd()
    zero = 0.d0
!
    motcle = 'NOEUD   '
    mogrou = 'GROUP_NO'
    typlag = '12'
! --- INITIALISATION PROVISOIRE ---
    betac = (1.0d0,0.0d0)
!
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE ---
!
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
!
! ---  LIGREL DU MODELE ---
!
    ligrmo = mod//'.MODELE'
!
! --- MAILLAGE ASSOCIE AU MODELE ---
!
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
! --- DIMENSION ASSOCIEE AU MODELE ---
    call dismoi('F', 'DIM_GEOM', mod, 'MODELE', ndimmo,&
                k8bid, ier)
    if (.not.(ndimmo.eq.2.or.ndimmo.eq.3)) call u2mess('F', 'MODELISA2_6')
!
    noeuma = noma//'.NOMNOE'
    grouma = noma//'.GROUPENO'
!
    ndim = 0
    ngro = 0
    nbgm = 0
    nbem = 0
    nbet = 0
    nent = 0
!
! --- DETERMINATION DU NOMBRE TOTAL DE NOEUDS INTERVENANT ---
! --- DANS TOUTES LES LIAISONS                            ---
!
    do 10 i = 1, nliai
        call getvtx(motfac, mogrou, i, iarg, 0,&
                    k8bid, ngro)
        call getvtx(motfac, motcle, i, iarg, 0,&
                    k8bid, nent)
!
        ngro = -ngro
        nbgm = max(nbgm,ngro)
        nent = -nent
        nbem = max(nbem,nent)
        nbet = nbet + nent
10  end do
!
    ndim = max(nbgm,nbem)
    if (ndim .eq. 0) then
        call u2mesk('F', 'MODELISA3_13', 1, motfac)
    endif
    trav = '&&CALIOB.'//motfac
    call wkvect(trav, 'V V K24', ndim, jjj)
!
    do 40 iocc = 1, nliai
        call getvtx(motfac, mogrou, iocc, iarg, ndim,&
                    zk24(jjj), ngr)
        do 20 igr = 1, ngr
            call jeexin(jexnom(grouma, zk24(jjj+igr-1)), iret)
            if (iret .eq. 0) then
                valk(1) = zk24(jjj+igr-1)
                valk(2) = noma
                call u2mesk('F', 'MODELISA2_95', 2, valk)
            else
                call jelira(jexnom(grouma, zk24(jjj+igr-1)), 'LONUTI', n1, k1bid)
            endif
20      continue
        call getvtx(motfac, motcle, iocc, iarg, ndim,&
                    zk24(jjj), nno)
        do 30 ino = 1, nno
            call jenonu(jexnom(noeuma, zk24(jjj+ino-1)), iret)
            if (iret .eq. 0) then
                valk(1) = motcle
                valk(2) = zk24(jjj+ino-1)
                valk(3) = noma
                call u2mesk('F', 'MODELISA2_96', 3, valk)
            endif
30      continue
40  end do
!
!     ALLOCATION DE TABLEAUX DE TRAVAIL
!
    call wkvect('&&CALIOB.LISTE', 'V V K24', ndim, jliste)
    call wkvect('&&CALIOB.LIST2', 'V V K8', ndim, jlist2)
    call wkvect('&&CALIOB.DDL  ', 'V V K8', nbddl, jddl)
    call wkvect('&&CALIOB.COEMU', 'V V R', nbddl, jcmu)
    call wkvect('&&CALIOB.COEMUC', 'V V C', nbddl, jcmuc)
!
!     MISE A JOUR DE LIGRCH ET STOCKAGE DANS LES CARTES
!
    do 100 i = 1, nliai
        angl(1) = zero
        angl(2) = zero
        angl(3) = zero
        call getvr8(motfac, 'ANGL_NAUT', i, iarg, 3,&
                    angl, na)
!
        do 50 i1 = 1, min(3, abs(na))
            angl(i1) = angl(i1)*dgrd
50      continue
!
!  --- MATRICE DE PASSAGE AU REPERE GLOBAL ---
!
        call matrot(angl, mat)
!
        n1 = 0
        do 90 i1 = 1, nbddl
!
            if (fonree .eq. 'REEL') then
                call getvr8(motfac, kddl(i1), i, iarg, 1,&
                            reel, imp)
!
                if (imp .ne. 0) then
                    zk8(jddl) = kddl(i1)
                    zr(jcmu) = 1.0d0
                    beta = reel
                endif
            else
                call getvid(motfac, kddl(i1), i, iarg, 1,&
                            kfonc, imp)
!
                if (imp .ne. 0) then
                    zk8(jddl) = kddl(i1)
                    zr(jcmu) = 1.0d0
                    kbeta = kfonc
                endif
            endif
            if (imp .eq. 0) goto 90
!
            call lxcaps(zk8(jddl))
            call lxcadr(zk8(jddl))
!
            if (zk8(jddl) .eq. 'DX' .or. zk8(jddl) .eq. 'DRX') then
                direct(1) = mat(1,1)
                direct(2) = mat(1,2)
                direct(3) = mat(1,3)
            else if (zk8(jddl).eq.'DY' .or. zk8(jddl).eq.'DRY') then
                direct(1) = mat(2,1)
                direct(2) = mat(2,2)
                direct(3) = mat(2,3)
            else if (zk8(jddl).eq.'DZ' .or. zk8(jddl).eq.'DRZ') then
                direct(1) = mat(3,1)
                direct(2) = mat(3,2)
                direct(3) = mat(3,3)
            endif
!
            if (zk8(jddl) .eq. 'DX' .or. zk8(jddl) .eq. 'DY' .or. zk8( jddl) .eq. 'DZ') then
                zk8(jddl) = 'DEPL'
                else if (zk8(jddl).eq.'DRX' .or. zk8(jddl).eq.'DRY' .or.&
            zk8(jddl).eq.'DRZ') then
                zk8(jddl) = 'ROTA'
            endif
!
! ---   CAS DE GROUP_NO ---
!
            call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO', i,&
                        iarg, 0, zk24(jliste), ng)
            if (ng .ne. 0) then
                ng = -ng
                call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO', i,&
                            iarg, ng, zk24(jliste), n)
                do 70 j = 1, ng
                    call jeveuo(jexnom(grouma, zk24(jliste-1+j)), 'L', jgr0)
                    call jelira(jexnom(grouma, zk24(jliste-1+j)), 'LONUTI', n, k1bid)
                    do 60 k = 1, n
                        in = zi(jgr0-1+k)
                        call jenuno(jexnum(noma//'.NOMNOE', in), nomnoe)
!
! --- CREATION ET CALCUL DE LA RELATION      ---
! --- ET AFFECTATION A LA LISTE DE RELATIONS ---
!
                        call afrela(zr(jcmu), zc(jcmuc), zk8(jddl), nomnoe, ndimmo,&
                                    direct, 1, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 0.d0, lisrel)
60                  continue
70              continue
!
!
            else
!
! ---   CAS DE NOEUD ---
!
                call getvem(noma, 'NOEUD', motfac, 'NOEUD', i,&
                            iarg, 0, zk8(jlist2), nbno)
                if (nbno .ne. 0) then
                    nbno = -nbno
                    call getvem(noma, 'NOEUD', motfac, 'NOEUD', i,&
                                iarg, nbno, zk8(jlist2), n)
!
                    do 80 k = 1, nbno
!
! --- CREATION ET CALCUL DE LA RELATION      ---
! --- ET AFFECTATION A LA LISTE DE RELATIONS ---
!
                        call afrela(zr(jcmu), zc(jcmuc), zk8(jddl), zk8(jlist2+k-1), ndimmo,&
                                    direct, 1, beta, betac, kbeta,&
                                    typcoe, fonree, typlag, 0.d0, lisrel)
80                  continue
!
                endif
!
            endif
90      continue
!
100  end do
!
! --- AFFECTATION DE LA LISTE DE RELATIONS A LA CHARGE ---
!
    call aflrch(lisrel, charge)
!
!
! --- MENAGE
!
    call jedetc('V', '&&CALIOB.RLLISTE', 1)
    call jedetr(trav)
    call jedetr('&&CALIOB.LISTE')
    call jedetr('&&CALIOB.DDL  ')
    call jedetr('&&CALIOB.COEMU')
    call jedetr('&&CALIOB.COEMUC')
!
110  continue
    call jedema()
end subroutine
