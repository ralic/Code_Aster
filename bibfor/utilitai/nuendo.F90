subroutine nuendo(numedd, sdnuen)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
!
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbelem.h'
    include 'asterfort/nbgrel.h'
    include 'asterfort/typele.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: numedd
    character(len=24) :: sdnuen
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DE LA SD POUR REPERAGE DDL ENDOMMAGEMENT AUX NOEUDS
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  SDNUEN : NOM DE LA S.D. DDL ENDOMMAGEMENT AUX NOEUDS
!
!
!
!
    character(len=8) :: nocmp, nomgd, modele, noma, k8bid
    character(len=16) :: nomte
    character(len=19) :: ligrmo
    character(len=24) :: nolili, noliel
    integer :: nec, nbnoeu, ncmpmx
    integer :: nlili, nbno, neq, nbnoc
    integer :: ier, ibid, ico
    integer :: ima, ino, idamg, i, k, inoc, ival, iadg
    integer :: itrav, iconex
    integer :: iancmp, ianueq, iaprno
    integer :: ifm, niv
    integer :: nbgr, igr, te, nbelgr, liel, iel
    integer :: jnuen
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- MODELE ASSOCIE AU NUME_DDL
!
    call dismoi('F', 'NOM_MODELE', numedd, 'NUME_DDL', ibid,&
                modele, ier)
!
! --- NOM DU MAILLAGE
!
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, ier)
!
! --- NOMBRE DE NOEUDS DU MAILLAGE
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoeu,&
                k8bid, ier)
!
! --- LIGREL DU MODELE
!
    ligrmo = modele//'.MODELE'
!
! --- CREATION D'UN VECTEUR DESTINE A CONTENIR LES NUMEROS DES NOEUDS
!
    call wkvect('&&NUENDO.NOEUDS', 'V V I', nbnoeu, itrav)
!
! --- REPERAGE DES NOEUDS SUR ELEMENTS AVEC ENDOMMAGEMENT AUX NOEUDS
!
    nbgr = nbgrel(ligrmo)
    noliel = ligrmo//'.LIEL'
    do 140 igr = 1, nbgr
        te = typele(ligrmo,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
        nbelgr = nbelem(ligrmo,igr)
        call jeveuo(jexnum(noliel, igr), 'L', liel)
        if (nomte .eq. 'MNDPTR6' .or. nomte .eq. 'MNDPQS8' .or. nomte .eq. 'MNAXTR6' .or.&
            nomte .eq. 'MNAXQS8' .or. nomte .eq. 'MNVG_HEXA20' .or. nomte .eq.&
            'MNVG_TETRA10' .or. nomte .eq. 'MNVG_PENTA15') then
            do 130 iel = 1, nbelgr
                ima = zi(liel-1+iel)
                call jeveuo(jexnum(noma//'.CONNEX', ima), 'L', iconex)
                call jelira(jexnum(noma//'.CONNEX', ima), 'LONMAX', nbno, k8bid)
                do 20 ino = 1, nbno
                    zi(itrav+zi(iconex+ino-1)-1) = 1
20              continue
130          continue
        endif
140  end do
!
! --- NOMBRE DE NOEUDS
!
    nbnoc = 0
    do 30 ino = 1, nbnoeu
        if (zi(itrav+ino-1) .eq. 1) then
            nbnoc = nbnoc + 1
        endif
30  end do
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD DDLS EN'//&
        ' ENDOMMAGEMENT AUX NOEUDS: ',nbnoc
    endif
!
! --- RECUPERATION DU NOMBRE D'INCONNUES DU MODELE
!
    call jelira(numedd(1:14)//'.NUME.NUEQ', 'LONUTI', neq, k8bid)
    if (nbnoc .gt. 0) then
        call wkvect(sdnuen, 'V V I', neq, jnuen)
    else
        goto 9999
    endif
!
! --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR DEPL_R
!
    nomgd = 'DEPL_R'
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                k8bid, ier)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx, k8bid)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', iancmp)
    nocmp = 'DAMG'
!
! --- LOCALISATION DE DAMG DANS LA LISTE DES DDLS ASSOCIES
! --- A LA GRANDEUR DEPL_R
!
    idamg = indik8(zk8(iancmp),nocmp,1,ncmpmx)
    call assert(idamg.ne.0)
!
! --- RECUPERATION DU .PRNO ASSOCIE AU MAILLAGE
!
    call jelira(numedd(1:14)//'.NUME.PRNO', 'NMAXOC', nlili, k8bid)
    k = 0
    do 40 i = 1, nlili
        call jenuno(jexnum(numedd(1:14)//'.NUME.LILI', i), nolili)
        if (nolili(1:8) .ne. '&MAILLA ') goto 40
        k = i
40  end do
    call assert(k.ne.0)
!
    call jeveuo(jexnum(numedd(1:14)//'.NUME.PRNO', k), 'L', iaprno)
!
! --- TABLEAU DES NUMEROS D'EQUATIONS
!
    call jeveuo(numedd(1:14)//'.NUME.NUEQ', 'L', ianueq)
!
! --- AFFECTATION DU TABLEAU DES NUMEROS DES INCONNUES
!
    inoc = 0
    do 50 ino = 1, nbnoeu
        if (zi(itrav+ino-1) .eq. 0) goto 50
        inoc = inoc + 1
        ival = zi(iaprno+(ino-1)*(nec+2)+1-1)
        iadg = iaprno+(ino-1)*(nec+2)+3-1
!
        if (exisdg(zi(iadg),idamg)) then
!
            ico = 0
            do 70 i = 1, idamg-1
                if (exisdg(zi(iadg),i)) ico = ico + 1
70          continue
            zi(jnuen+ival+ico-1) = 1
        endif
50  end do
!
! --- MENAGE
    call jedetr('&&NUENDO.NOEUDS')
!
9999  continue
!
    call jedema()
end subroutine
