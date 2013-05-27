subroutine nurota(numedd, compor, sdnuro)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
!
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/etenca.h'
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
    character(len=24) :: numedd, compor
    character(len=24) :: sdnuro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DE LA SD POUR REPERAGE DDL GRANDES ROTATIONS
!
! ----------------------------------------------------------------------
!
!
! CREATION DE LA S.D. DE NOM SDNURO QUI INDIQUE QUELLES SONT LES
! ADRESSES DANS L'OBJET .VALE D'UN CHAM_NO S'APPUYANT SUR NUMEDD
! DES ROTATIONS DRX, DRY, DRZ DES NOEUDS DES MAILLES
!
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  COMPOR : NOM DE LA CARTE COMPOR
! IN  SDNURO : NOM DE LA S.D. NUME_DDL_ROTA
!
!
!
!
    integer :: dg
    character(len=8) :: nocmp, nomgd, modele, noma, k8bid
    character(len=16) :: compt, nomte, deform
    character(len=19) :: ligrmo
    character(len=24) :: nolili, noliel
    integer :: nec, nbma, nbnoeu, ngdmax, ncmpmx
    integer :: nlili, nbno, nequa, nbnoc
    integer :: ier, iret, ibid, ico
    integer :: ima, igd, ino, idebgd, idrz, i, k, inoc, ival, iadg
    integer :: itrav, idesc, ivale, iptma, iconex
    integer :: indro, iancmp, ianueq, iaprno
    integer :: ifm, niv, nbgr, igr, te, nbelgr, liel, iel
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    deform = 'GROT_GDEP'
    nomgd = 'COMPOR  '
!
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                k8bid, ier)
!
    call assert(nec.le.1)
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
! --- NOMBRE DE MAILLES DU MAILLAGE
!
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8bid, ier)
!
! --- NOMBRE DE NOEUDS DU MAILLAGE
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoeu,&
                k8bid, ier)
!
    ligrmo = modele//'.MODELE'
!
! --- CREATION DU TABLEAU DESCRIPTEUR DE LA CARTE COMPOR
!
    call etenca(compor, ligrmo, iret)
    call assert(iret.eq.0)
!
! --- CREATION D'UN VECTEUR DESTINE A CONTENIR LES NUMEROS
! --- DES NOEUDS EN GRANDES ROTATIONS
!
    call wkvect('&&NUROTA.NOEUDS.GR', 'V V I', nbnoeu, itrav)
!
! --- RECUPERATION DE LA GRANDEUR (ICI COMPOR)
! --- REFERENCEE PAR LA CARTE COMPOR
!
    call jeveuo(compor(1:19)//'.DESC', 'L', idesc)
    ngdmax = zi(idesc+2-1)
!
! --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR
!
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx, k8bid)
!
! --- TABLEAU DE VALEURS DE LA CARTE COMPOR
! --- (CONTENANT LES VALEURS DU COMPORTEMENT)
!
    call jeveuo(compor(1:19)//'.VALE', 'L', ivale)
!
! --- RECUPERATION DU VECTEUR D'ADRESSAGE DANS LA CARTE
! --- CREE PAR ETENCA
!
    call jeveuo(compor(1:19)//'.PTMA', 'L', iptma)
!
! --- AFFECTATION DU TABLEAU DES NOEUDS EN GRANDES ROTATIONS
!
    nbgr = nbgrel(ligrmo)
    noliel = ligrmo//'.LIEL'
    do 140 igr = 1, nbgr
        te = typele(ligrmo,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
        if (nomte .eq. 'MECA_POU_D_T_GD' .or. nomte .eq. 'MEC3TR7H' .or. nomte .eq.&
            'MEC3QU9H') then
            nbelgr = nbelem(ligrmo,igr)
            call jeveuo(jexnum(noliel, igr), 'L', liel)
            do 130 iel = 1, nbelgr
                ima = zi(liel-1+iel)
                if (zi(iptma+ima-1) .ne. 0) then
                    igd = zi(iptma+ima-1)
                    idebgd = (igd-1)*ncmpmx
                    dg = zi(idesc+3+2*ngdmax+zi(iptma+ima-1)-1)
!
! ---     ON S'ASSURE QUE LA PREMIERE COMPOSANTE DE LA GRANDEUR
! ---     QUI EST RELCOM A BIEN ETE AFFECTEE
!
                    call assert(exisdg(dg, 1))
! ---     RECUPERATION DU COMPORTEMENT AFFECTE A LA MAILLE
                    compt = zk16(ivale+idebgd+3-1)
                    if (compt .ne. deform) goto 130
! ---     RECUPERATION DES NUMEROS DES NOEUDS DE LA MAILLE
                    call jeveuo(jexnum(noma//'.CONNEX', ima), 'L', iconex)
                    call jelira(jexnum(noma//'.CONNEX', ima), 'LONMAX', nbno, k8bid)
                    do 20 ino = 1, nbno
                        zi(itrav+zi(iconex+ino-1)-1) = 1
20                  continue
                endif
130          continue
        endif
140  end do
!
! --- NOMBRE DE NOEUDS EN GRANDES ROTATIONS
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
        ' GRANDES ROTATIONS: ',nbnoc
    endif
!
! --- CREATION DU TABLEAU DES NUMEROS D'EQUATIONS CORRESPONDANT   ---
! --- AUX DDLS DE ROTATION POUR LES NOEUDS EN GRANDES ROTATIONS   ---
!
! --- RECUPERATION DU NOMBRE D'INCONNUES DU MODELE  ---
    call jelira(numedd(1:14)//'.NUME.NUEQ', 'LONUTI', nequa, k8bid)
    if (nbnoc .gt. 0) then
        call wkvect(sdnuro, 'V V I', nequa, indro)
    else
        goto 9999
    endif
!
    nomgd = 'DEPL_R'
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                k8bid, ier)
!
! --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR DEPL_R ---
!
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx, k8bid)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', iancmp)
!
    nocmp = 'DRZ'
!
! --- LOCALISATION DE DRZ DANS LA LISTE DES DDLS ASSOCIES  ---
! ---  A LA GRANDEUR DEPL_R                                ---
!
    idrz = indik8(zk8(iancmp),nocmp,1,ncmpmx)
    if (idrz .eq. 0) then
        call assert(.false.)
    endif
!
! --- RECUPERATION DU .PRNO ASSOCIE AU MAILLAGE  ---
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
! --- TABLEAU DES NUMEROS D'EQUATIONS  ---
!
    call jeveuo(numedd(1:14)//'.NUME.NUEQ', 'L', ianueq)
!
! --- AFFECTATION DU TABLEAU DES NUMEROS DES INCONNUES ROTATIONS  ---
! --- DES NOEUDS EN GRANDES ROTATIONS                             ---
!
    inoc = 0
    do 50 ino = 1, nbnoeu
        if (zi(itrav+ino-1) .eq. 0) goto 50
        inoc = inoc + 1
! ---  IVAL  : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
        ival = zi(iaprno+(ino-1)*(nec+2)+1-1)
! ---  NCMP  : NOMBRE DE COMPOSANTES SUR LE NOEUD INO
! ---  IADG  : DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD INO
        iadg = iaprno+(ino-1)*(nec+2)+3-1
!
        do 60 i = idrz-2, idrz
            if (.not.exisdg(zi(iadg),i)) then
                call assert(.false.)
            endif
60      continue
        ico = 0
        do 70 i = 1, idrz-3
            if (exisdg(zi(iadg),i)) then
                ico = ico + 1
            endif
70      continue
!
        zi(indro-1+ival-1+ico+1) = 1
        zi(indro-1+ival-1+ico+2) = 1
        zi(indro-1+ival-1+ico+3) = 1
!
50  end do
!
! --- MENAGE
    call jedetr('&&NUROTA.NOEUDS.GR')
!
9999  continue
!
    call jedema()
end subroutine
