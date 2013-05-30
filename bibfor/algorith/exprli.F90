subroutine exprli(basmdz, lintfz, nmintz, numint, famprz,&
                  ii, ordo)
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
!***********************************************************************
!    P. RICHARD     DATE 23/05/91
!-----------------------------------------------------------------------
!  BUT:  < DETERMINATION PROFNO  INTERFACE >
    implicit none
!
!  CONSISTE A DETERMINER UN MINI PROFNO POUR UNE INTERFACE
!
!  PROFLI(1,I)=NUMERO DU PREMIER DDL DU IEME NOEUD DE
!              L'INTERFACE DANS LA MATRICE DE LIAISON
!               (1 DDL = 1 LIGNE)
!  PROFLI(2,I)= ENTIER CODE DES TYPE DDL ACTIF AU NOEUD DANS
!              L'INTERFACE
!
! C'EST A DIRE POUR CHAQUE NOEUDS DE L'INTERFACE LE RANG DE SON PREMIER
!  DDL ACTIF DANS LA MATRICE DE LIAISON ET L'ENTIER CODE DES DDL ACTIFS
! A LA LIAISON
!
!-----------------------------------------------------------------------
!
! BASMDZ   /I/: NOM UT DE LA BASE_MODALE
! LINTFZ   /I/: NOM UT DE LA LIST_INTERFACE
! NMINTZ   /I/: NOM DE L'INTERFACE
! NUMINT   /I/: NUMERO DE L'INTERFACE
! FAMPRZ   /I/: FAMILLE DES MINI-PROFNO
! II       /I/: NUMERO DU PROFNO A CREER DANS LA FAMILLE
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/bmnoin.h'
    include 'asterfort/bmrdda.h'
    include 'asterfort/codent.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/isdeco.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
!
!
    integer :: idec(30), llint4, nbcmpm, llref, nbec, ierd, nbcmp, nbddl, ordo
    integer :: ii, llact, iec, ldmap, numint, nbdef, ibid, nbnoe, icomp, i, j
    integer :: ier
    parameter   (nbcmpm=10)
    character(len=*) :: basmdz, nmintz, lintfz, famprz
    character(len=1) :: k1bid
    character(len=4) :: nliai
    character(len=8) :: basmod, nomint, lintf, kbid, blanc, nomg, temp
    character(len=24) :: ordod
    character(len=32) :: famprl
!
!-----------------------------------------------------------------------
    data blanc /'        '/
!-----------------------------------------------------------------------
!
!-------------RECUPERATION LIST_INTERFACE AMONT SI BASE MODALE----------
!
    call jemarq()
!
    basmod = basmdz
    nomint = nmintz
    lintf = lintfz
    famprl = famprz
!
    if (basmod .ne. blanc) then
        call jeveuo(basmod//'           .REFD', 'L', llref)
        lintf=zk24(llref+4)
    endif
!
!-----RECUPERATION DU NOMBRE DU NOMBRE D'ENTIERS CODES ASSOCIE A DEPL_R
!
    nomg = 'DEPL_R'
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                kbid, ierd)
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    endif
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
!
!----------------RECUPERATION EVENTUELLE DU NUMERO INTERFACE------------
!
    if (nomint .ne. '             ') then
        call jenonu(jexnom(lintf//'.IDC_NOMS', nomint), numint)
    endif
!
!----------------RECUPERATION DU NOMBRE DE DDL GENERALISES--------------
!
    call dismoi('F', 'NB_MODES_TOT', basmod, 'RESULTAT', nbdef,&
                kbid, ier)
!
!----RECUPERATION DU NOMBRE DE DDL  ET NOEUDS ASSOCIES A L'INTERFACE----
!
    kbid=' '
    call bmrdda(basmod, kbid, nomint, numint, 0,&
                ibid, nbddl, ordo, ii)
    kbid=' '
    call bmnoin(basmod, kbid, nomint, numint, 0,&
                ibid, nbnoe)
!
!-------ALLOCATION DU MINI PROFNO LIAISON INTERFACE COURANTE------------
!
    call jeecra(jexnum(famprl, ii), 'LONMAX', nbnoe*(1+nbec), ' ')
    call jeveuo(jexnum(famprl, ii), 'E', ldmap)
!
!--------------------------DETERMINATION DU PRNO------------------------
!
    call jeveuo(jexnum(lintf//'.IDC_DDAC', numint), 'L', llact)
!
    icomp=0
    do 10 i = 1, nbnoe
        do 20 iec = 1, nbec
            if (ordo .eq. 0) then
                zi(ldmap+(1+nbec)*(i-1)+iec)=zi(llact+(i-1)*nbec+iec-&
                1)
            else
                temp='&&OP0126'
                call codent(ii, 'D', nliai)
                ordod=temp//'      .LDAC.'//nliai
                call jeveuo(ordod, 'L', llint4)
                zi(ldmap+(1+nbec)*(i-1)+iec)=zi(llint4+(i-1)*nbec+iec-&
                1)
            endif
20      continue
        zi(ldmap+(1+nbec)*(i-1))=icomp+1
        if (ordo .eq. 0) then
            call isdeco(zi(llact+(i-1)*nbec+1-1), idec, nbcmpm)
        else
            temp='&&OP0126'
            call codent(ii, 'D', nliai)
            ordod=temp//'      .LDAC.'//nliai
            call jeveuo(ordod, 'L', llint4)
            call isdeco(zi(llint4+(i-1)*nbec+1-1), idec, nbcmpm)
        endif
        do 30 j = 1, 6
            icomp=icomp+idec(j)
30      continue
10  end do
!
    call jedema()
end subroutine
