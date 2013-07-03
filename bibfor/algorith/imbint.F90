subroutine imbint(nomres, ifm)
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
!    P. RICHARD     DATE 21/02/1991
!-----------------------------------------------------------------------
!  BUT:  IMPRIMER LES RESULTATS RELATIFS A LA BASE MODALE
    implicit none
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM DU CONCEPT RESULTAT
! MAILLA   /I/: NOM DU MAILLA
! IFM      /I/: UMITE DU FICHIER MESSAGE
!
!
!
!
!
#include "jeveux.h"
!
#include "asterfort/bmnodi.h"
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
!-----------------------------------------------------------------------
    integer :: i, ibid, idau, idcb, idda, idha, idmn
    integer :: ifau, ifcb, ifha, ifmn, ino, ipoin, iret
    integer :: ityp, j, k, llact, lldes, lldesc, llncmp
    integer :: llnoe, lltyp, nbcmp, nbcpmx, nbdef, nbec, nbint
    integer :: nbno, nbnot, ncomp, numgd
!-----------------------------------------------------------------------
    parameter    (nbcpmx=300)
    character(len=1) :: k1bid
    character(len=8) :: nomcou, typcou, nomnoe, typ, typbas(3), nomtyp
    character(len=8) :: nomres, mailla, flec, craigb, mneal, aucun, cbharm
    character(len=8) :: k8bid
    character(len=16) :: tydef
    character(len=11) :: dactif
    character(len=24) :: nomint, typint, noeint, desdef, ddact
    character(len=80) :: chaine
    integer :: idec(nbcpmx), ifm
!
    integer :: ibid1
    data ibid1/0/
!
!-----------------------------------------------------------------------
    data mneal,craigb,aucun,flec /'MNEAL','CRAIGB','AUCUN','--->'/
    data cbharm /'CB_HARMO'/
    data dactif /'DDL_ACTIF: '/
    data typbas / 'CONNUE','CYCLIQUE','RITZ'/
!-----------------------------------------------------------------------
!
    call jemarq()
!
    write(ifm,*)' '
    write(ifm,*)'----------------------------------------------------'
    write(ifm,*)' '
    write(ifm,*)'                DEFI_INTERF_DYNA '
    write(ifm,*)' '
    write(ifm,*)'  IMPRESSIONS NIVEAU: 2 '
    write(ifm,*)' '
!
!--------------RECUPERATION DU NOM DU MAILLA--------------------------
!
    call dismoi('F', 'NOM_MAILLA', nomres, 'INTERF_DYNA', ibid,&
                mailla, ibid1)
!
!--------------RECUPERATION TYPE LIST_INTERFACE-------------------------
!
    call jeveuo(nomres//'.IDC_DESC', 'L', lldesc)
    ityp=zi(lldesc)
    call jelibe(nomres//'.IDC_DESC')
!
    nomtyp=typbas(ityp)
!
!----RECUPERATION DES DONNEES RELATIVES A LA GRANDEUR SOUS-JACENTE------
!
    call dismoi('F', 'NB_CMP_MAX', nomres, 'INTERF_DYNA', nbcmp,&
                k8bid, iret)
    call dismoi('F', 'NB_EC', nomres, 'INTERF_DYNA', nbec,&
                k8bid, iret)
    call dismoi('F', 'NUM_GD', nomres, 'INTERF_DYNA', numgd,&
                k8bid, iret)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', llncmp)
!
!--------------------INITIALISATION DES MOTS USUELS---------------------
!
    desdef=nomres//'.IDC_DEFO'
    nomint=nomres//'.IDC_NOMS'
    typint=nomres//'.IDC_TYPE'
    noeint=nomres//'.IDC_LINO'
    ddact=nomres//'.IDC_DDAC'
!
    call jeveuo(typint, 'L', lltyp)
    call jeveuo(desdef, 'L', lldes)
    call jelira(nomint, 'NOMMAX', nbint, k1bid)
    call jelira(desdef, 'LONMAX', nbnot, k1bid)
    nbnot=nbnot/(2+nbec)
!
    idau=nbnot+1
    idmn=nbnot+1
    idcb=nbnot+1
    idha=nbnot+1
    ifcb=0
    ifmn=0
    ifau=0
    ifha=0
!
    write(ifm,*) ' '
    write(ifm,*) ' NOM DE L'' INTERF_DYNA: ',nomres
    write(ifm,*) '-------------------------- '
!
    write(ifm,*) ' '
    write(ifm,*) ' TYPE : ',nomtyp
    write(ifm,*) '------ '
!
    write(ifm,*) ' '
    write(ifm,*) ' DEFINITION DES INTERFACES'
    write(ifm,*) '--------------------------- '
!
!  BOUCLE SUR LES INTERFACES
!
    do 10 i = 1, nbint
        write(ifm,*) ' '
        write(ifm,*) ' '
        typcou=zk8(lltyp+i-1)
        call jeveuo(jexnum(ddact, i), 'L', llact)
        call jenuno(jexnum(nomint, i), nomcou)
        call jeveuo(jexnum(noeint, i), 'L', llnoe)
        call jelira(jexnum(noeint, i), 'LONMAX', nbno, k1bid)
        write(ifm,*)' INTERFACE: ',nomcou
        write(ifm,*) '---------- '
        write(ifm,*)'              TYPE: ',typcou
        write(ifm,*) ' '
        write(ifm,*) ' LISTE DES NOEUDS:  NOMBRE: ',nbno
        write(ifm,*) ' '
!
!  BOUCLE SUR LES NOEUDS DE L'INTERFACE COURANTE
!
        do 20 j = 1, nbno
            ipoin=zi(llnoe+j-1)
            call isdeco(zi(llact+(j-1)*nbec+1-1), idec, nbcmp)
            idda=1
            do 25 k = 1, nbcmp
                if (idec(k) .gt. 0) then
                    chaine(idda:idda+7)=zk8(llncmp+k-1)
                    idda=idda+8
                endif
25          continue
            idda=idda-1
            ino=zi(lldes+ipoin-1)
            call jenuno(jexnum(mailla//'.NOMNOE', ino), nomnoe)
            if (idda .lt. 1) then
                write(ifm,*)'NOEUD: ',j,flec,nomnoe,' ',dactif,&
                'PAS DE DDL ACTIF'
                goto 100
            endif
            write(ifm,*)'NOEUD: ',j,flec,nomnoe,' ',dactif, chaine(1:&
            idda)
100          continue
!
!  STOCKAGE DU NUMERO DU PREMIER ET DERNIER NOEUD DE CHAQUE TYPE
!            D'INTERFACE
!
            if (typcou .eq. mneal) then
                idmn=min(idmn,ipoin)
                ifmn=max(ifmn,ipoin)
            endif
!
            if (typcou .eq. craigb) then
                idcb=min(idcb,ipoin)
                ifcb=max(ifcb,ipoin)
            endif
!
            if (typcou .eq. cbharm) then
                idha=min(idha,ipoin)
                ifha=max(ifha,ipoin)
            endif
!
            if (typcou .eq. aucun) then
                idau=min(idau,ipoin)
                ifau=max(ifau,ipoin)
            endif
!
20      continue
        write(ifm,*)'  '
        call bmnodi('        ', nomres, '         ', i, 0,&
                    ibid, nbdef)
        write(ifm,*)'  '
        write(ifm,*)' NOMBRE DE DEFORMEES STATIQUES ASSOCIES: ',nbdef
        write(ifm,*)'  '
        call jelibe(jexnum(ddact, i))
        call jelibe(jexnum(noeint, i))
10  end do
!
    write(ifm,*) ' '
    write(ifm,*) ' '
    write(ifm,*) ' '
    write(ifm,*) ' DEFINITION DES DEFORMEES A CALCULER'
    write(ifm,*) '------------------------------------'
!
!  CAS OU IL Y A DES DEFORMEES STATIQUES
!
    if (idau .eq. 1) then
        write(ifm,*)' PAS DE DEFORMEES STATIQUES A CALCULER'
        goto 9999
    endif
    write(ifm,*) ' '
    ncomp=0
    do 40 i = 1, nbnot
        write(ifm,*) ' '
        if (i .ge. idmn .and. i .le. ifmn) tydef='MODE D''ATTACHE'
        if (i .ge. idcb .and. i .le. ifcb) tydef='MODE CONTRAINT'
        if (i .ge. idha .and. i .le. ifha) tydef='MODE CONT-HARM'
        ino=zi(lldes+i-1)
        call jenuno(jexnum(mailla//'.NOMNOE', ino), nomnoe)
        call isdeco(zi(lldes+nbnot*2+(i-1)*nbec+1-1), idec, nbcmp)
        do 50 j = 1, nbcmp
            if (idec(j) .gt. 0) then
                typ=zk8(llncmp+j-1)
                ncomp=ncomp+1
                write(ifm,*)'DEFORMEE: ',ncomp,flec,nomnoe,' ',typ,' ',tydef
            endif
50      continue
40  continue
!
    write(ifm,*)' '
    write(ifm,*)'----------------------------------------------------'
    write(ifm,*)' '
!
9999  continue
    call jedema()
end subroutine
