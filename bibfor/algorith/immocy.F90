subroutine immocy(nomres, ifm)
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
!    P. RICHARD     DATE 14/03/91
!-----------------------------------------------------------------------
!  BUT:  IMPRIMER LES RESULTATS RELATIF A UN CONCEPT MODE_CYCLIC
    implicit none
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DU CONCEPT
! IFM      /I/: UNITE DU FICHIER MESSAGE
!
!
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=8) :: nomres, nomcou, basmod, mailla, intf
    character(len=8) :: droite, gauche, axe
    character(len=24) :: refe, typint, nosec, numint, diamod, freq
    character(len=24) :: cmode, desc
    character(len=1) :: k1bid
    integer :: ifm
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, iam, idiam, j, k, lldesc
    integer :: lldiam, llfre, llmoc, llnin, llnosc, llref, lltyp
    integer :: nbdax, nbddge, nbddr, nbdiam, nbmobt, nbmod, numa
    integer :: numd, numg
    real(kind=8) :: x1, xmodu, xpar
!-----------------------------------------------------------------------
    call jemarq()
!
!
!-------------------INITIALISATION DES NOMS COURANTS--------------------
!
    refe=nomres//'.CYCL_REFE'
    desc=nomres//'.CYCL_DESC'
    typint=nomres//'.CYCL_TYPE'
    nosec=nomres//'.CYCL_NBSC'
    numint=nomres//'.CYCL_NUIN'
    diamod=nomres//'.CYCL_DIAM'
    freq=nomres//'.CYCL_FREQ'
    cmode=nomres//'.CYCL_CMODE'
!
!
!
    call jeveuo(refe, 'L', llref)
    mailla=zk24(llref)
    intf=zk24(llref+1)
    basmod=zk24(llref+2)
    call jeveuo(nosec, 'L', llnosc)
!
!
    write(ifm,*)' '
    write(ifm,*)'----------------------------------------------------'
    write(ifm,*)' '
    write(ifm,*)'                CALC_MODE_CYCL '
    write(ifm,*)' '
    write(ifm,*)'  IMPRESSIONS NIVEAU:  2 '
    write(ifm,*)' '
!
    write(ifm,*) ' '
    write(ifm,*) ' DEFINITION DU SECTEUR'
    write(ifm,*) '----------------------- '
    write(ifm,*) ' '
    write(ifm,*) ' '
    write(ifm,*)'   MAILLAGE: ',mailla
    write(ifm,*) ' '
    write(ifm,*)'   BASE_MODALE: ',basmod
    write(ifm,*)'   INTERF_DYNA: ',intf
    write(ifm,*) ' '
    write(ifm,*) ' '
    write(ifm,*) ' '
!
    call jeveuo(numint, 'L', llnin)
    call jeveuo(typint, 'L', lltyp)
    nomcou=zk8(lltyp)
    numd=zi(llnin)
    numg=zi(llnin+1)
    numa=zi(llnin+2)
    call jenuno(jexnum(intf//'.IDC_NOMS', numd), droite)
    call jenuno(jexnum(intf//'.IDC_NOMS', numg), gauche)
    if (numa .ne. 0) then
        call jenuno(jexnum(intf//'.IDC_NOMS', numa), axe)
    endif
!
!
!
!
    write(ifm,*) ' DEFINITION DE LA LIAISON'
    write(ifm,*) '-------------------------- '
    write(ifm,*) ' '
    write(ifm,*) ' '
    write(ifm,*) '  TYPE DE BASE MODALE: ',nomcou
    write(ifm,*) ' '
    write(ifm,*) '  INTERFACE DROITE: ',droite
    write(ifm,*) '  INTERFACE GAUCHE: ',gauche
    if (numa .ne. 0) then
        write(ifm,*) '  INTERFACE AXE: ',axe
    endif
    write(ifm,*) ' '
    write(ifm,*) ' '
    write(ifm,*) ' '
!
!
!
    call jeveuo(desc, 'L', lldesc)
    nbmod=zi(lldesc)
    nbddr=zi(lldesc+1)
    nbdax=zi(lldesc+2)
    nbddge=nbmod+nbddr+nbdax
!
    call jelira(diamod, 'LONMAX', nbdiam, k1bid)
    nbdiam=nbdiam/2
!
    call jeveuo(freq, 'L', llfre)
    call jeveuo(diamod, 'L', lldiam)
    call jeveuo(cmode, 'L', llmoc)
!
!
    write(ifm,*) '                           RESULTATS MODAUX'
    write(ifm,*) '                          ------------------ '
    write(ifm,*) ' '
    write(ifm,*) ' '
!
    iad=0
!
    do 10 i = 1, nbdiam
        idiam=zi(lldiam+i-1)
        nbmobt=zi(lldiam+nbdiam+i-1)
        write(ifm,*) ' '
        write(ifm,*) ' '
        write(ifm,*) '  MODES A ',idiam,' DIAMETRES NODAUX'
        write(ifm,*) '------------------------------------- '
        write(ifm,*) ' '
        write(ifm,*) ' NUMERO    FREQUENCE(HZ)    '
        write(ifm,*) ' '
        do 20 j = 1, nbmobt
            x1=zr(llfre+iad)
            write(ifm,*)'  ',j,'       ',x1
!
            write(ifm,*)'  '
            iam=(iad*nbddge)+llmoc
            xmodu=0.d0
            do 30 k = 1, nbmod
                xmodu=xmodu+(abs(zc(iam+k-1)))**2
30          continue
            xmodu=xmodu
            do 40 k = 1, nbmod
                xpar=100.d0*(abs(zc(iam+k-1))**2)/xmodu
                write(ifm,*)'                             ',&
     &'PARTICIPATION MODE:',k,' --> ',xpar,' %'
40          continue
            write(ifm,*) ' '
            iad=iad+1
20      continue
        write(ifm,*) ' '
10  end do
!
    call jedema()
end subroutine
