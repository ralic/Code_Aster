subroutine nmextr(noma, nomo, sdextz, sdieto, motfac,&
                  nbocc, numreo, ntextr)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit      none
    include       'jeveux.h'
    include 'asterc/getvtx.h'
    include 'asterfort/impfoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmextc.h'
    include 'asterfort/nmextd.h'
    include 'asterfort/nmextf.h'
    include 'asterfort/nmextk.h'
    include 'asterfort/nmextl.h'
    include 'asterfort/nmextn.h'
    include 'asterfort/nmextp.h'
    include 'asterfort/nmextt.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: noma, nomo
    character(len=*) :: sdextz
    character(len=24) :: sdieto
    integer :: numreo, nbocc, ntextr
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - EXTRACTION)
!
! LECTURE DES DONNEES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  SDEXTR : NOM DE LA SD POUR EXTRACTION
! IN  SDIETO : SD GESTION IN ET OUT
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  NBOCC  : NOMBRE D'OCCURRENCES DE MOTFAC
! IN  NUMREO : NUMERO DE REUSE POUR LA TABLE OBSERVATION
! OUT NTEXTR : NOMBRE TOTAL D'EXTRACTIONS
!
! ----------------------------------------------------------------------
!
    integer :: iocc, ioc2, icham, ibid
    integer :: nbext, nbcham
    integer :: nbno, nbma, nbpi, nbspi, nbcmp
    character(len=2) :: chaine
    character(len=24) :: oldcha, nomcha, nomchs, nomchx
    character(len=4) :: typcha
    logical :: lextr, trouve
    character(len=24) :: listno, listma, listpi, listsp, listcp
    character(len=24) :: extinf, extcha, exttyp, extact
    integer :: jextin, jextch, jextty, jextac
    character(len=19) :: champ
    character(len=8) :: extrcp, extrch, extrga
    character(len=24) :: list
    integer :: jlist
    integer :: iarg
    character(len=19) :: sdextr
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ntextr = 0
    list = '&&NMEXTR.LIST'
    nbcham = 0
    sdextr = sdextz
!
! --- SD PRINCIPALE (INFO)
!
    extinf = sdextr(1:14)//'     .INFO'
    call wkvect(extinf, 'V V I', 7*nbocc+4, jextin)
    if (nbocc .eq. 0) goto 99
!
! --- SD TYPE D'EXTRACTION (CHAMP, GAUSS, COMPOSANTES)
!
    exttyp = sdextr(1:14)//'     .EXTR'
    call wkvect(exttyp, 'V V K8', 3*nbocc, jextty)
!
! --- SD ACTIVATION EXTRACTION
!
    extact = sdextr(1:14)//'     .ACTI'
    call wkvect(extact, 'V V L', nbocc, jextac)
!
! --- LISTE DES CHAMPS A EXTRAIRE
!
    call wkvect(list, 'V V K24', nbocc, jlist)
    do 5 iocc = 1, nbocc
!
! ----- CE CHAMP EST-IL OBSERVABLE ?
!
        call nmextc(sdieto, motfac, iocc, nomcha, lextr)
        if (.not.lextr) nomcha = 'NONE'
!
! ----- CE CHAMP EXISTE-T-IL DEJA ?
!
        trouve = .false.
        do 6 ioc2 = 1, nbocc - 1
            oldcha = zk24(jlist-1+ioc2)
            if (oldcha .eq. nomcha) then
                icham = ioc2
                trouve = .true.
            endif
 6      continue
        if (.not.trouve) then
            nbcham = nbcham + 1
            icham = nbcham
            zk24(jlist-1+icham) = nomcha
        endif
        zi(jextin+4+7*(iocc-1)+7-1) = icham
 5  end do
!
! --- SD LISTE DES CHAMPS: CHAMP DE REFERENCE ET CHAMP SIMPLE
!
    extcha = sdextr(1:14)//'     .CHAM'
    call wkvect(extcha, 'V V K24', 2*nbcham, jextch)
    do 20 icham = 1, nbcham
        nomcha = zk24(jlist-1+icham)
        nomchs = nomcha(1:18)//'S'
        zk24(jextch+2*(icham-1)+1-1) = nomcha
        zk24(jextch+2*(icham-1)+2-1) = nomchs
20  end do
!
    do 10 iocc = 1, nbocc
!
        nbext = 0
        extrga = 'NONE'
        extrcp = 'NONE'
        extrch = 'NONE'
!
! ----- GENERATION DU NOM DES SD
!
        call impfoi(0, 2, iocc, chaine)
        listno = sdextr(1:14)//chaine(1:2)//'   .NOEU'
        listma = sdextr(1:14)//chaine(1:2)//'   .MAIL'
        listpi = sdextr(1:14)//chaine(1:2)//'   .POIN'
        listsp = sdextr(1:14)//chaine(1:2)//'   .SSPI'
        listcp = sdextr(1:14)//chaine(1:2)//'   .CMP '
!
! ----- NOM DU CHAMP
!
        icham = zi(jextin+4+7*(iocc-1)+7-1)
        nomcha = zk24(jextch+2*(icham-1)+1-1)
        nomchs = zk24(jextch+2*(icham-1)+2-1)
        if (nomcha .eq. 'NONE') then
            call getvtx(motfac, 'NOM_CHAM', iocc, iarg, 1,&
                        nomchx, ibid)
            call u2mesk('A', 'EXTRACTION_99', 1, nomchx)
            goto 999
        endif
!
! ----- TYPE DU CHAMP (NOEU OU ELGA)
!
        call nmextt(sdieto, nomcha, typcha)
!
! ----- RECUPERATION DU CHAMP TEST POUR VERIF. COMPOSANTES
!
        call nmextd(nomcha, sdieto, champ)
!
! ----- LECTURE DE L'ENDROIT POUR EXTRACTION (MAILLE/NOEUD)
!
        call nmextl(noma, nomo, motfac, iocc, nomcha,&
                    typcha, listno, listma, nbno, nbma,&
                    extrch)
!
! ----- LECTURE INFO. SI CHAM_ELGA/CHAM_ELEM
!
        if (typcha .eq. 'ELGA') then
            call nmextp(motfac, iocc, nomcha, champ, nomchs,&
                        listpi, listsp, nbpi, nbspi, extrga)
        endif
!
! ----- LECTURE ET VERIF DES COMPOSANTES
!
        call nmextk(noma, motfac, iocc, champ, nomcha,&
                    nomchs, typcha, listno, listma, listpi,&
                    listsp, nbno, nbma, nbpi, nbspi,&
                    listcp, nbcmp)
!
! ----- TYPE EXTRACTION SUR LES COMPOSANTES
!
        call nmextf(motfac, iocc, extrcp)
!
! ----- DECOMPTE DES POINTS D'EXTRACTION
!
        call nmextn(typcha, extrcp, extrga, extrch, nbno,&
                    nbma, nbcmp, nbpi, nbspi, nbext)
!
! ----- SAUVEGARDE
!
        zk8(jextty+3*(iocc-1)+1-1) = extrch
        zk8(jextty+3*(iocc-1)+2-1) = extrga
        zk8(jextty+3*(iocc-1)+3-1) = extrcp
        zi(jextin+4+7*(iocc-1)+1-1) = nbcmp
        zi(jextin+4+7*(iocc-1)+2-1) = nbno
        zi(jextin+4+7*(iocc-1)+3-1) = nbma
        zi(jextin+4+7*(iocc-1)+4-1) = nbpi
        zi(jextin+4+7*(iocc-1)+5-1) = nbspi
        zi(jextin+4+7*(iocc-1)+6-1) = nbext
!
999      continue
!
        ntextr = ntextr + nbext
!
10  end do
!
!
! --- DESTRUCTION DES CHAM_ELEM_S
!
    do 45 icham = 1, nbcham
        nomchs = zk24(jextch+2*(icham-1)+2-1)
        call jedetr(nomchs)
45  end do
99  continue
!
! --- INFOS PRINCIPALES
!
    zi(jextin-1+1) = nbocc
    zi(jextin-1+2) = ntextr
    zi(jextin-1+3) = 1
    zi(jextin-1+4) = numreo
!
    call jedetr(list)
    call jedema()
end subroutine
