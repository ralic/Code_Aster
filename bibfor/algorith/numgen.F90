subroutine numgen(nugene, modgen)
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
!    P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT:      < NUMEROTATION GENERALISEE >
    implicit none
!
!  DETERMINER LA NUMEROTATION DES DEGRES DE LIBERTE GENERALISES
!   A PARTIR D'UN MODELE GENERALISE
!
!-----------------------------------------------------------------------
!
! NUGENE   /I/: NOM K14 DU NUME_DDL_GENE
! MODGEN   /I/: NOM K8 DU MODELE GENERALISE
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/iunifi.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mgutdm.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
!
!
    integer :: lddelg
    character(len=6) :: pgc
    character(len=8) :: modgen, nomcou, sst1, sst2, kbid
    character(len=14) :: nugene
    character(len=19) :: prgene
    character(len=24) :: defli, fprofl, nomsst
    character(len=24) :: valk
    logical :: assok, pbcone
    character(len=8) :: bid
!
    character(len=1) :: k8bid
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, icomp, icompl, icomps, ifimes
    integer :: j, jrefn, k, lddeeq, lddesc, ldnequ, ldnueq
    integer :: ldorl, ldors, ldprl, ldprs, lldefl, lldesc, llprof
    integer :: ltlia, ltoras, ltssnb, ltsst, nbddl, nblia, nblig
    integer :: nbmod, nbsst, neq, ntail, nuas, nulia, null
    integer :: nult, nusst, nusst1, nusst2, nut, nutarl
!-----------------------------------------------------------------------
    data pgc/'NUMGEN'/
!-----------------------------------------------------------------------
!
    call jemarq()
    ifimes=iunifi('MESSAGE')
!
!-----------------------------------------------------------------------
!
    defli=modgen//'      .MODG.LIDF'
    fprofl=modgen//'      .MODG.LIPR'
    nomsst=modgen//'      .MODG.SSNO'
!
!--------------------CREATION DU .REFN----------------------------------
!                       ET DU DESC
    prgene=nugene//'.NUME'
    call wkvect(prgene//'.REFN', 'G V K24', 4, jrefn)
    zk24(jrefn)=modgen
    zk24(jrefn+1)='DEPL_R'
    call wkvect(prgene//'.DESC', 'G V I', 1, lddesc)
    zi(lddesc)=2
!
!---------------------------DECLARATION JEVEUX--------------------------
!
    call jecreo(prgene//'.LILI', 'G N K8')
    call jeecra(prgene//'.LILI', 'NOMMAX', 2, k8bid)
    call jecroc(jexnom(prgene//'.LILI', '&SOUSSTR'))
    call jecroc(jexnom(prgene//'.LILI', 'LIAISONS'))
!
    call jecrec(prgene//'.PRNO', 'G V I', 'NU', 'DISPERSE', 'VARIABLE',&
                2)
    call jecrec(prgene//'.ORIG', 'G V I', 'NU', 'DISPERSE', 'VARIABLE',&
                2)
!
!----------------------RECUPERATION DES DIMENSIONS PRINCIPALES----------
!
    call jelira(defli, 'NMAXOC', nblia, bid)
    call jelira(nomsst, 'NOMMAX', nbsst, bid)
!
!-----------------------------ECRITURE DIMENSIONS-----------------------
!
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', nbsst*2, ' ')
    call jenonu(jexnom(prgene//'.LILI', 'LIAISONS'), ibid)
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', nblia*4, ' ')
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', nbsst, ' ')
    call jenonu(jexnom(prgene//'.LILI', 'LIAISONS'), ibid)
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', nblia*2, ' ')
!
!----------------------BOUCLES DE COMPTAGE DES DDL----------------------
!
    icomp=0
    icomps=0
    icompl=0
!
!   BOUCLE SUR LES SOUS-STRUCTURES
!
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprs)
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldors)
!
    do 10 i = 1, nbsst
        kbid='        '
        call mgutdm(modgen, kbid, i, 'NOM_MACR_ELEM', ibid,&
                    nomcou)
        zi(ldors+i-1)=i
        call jeveuo(nomcou//'.MAEL_RAID_DESC', 'L', lldesc)
        nbmod=zi(lldesc+1)
        zi(ldprs+(i-1)*2+1)=nbmod
        icomp=icomp+nbmod
        icomps=icomps+nbmod
10  end do
!
!   BOUCLE SUR LES LIAISONS
!   (ON SUPPOSE QUE LES MATRICES DES LIAISONS 1 ET 2 ONT
!   MEME NOMBRE DE LIGNES = VERIF VERILI)
!
    call jenonu(jexnom(prgene//'.LILI', 'LIAISONS'), ibid)
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprl)
    call jenonu(jexnom(prgene//'.LILI', 'LIAISONS'), ibid)
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldorl)
    call jeveuo(fprofl, 'L', llprof)
!
    do 20 i = 1, nblia
        nblig=zi(llprof+(i-1)*9)
        zi(ldorl+(i-1)*2)=i
        zi(ldorl+(i-1)*2+1)=i
        zi(ldprl+(i-1)*4+1)=nblig
        zi(ldprl+(i-1)*4+3)=nblig
        icomp=icomp+2*nblig
        icompl=icompl+2*nblig
20  end do
!
    neq=icomp
!
    write (ifimes,*)'+++ NOMBRE DE SOUS-STRUSTURES: ',nbsst
    write (ifimes,*)'+++ NOMBRE DE LIAISONS: ',nblia
    write (ifimes,*)'+++ NOMBRE TOTAL D''EQUATIONS: ',neq
    write (ifimes,*)'+++ DONT NOMBRE D''EQUATIONS STRUCTURE: ',icomps
    write (ifimes,*)'+++ DONT NOMBRE D''EQUATIONS LIAISON: ',icompl
!
    call wkvect(prgene//'.NEQU', 'G V I', 1, ldnequ)
    zi(ldnequ)=neq
!
!------------------------ALLOCATIONS DIVERSES---------------------------
!
    call wkvect(prgene//'.DEEQ', 'G V I', neq*2, lddeeq)
    call wkvect(prgene//'.NUEQ', 'G V I', neq, ldnueq)
    call wkvect(prgene//'.DELG', 'G V I', neq, lddelg)
!
    call wkvect('&&'//pgc//'.SST.NBLIA', 'V V I', nbsst, ltssnb)
    call wkvect('&&'//pgc//'.LIA.SST', 'V V I', nblia*2, ltlia)
    call jecrec('&&'//pgc//'.SST.LIA', 'V V I', 'NU', 'DISPERSE', 'CONSTANT',&
                nbsst)
    call jeecra('&&'//pgc//'.SST.LIA', 'LONMAX', 2*nblia, ' ')
!
!   BOUCLE DE DETERMINATION DE LA RELATION
!   NUMERO TARDIF  LIAISON --> NUMERO SOUS-STRUCTURE DE PLUS PETIT
!                              NUMERO
!
    do 30 i = 1, nblia*2
        nulia=zi(ldorl+i-1)
        call jeveuo(jexnum(defli, nulia), 'L', lldefl)
        sst1=zk8(lldefl)
        sst2=zk8(lldefl+2)
        call jenonu(jexnom(nomsst, sst1), nusst1)
        call jenonu(jexnom(nomsst, sst2), nusst2)
        zi(ltssnb+nusst1-1)=1
        zi(ltssnb+nusst2-1)=1
        zi(ltlia+i-1)=max(nusst1,nusst2)
30  end do
!
!   BOUCLE PERMETTANT DE DETERMINER L'INVERSE
!   NUMERO TARDIF  SOUS-STRUCTURE --> NUMEROS TARDIF LIAISONS
!                     DONT ELLE EST LA STRUCTURE DE PLUS PETIT NUMERO
!
!   ET POUR DETECTER LES SOUS-STRUCTURES NON CONNECTEES
!
    pbcone=.false.
    do 50 i = 1, nbsst
        icomp=0
        nusst=zi(ldors+i-1)
        if (zi(ltssnb+nusst-1) .eq. 0) then
            pbcone=.true.
            call jenuno(jexnum(nomsst, nusst), sst1)
            valk=sst1
            call u2mesg('E', 'ALGORITH13_75', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
        call jecroc(jexnum('&&'//pgc//'.SST.LIA', i))
        call jeveuo(jexnum('&&'//pgc//'.SST.LIA', i), 'E', ltsst)
        do 40 j = 1, nblia*2
            if (zi(ltlia+j-1) .eq. nusst) then
                icomp=icomp+1
                zi(ltsst+icomp-1)=j
            endif
40      continue
50  end do
!
    if (pbcone) then
        call u2mesg('F', 'ALGORITH13_76', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
    call jedetr('&&'//pgc//'.LIA.SST')
    call jedetr('&&'//pgc//'.SST.NBLIA')
!
!--------------------DETERMINATION DE L'ORDRE D'ASSEMBLAGE--------------
!                            DES NOEUDS TARDIFS
!
    ntail=nbsst+2*nblia
    call wkvect('&&'//pgc//'.ORD.ASS', 'V V I', ntail, ltoras)
!
!   BOUCLE SUR LES SOUS-STRUCTURES
!
    icomp=0
    do 100 i = 1, nbsst
        call jeveuo(jexnum('&&'//pgc//'.SST.LIA', i), 'L', ltsst)
!
!  BOUCLE SUR LES LIAISONS POUR ASSEMBLAGES DES DUALISATION AVANT
!
        do 70 j = 1, nblia*2
            assok=.true.
            nutarl=zi(ltsst+j-1)
            nulia=zi(ldorl+nutarl-1)
            if (nutarl .gt. 0) then
!
!   BOUCLE SUR LES NOEUDS TARDIFS DE LIAISON DE LA SOUS-STRUCTURE
!   COURANTE POUR EVITER LES DOUBLES ASSEMBLAGES
!   (NE PAS ASSEMBLER AVANT CE QUI DOIT L'ETRE APRES)
!
                if (j .ne. 1) then
                    do 60 k = 1, j-1
                        nult=zi(ltsst+k-1)
                        null=zi(ldorl+nult-1)
                        if (null .eq. nulia .and. nult .ne. 0) assok= .false.
60                  continue
                endif
                if (assok) then
                    icomp=icomp+1
                    zi(ltoras+icomp-1)=-nutarl
                endif
            endif
70      continue
!
!  ASSEMBLAGE DE LA SOUS-STRUCTURE COURANTE
!
        icomp=icomp+1
        zi(ltoras+icomp-1)=i
!
!   ASSEMBLAGE DES DUALISATIONS APRES LA SOUS-STRUCTURE COURANTE
!
        do 90 j = 1, nblia*2
            assok=.true.
            nutarl=zi(ltsst+j-1)
            if (nutarl .gt. 0) then
                do 80 k = 1, icomp
                    nut=-zi(ltoras+k-1)
                    if (nut .eq. nutarl) assok=.false.
80              continue
                if (assok) then
                    icomp=icomp+1
                    zi(ltoras+icomp-1)=-nutarl
                endif
            endif
90      continue
100  end do
!
    call jedetr('&&'//pgc//'.SST.LIA')
!
!--------------------REMPLISSAGE DES NUMERO D'EQUATION-----------------
!
    icomp=1
!
    do 120 i = 1, ntail
        nuas=zi(ltoras+i-1)
!
!  CAS DE LA SOUS-STRUCTURE
        if (nuas .gt. 0) then
            nbddl=zi(ldprs+(nuas-1)*2+1)
            zi(ldprs+(nuas-1)*2)=icomp
!
! CAS DE LA LIAISON
        else
            nbddl=zi(ldprl-(nuas+1)*2+1)
            zi(ldprl-(nuas+1)*2)=icomp
        endif
!
        do 110 j = icomp, icomp+nbddl-1
            zi(ldnueq+j-1)=j
            zi(lddelg+j-1)=0
            zi(lddeeq+2*j-1)=nuas
            zi(lddeeq+2*j-2)=j-icomp+1
110      continue
        icomp=icomp+nbddl
120  end do
!
!----------------------SAUVEGARDES DIVERSES-----------------------------
!
    call jedetr('&&'//pgc//'.ORD.ASS')
!
    call jedema()
end subroutine
