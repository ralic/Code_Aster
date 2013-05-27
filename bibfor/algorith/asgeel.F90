subroutine asgeel(nomres, option, nugene)
!
    implicit none
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
!***********************************************************************
!    M. CORUS     DATE 26/02/10
!-----------------------------------------------------------------------
!  BUT:      < ASSEMBLAGE GENERALISEE DE MATRICE PLEINE ET ELIMINATION >
!
!  ASSEMBLER UNE MATRICE A PARTIR D'UNE NUMEROTATION GENERALISEE
!  ET D'UNE OPTION (RIGI_GENE,MASS_GENE,AMOR_GENE)
!
! REMARQUE : L'ASSEMBLAGE DONNE UNE MATRICE ASSEMBLEE PLEINE
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM K8 DE LA MATRICE GENERALISEE RESULTAT
! OPTION   /I/: OPTION DE CALCUL (RIGI_GENE,MASS_GENE)
! NUGENE   /I/: NOM K14 DE LA NUMEROTATION GENERALISEE
! STOPLE   /I/: NOM K19 DU STOCKAGE DE LA MATRICE (PLEIN)
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mgutdm.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
!
!
    character(len=8) :: nomres, modgen
    character(len=14) :: nugene
    character(len=19) :: prgene
    character(len=11) :: option
    character(len=24) :: k24bid, seliai, sizlia, sst, nomsst
    real(kind=8) :: temp
!
    integer :: ibid, i1, j1, k1, l1, n1, lres, neq, lneq, lproj, nbddl, indsst
    integer :: lsst, llref, nbmacr, exist, lnomcr, limacr, lmacr, llmacr, indmcr
    integer :: decal, ddlcp, lselia, nlt, ind, ldconl, iret, nmaddl, ltemp
    integer :: jrefa, ldlim, nbsst, lsilia
    character(len=8) :: kbid, k8bid
    character(len=1) :: k1bid
!-----------C
!--       --C
!-- DEBUT --C
!--       --C
!-----------C
!
    call jemarq()
!
!----------------------------------C
!--                              --C
!-- INITIALISATION DE LA MATRICE --C
!--                              --C
!----------------------------------C
!
    prgene=nugene//'.NUME'
!
!
    call wkvect(nomres//'           .REFA', 'G V K24', 11, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1)=' '
    zk24(jrefa-1+2)=nugene
    zk24(jrefa-1+8) = 'ASSE'
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'GENE'
!
!--------------------RECUPERATION DU MODE_GENE AMONT--------------------
!
    k24bid=prgene//'.REFN'
    call jeveuo(k24bid, 'L', llref)
    modgen=zk24(llref)(1:8)
    nomsst=modgen//'      .MODG.SSNO'
!
!--------------------------CREATION DU .LIME----------------------------
!   POUR L'INSTANT ON DONNE LE NOM DU MODELE GENERALISE
!
    call wkvect(nomres//'           .LIME', 'G V K24', 1, ldlim)
    zk24(ldlim)=modgen
!      CALL WKVECT(NOMRES//'           .DESC','G V I',1,LDDESC)
!      ZI(LDDESC)=2
!
!
!------------------RECUPERATION DU NOMBRE DE SOUS-STRUCTURE-------------
!
    call jelira(nomsst, 'NOMMAX', nbsst, k1bid)
!
!--
!
!      SELIAI= '&&'//NUGENE(1:8)//'PROJ_EQ_LIAI'
!      SIZLIA='&&'//NUGENE(1:8)//'VECT_SIZE_SS'
!      SST=    '&&'//NUGENE(1:8)//'VECT_NOM_SS'
    seliai=nugene(1:14)//'.ELIM.BASE'
    sizlia=nugene(1:14)//'.ELIM.TAIL'
    sst=   nugene(1:14)//'.ELIM.NOMS'
!
    k24bid=prgene//'.NEQU'
    call jeveuo(k24bid, 'L', lneq)
    neq=zi(lneq)
!
    call wkvect(nomres//'           .CONL', 'G V R', neq, ldconl)
    do 10 i1 = 1, neq
        zr(ldconl+i1-1)=1.d0
10  end do
!
!-- RECUPERATION DES DIFFERENTS MACRO ELEMENTS
!
    call wkvect('&&ASGEEL.NOMS_MACRO', 'V V K8', nbsst, lnomcr)
    call wkvect('&&ASGEEL.INDICES_MACRO', 'V V I', nbsst*3, limacr)
!
    call jeveuo(sizlia, 'L', lsilia)
    call jeveuo(sst, 'L', lsst)
!
    ddlcp=0
    nlt=0
    nmaddl=0
    do 20 i1 = 1, nbsst
        nbddl=zi(lsilia+i1-1)
        if (nbddl .gt. nmaddl) then
            nmaddl=nbddl
        endif
        nlt=nlt+nbddl
        ddlcp=ddlcp+((nbddl*(nbddl+1))/2)
        kbid='        '
        indsst=i1
        call jenonu(jexnom(nomsst, zk8(lsst+i1-1)), indsst)
        call mgutdm(modgen, kbid, indsst, 'NOM_MACR_ELEM', ibid,&
                    k8bid)
!
        if (i1 .eq. 1) then
            nbmacr=1
            zi(limacr)=1
            zi(limacr+nbsst)=nbddl
            zi(limacr+2*nbsst)=nlt-nbddl
            zk8(lnomcr)=k8bid
        else
            exist=0
            do 30 j1 = 1, nbmacr
                if (zk8(lnomcr+j1-1) .eq. k8bid) then
                    exist=1
                    zi(limacr+i1-1)=j1
                    zi(limacr+nbsst+i1-1)=nbddl
                    zi(limacr+2*nbsst+i1-1)=nlt-nbddl
                endif
30          continue
            if (exist .eq. 0) then
                nbmacr=nbmacr+1
                zi(limacr+i1-1)=nbmacr
                zi(limacr+nbsst+i1-1)=nbddl
                zi(limacr+2*nbsst+i1-1)=nlt-nbddl
                zk8(lnomcr+nbmacr-1)=k8bid
            endif
        endif
20  end do
!
!
!-- RECUPERATION DES MATRICES DES DIFERENTS MACROS ELEMENTS
!
    call wkvect('&&ASGEEL.POINTEURS_MACRO', 'V V I', nbmacr, llmacr)
    do 40 i1 = 1, nbmacr
        if (option .eq. 'RIGI_GENE') then
            call jeveuo(zk8(lnomcr+i1-1)//'.MAEL_RAID_VALE', 'L', lmacr)
            k1bid='K'
        else if (option .eq. 'MASS_GENE') then
            call jeveuo(zk8(lnomcr+i1-1)//'.MAEL_MASS_VALE', 'L', lmacr)
            k1bid='M'
        else if (option .eq. 'AMOR_GENE') then
            call jeveuo(zk8(lnomcr+i1-1)//'.MAEL_AMOR_VALE', 'L', lmacr)
        endif
        zi(llmacr+i1-1)=lmacr
40  end do
!
!-- .VALM NE DOIT PAS EXISTER :
    call jeexin(nomres//'           .VALM', iret)
    call assert(iret.eq.0)
!
!-- ALLOCATION DE LA MATRICE PROJETEE
    call jecrec(nomres//'           .VALM', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                1)
    call jecroc(jexnum(nomres//'           .VALM', 1))
    call jeecra(jexnum(nomres//'           .VALM', 1), 'LONMAX', int((neq*(neq+1))/2), ' ')
    call jeveuo(jexnum(nomres//'           .VALM', 1), 'E', lres)
!
!----------------------------------------C
!--                                    --C
!-- REMPLISSAGE DE LA MATRICE PROJETEE --C
!--                                    --C
!----------------------------------------C
!
    call jeveuo(seliai, 'L', lselia)
!
    call wkvect('&&ASGEEL.MATR_TEMP', 'V V R', nmaddl**2, ltemp)
    call wkvect('&&ASGEEL.PROJ_TEMP', 'V V R', nmaddl*neq, lproj)
!
    do 50 n1 = 1, nbsst
        indmcr=zi(limacr+n1-1)
        nbddl=zi(limacr+nbsst+n1-1)
        decal=zi(limacr+2*nbsst+n1-1)
        lmacr=zi(llmacr+indmcr-1)
!
!-- ON RECOPIE LA MATRICE DU MACRO ELEMENT (STOCKAGE PLEIN)
        do 60 j1 = 1, int(nbddl*(nbddl+1)/2)
            temp=(sqrt(1.d0+8.d0*j1)-1.d0)/2.d0
            l1=int(temp)
            if (temp .gt. l1) then
                l1=l1+1
            endif
            k1=j1-int(l1*(l1-1)/2)
            zr(ltemp+nbddl*(k1-1)+l1-1)=zr(lmacr+j1-1)
            zr(ltemp+nbddl*(l1-1)+k1-1)=zr(lmacr+j1-1)
60      continue
!
!-- ON FAIT K*T
        do 70 i1 = 1, nbddl
            do 80 j1 = 1, neq
                zr(lproj+(j1-1)*nbddl+i1-1)=ddot(nbddl, zr(ltemp+(i1-&
                1)*nbddl),1, zr(lselia+(j1-1)*nlt+decal),1)
80          continue
70      continue
!
!-- ON FAIT T^T*(K*T)
        do 90 j1 = 1, neq
            do 100 i1 = 1, j1
                ind=int(((j1-1)*j1)/2)+i1-1
                zr(lres+ind)=zr(lres+ind)+ddot(nbddl, zr(lproj+(j1-1)*&
                nbddl),1, zr(lselia+(i1-1)*nlt+decal),1)
100          continue
90      continue
!
50  end do
!
    call jedetr('&&ASGEEL.POINTEURS_MACRO')
    call jedetr('&&ASGEEL.NOMS_MACRO')
    call jedetr('&&ASGEEL.INDICES_MACRO')
    call jedetr('&&ASGEEL.MATR_TEMP')
    call jedetr('&&ASGEEL.PROJ_TEMP')
!
!---------C
!--     --C
!-- FIN --C
!--     --C
!---------C
!      CALL MATIMP(NOMRES//'           ',8,'MATLAB')
    call jedema()
!
end subroutine
