subroutine conint(nume, raide, coint, sizeco, connec,&
                  noddli, nnoint, nume91, raiint, ssami)
    implicit none
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
!-----------------------------------------------------------------------
!    M. CORUS     DATE 7/03/10
!-----------------------------------------------------------------------
!
!  BUT:      < DETERMINER LA CONNECTIVITE DES NOEUDS D'INTERFACE >
!
!  ON RECONSTRUIT UNE CONNECTIVITE DES NOEUDS A L'INTERFACE A PARTIR
!  DE LA MATRICE DE RAIDEUR ASSEMBLEE DU MODELE SOUS JACENT. ON ASSEMBLE
!  ENSUITE UN MODELE CONSTRUIT SUR LA BASE D'UN TREILLIS DE POUTRES AVEC
!  LA MEME CONNECTIVITE.
!
!-----------------------------------------------------------------------
!  NUME      /I/ : NOM DU NUME_DDL
!  RAIDE     /I/ : NOM DE LA MATRICE DE RAIDEUR
!  COINT   /I/ : NOM DE LA MATRICE DE CONNECTIVITE
!  SIZECO  /I/ : NB DE LIGNE DE LA MATRICE DE CONNECTIVITE PRE ALLOUEE
!  CONNEC    /O/ : NOMBRE DE CONNECTIONS
!  NODDLI  /I/ : NOM DU VECTEUR CONTENANT LES NOEUD ET LES DDL
!                    D'INTERFACE
!  NNOINT   /I/ : NOMBRE DE NOEUDS D'INTERFACE
!  NUME91    /O/ : NUME_DDL_GENE DES OPERATEURS D'INTERFACE
!  RAIINT   /O/ : MATRICE DE RAIDEUR DU MODELE D'INTERFACE
!  SSAMI   /O/ : MATRICE DE MASSE DU MODELE D'INTERFACE
!
!     ------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterc/getran.h'
    include 'asterfort/crsint.h'
    include 'asterfort/crsolv.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/haslib.h'
    include 'asterfort/inmain.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/matint.h'
    include 'asterfort/wkvect.h'
!
!-- VARIABLES EN ENTREES / SORTIE
    integer :: sizeco, connec, nnoint
    character(len=14) :: nume, nume91
    character(len=24) :: coint, noddli
    character(len=19) :: raide
!
!-- VARIABLES DE LA ROUTINE
    integer :: ibid, ierd, i1, j1, k1, l1, m1, n1, lraide, lsmdi, lsmhc, neq
    integer :: lprno, lnddli, ipos1, ipos2, noeu, nbec, icon1, icon2, noeuco
    integer :: numno, lconnc, lrefn, ldprs, ldors, lddeeq, lipos, ldnueq, lddelg
    integer :: neqddl, nozero, no1, no2, lindno, indeq, ismhc, indddl, neqd2
    integer :: nbvois, iret, nbvmax, lcoord, lraint, lmaint
    real(kind=8) :: rayon, dist, mindis, maxdis, kr(12, 12), mr(12, 12)
    real(kind=8) :: direc(3), ptref(3), temp, long, vtest(3)
    character(len=8) :: k8bid, nomma
    character(len=19) :: prgene, prno, raiint, ssami, solveu
    character(len=24) :: repsst, nommcl
!
!-----------C
!--       --C
!-- DEBUT --C
!--       --C
!-----------C
!
    call jemarq()
!
!--------------------------------C
!--                            --C
!-- INITIALISATION DU NUME_DDL --C
!--                            --C
!--------------------------------C
!
!
!--------------------CREATION DU .REFN----------------------------------
!                       ET DU DESC
    prgene=nume91//'.NUME'
    call wkvect(prgene//'.REFN', 'V V K24', 4, lrefn)
    zk24(lrefn)='&&MODL91'
    zk24(lrefn+1)='DEPL_R'
    call wkvect(prgene//'.DESC', 'V V I', 1, ibid)
    zi(ibid)=2
!
!-- CREATION D'UN MODELE_GENE MINIMALISTE
    call wkvect(prgene//'.REFE', 'V V K24', 4, ibid)
    zk24(ibid)='&&MODL91'
!-- ET ON REMPLIT AVEC JUSTE LES INFOS UTILES POUR RGNDAS.F
    repsst='&&MODL91      .MODG.SSNO'
    nommcl='&&MODL91      .MODG.SSME'
    call jecreo(repsst, 'V N K8')
    call jeecra(repsst, 'NOMMAX', 1, ' ')
    call jecrec(nommcl, 'V V K8', 'NU', 'CONTIG', 'CONSTANT',&
                1)
    call jecroc(jexnum(nommcl, 1))
    call jeecra(nommcl, 'LONT', 1, ' ')
    call jecroc(jexnom(repsst, 'MODLINTF'))
    call jenonu(jexnom(repsst, 'MODLINTF'), ibid)
    call jeveuo(jexnum(nommcl, 1), 'E', ibid)
    zk8(ibid)='MODLINTF'
!-- FIN DU MODELE_GENE
!
    call jecreo(prgene//'.LILI', 'V N K8')
    call jeecra(prgene//'.LILI', 'NOMMAX', 1, k8bid)
    call jecroc(jexnom(prgene//'.LILI', '&SOUSSTR'))
    call jecrec(prgene//'.PRNO', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                1)
    call jecrec(prgene//'.ORIG', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                1)
!
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', 2, ' ')
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', 1, ' ')
!
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprs)
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldors)
!
    zi(ldors)=1
    zi(ldprs)=1
!
!-------------------------------------C
!--                                 --C
!-- CONSTRUCTION DE LA CONNECTIVITE --C
!--                                 --C
!-------------------------------------C
!
!-- RECUPERATION DE LA MATRICE DE RAIDEUR
!
    call jeveuo(jexnum(raide//'.VALM', 1), 'L', lraide)
    call jeveuo(nume//'.SMOS.SMDI', 'L', lsmdi)
    call jeveuo(nume//'.SMOS.SMHC', 'L', lsmhc)
    call dismoi('F', 'PROF_CHNO', nume, 'NUME_DDL', ibid,&
                prno, ierd)
    call jeveuo(jexnum(prno//'.PRNO', 1), 'L', lprno)
!
!-- BOUCLE SUR LES NOEUDS D'INTERFACE
!
    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nbec,&
                k8bid, ibid)
    call jeveuo(noddli, 'L', lnddli)
    call jeveuo(coint, 'E', lconnc)
    connec=0
    neq=0
    nozero=0
    nbvmax=0
    do 10 i1 = 2, nnoint
        l1=nnoint-i1+1
        noeu=zi(lnddli+l1)
        nbvois=0
        ipos2=zi(lprno+(noeu-1)*(2+nbec))
        ipos1=ipos2-1
        neqddl=zi(lprno+(noeu-1)*(2+nbec)+1)
        neq=neq+neqddl
        nozero=nozero+int((neqddl*(neqddl+1))/2)
!
        do 20 j1 = 1, nnoint-i1+1
            noeuco=zi(lnddli+j1-1)
            icon1=zi(lprno+(noeuco-1)*(2+nbec))
            icon2=icon1+zi(lprno+(noeuco-1)*(2+nbec)+1)-1
            numno=0
!
            do 30 k1 = zi(lsmdi+ipos1), zi(lsmdi+ipos2)
                if ((zi4(lsmhc+k1-1) .ge. icon1) .and. (zi4(lsmhc+k1- 1) .le. icon2) .and.&
                    (numno .eq. 0)) then
                    nbvois=nbvois+1
                    connec=connec+1
                    zi(lconnc+l1+nnoint*nbvois)=noeuco
                    numno=1
                    nozero=nozero+zi(lprno+(noeuco-1)*(2+nbec)+1)*&
                    neqddl
                endif
30          continue
20      continue
!
        zi(lconnc+l1)=nbvois
        if (nbvois .gt. nbvmax) then
            nbvmax=nbvois
        endif
!
10  end do
!
!-----------------------------C
!--                         --C
!-- REMPLISSAGE DU NUME_DDL --C
!--                         --C
!-----------------------------C
!
    neq=6*nnoint
    nozero=21*nnoint+36*connec
!
    zi(ldprs+1)=neq
    call wkvect(prgene//'.NEQU', 'V V I', 1, ibid)
    zi(ibid)=neq
!
    call wkvect(prgene//'.DEEQ', 'V V I', neq*2, lddeeq)
    call wkvect(prgene//'.NUEQ', 'V V I', neq, ldnueq)
    call wkvect(prgene//'.DELG', 'V V I', neq, lddelg)
!
    do 40 i1 = 1, neq
        zi(ldnueq+i1-1)=i1
        zi(lddelg+i1-1)=0
        zi(lddeeq+(i1-1)*2)=i1
        zi(lddeeq+(i1-1)*2+1)=1
40  end do
!
!-- CONSTRUCTION DU .SMDE
    call wkvect(nume91//'.SMOS.SMDE', 'V V I', 6, ibid)
    zi(ibid)=neq
    zi(ibid+1)=nozero
    zi(ibid+2)=1
!
!-- CONSTRUCTION DU .SMDI ET DU .SMHC
    call wkvect(nume91//'.SMOS.SMDI', 'V V I', neq, lsmdi)
    call wkvect(nume91//'.SMOS.SMHC', 'V V S', nozero, lsmhc)
!
    call jeveuo('&&MOIN93.IND_NOEUD', 'E', lindno)
    call jeveuo('&&MOIN93.IPOS_DDL_INTERF', 'E', lipos)
    j1=0
    do 50 i1 = 1, nnoint
        zi(lindno+zi(lnddli+i1-1)-1)=i1
        zi(lipos+i1-1)=j1
        j1=j1+zi(lnddli+2*nnoint+i1-1)
50  end do
!
    ismhc=0
    indeq=0
!
    do 60 i1 = 1, nnoint
        no1=zi(lnddli+i1-1)
        nbvois=zi(lconnc+i1-1)
        neqddl=6
!
        do 70 j1 = 1, neqddl
!
!-- ON REMPLIT LES CONNECTIONS NOEUD COURANT / NOEUDS PRECEDENTS
            do 80 k1 = 1, nbvois
                no2=zi(lconnc+i1-1+k1*nnoint)
                neqd2=6
                indddl=zi(lipos+zi(lindno+no2-1)-1)
!
                do 90 l1 = 1, neqd2
                    zi4(lsmhc+ismhc)=indddl+l1
                    ismhc=ismhc+1
90              continue
80          continue
!
!-- ON REMPLIT LE BLOC DIAGONAL DU NOEUD COURANT
            indddl=zi(lipos+i1-1)
            do 100 l1 = 1, j1
                zi4(lsmhc+ismhc)=indddl+l1
                ismhc=ismhc+1
100          continue
            zi(lsmdi+indeq)=ismhc
            indeq=indeq+1
70      continue
60  end do
!
!-- CREATION DU SOLVEUR
    solveu=nume91//'.SOLV'
!
!-- TEST SUR LA PRESENCE DE MUMPS POUR ACCELERER LE CALCUL
    call haslib('MUMPS', iret)
    if (iret .eq. 0) then
        call crsolv('LDLT', 'SANS', solveu, 'V')
    else
        if (neq .lt. 120) then
!-- SOLVEUR = LDLT / OPTIONS PAR DEFAUT
            call crsolv('LDLT', 'SANS', solveu, 'V')
        else
!-- SOLVEUR = MUMPS / OPTIONS PAR DEFAUT
            call crsint(solveu)
        endif
    endif
!
!-----------------------------------------------------C
!--                                                 --C
!-- RECHERCHE DES PROPRIETES DU TREILLIS DE POUTRES --C
!--                                                 --C
!-----------------------------------------------------C
!
    call dismoi('F', 'NOM_MAILLA', raide, 'MATR_ASSE', ibid,&
                nomma, ierd)
    call jeveuo(nomma//'.COORDO    .VALE', 'L', lcoord)
!
    mindis=1.d16
    maxdis=0.d0
!
    do 110 i1 = 1, nnoint
        no1=zi(lnddli+i1-1)
        nbvois=zi(lconnc+i1-1)
        do 120 j1 = 1, nbvois
            no2=zi(lconnc+i1-1+j1*nnoint)
            dist=(zr(lcoord+(no2-1)*3)-zr(lcoord+(no1-1)*3))**2+&
            (zr(lcoord+(no2-1)*3+1)-zr(lcoord+(no1-1)*3+1))**2+&
            (zr(lcoord+(no2-1)*3+2)-zr(lcoord+(no1-1)*3+2))**2
            dist=sqrt(dist)
            if (dist .lt. mindis) then
                mindis=dist
            endif
            if (dist .gt. maxdis) then
                maxdis=dist
            endif
120      continue
110  end do
    rayon=(mindis+maxdis)/20
!
!
!
!------------------------------------------------------------------C
!--                                                              --C
!-- RECHERCHE D'UN POINT DE REFERENCE POUR DEFINIR L'ORIENTATION --C
!--                                                              --C
!------------------------------------------------------------------C
!
!      DO WHILE (TEMP .LT. LONG*1.D-10)
135  continue
!
    call getran(ptref(1))
    call getran(ptref(2))
    call getran(ptref(3))
!
    long=sqrt(ptref(1)**2+ptref(2)**2+ptref(3)**2)
    temp=1.d0
    do 130 i1 = 1, nnoint
        no1=zi(lnddli+i1-1)
        nbvois=zi(lconnc+i1-1)
        do 140 j1 = 1, nbvois
            no2=zi(lconnc+i1-1+j1*nnoint)
            direc(1)=zr(lcoord+(no2-1)*3)-zr(lcoord+(no1-1)*3)
            direc(2)=zr(lcoord+(no2-1)*3+1)-zr(lcoord+(no1-1)*3+1)
            direc(3)=zr(lcoord+(no2-1)*3+2)-zr(lcoord+(no1-1)*3+2)
            vtest(1)=ptref(1)-zr(lcoord+(no1-1)*3)
            vtest(2)=ptref(2)-zr(lcoord+(no1-1)*3+1)
            vtest(3)=ptref(3)-zr(lcoord+(no1-1)*3+2)
            dist=sqrt( (direc(2)*vtest(3)-direc(3)*vtest(2))**2+&
            (direc(1)*vtest(3)-direc(3)*vtest(1))**2+ (direc(2)*vtest(&
            1)-direc(1)*vtest(2))**2 )
            if (dist .lt. temp) then
                temp=dist
            endif
140      continue
130  continue
    if (temp .lt. long*1.d-10) then
        goto 135
    endif
!      END DO
!
!
!
!--------------------------------------------------------C
!--                                                    --C
!-- INITIALISATION DES MATRICES DE MASSE ET DE RAIDEUR --C
!--                                                    --C
!--------------------------------------------------------C
!
    call inmain(raiint, neq, nozero)
    call inmain(ssami, neq, nozero)
    call jeveuo(jexnum(raiint//'.VALM', 1), 'E', lraint)
    call jeveuo(jexnum(ssami//'.VALM', 1), 'E', lmaint)
!
    do 150 i1 = 1, nnoint
!
        nbvois=zi(lconnc+i1-1)
        do 160 j1 = 1, nbvois
            no1=zi(lnddli+i1-1)
            no2=zi(lconnc+i1-1+j1*nnoint)
!
!-- CONSTRUCTION DES MATRICES ELEMENTAIRES
            do 170 k1 = 1, 3
                direc(k1)=zr(lcoord+(no2-1)*3+k1-1)- zr(lcoord+(no1-1)&
                *3+k1-1)
                vtest(k1)=ptref(k1)-zr(lcoord+(no1-1)*3+k1-1)
170          continue
!
            call matint(kr, mr, direc, vtest, rayon)
            no1=i1
            no2=zi(lindno+no2-1)
            neqddl=zi(lnddli+2*nnoint+no1-1)
            neqd2=zi(lnddli+2*nnoint+no2-1)
!--
!-- REMPLISSAGE DES .VALM
!--
!
!-- REMPLISSAGE DES BLOCS SUR LA DIAGONALE
!
            do 180 k1 = 1, neqddl
                ipos1=zi(lipos+no1-1)+k1
                ipos2=zi(lsmdi+ipos1-1)-1
                m1=zi(lnddli+nnoint*(2+k1)+no1-1)
                do 190 l1 = 1, k1
                    n1=zi(lnddli+nnoint*(2+l1)+no1-1)
                    zr(lraint+ipos2-k1+l1)=zr(lraint+ipos2-k1+l1)+&
                    kr(n1,m1)
                    zr(lmaint+ipos2-k1+l1)=zr(lmaint+ipos2-k1+l1)+&
                    mr(n1,m1)
190              continue
180          continue
            do 200 k1 = 1, neqd2
                ipos1=zi(lipos+no2-1)+k1
                ipos2=zi(lsmdi+ipos1-1)-1
                m1=6+zi(lnddli+nnoint*(2+k1)+no2-1)
                do 210 l1 = 1, k1
                    n1=6+zi(lnddli+nnoint*(2+l1)+no1-1)
                    zr(lraint+ipos2-k1+l1)=zr(lraint+ipos2-k1+l1)+&
                    kr(n1,m1)
                    zr(lmaint+ipos2-k1+l1)=zr(lmaint+ipos2-k1+l1)+&
                    mr(n1,m1)
210              continue
200          continue
!
!-- REMPLISSAGE DU BLOC DE COUPLAGE
!
            ipos1=zi(lsmdi+ zi(lipos+no1-1) -1 )
            ipos2=zi(lsmdi+ zi(lipos+no1-1) )-ipos1
            m1=zi(lipos+no2-1)+1
            do 220 l1 = 1, ipos2-1
                if (zi4(lsmhc+ipos1+l1-1) .eq. m1) then
                    indeq=l1
                endif
220          continue
!
            do 230 k1 = 1, neqddl
                ipos1=zi(lsmdi+ zi(lipos+no1-1)+k1-2 )
                m1=zi(lnddli+nnoint*(2+k1)+no2-1)
                do 240 l1 = 1, neqd2
                    n1=6+zi(lnddli+nnoint*(2+l1)+no1-1)
                    zr(lraint+ipos1+indeq+l1-2)= zr(lraint+ipos1+&
                    indeq+l1-2)+ kr(m1,n1)
                    zr(lmaint+ipos1+indeq+l1-2)= zr(lmaint+ipos1+&
                    indeq+l1-2)+ mr(m1,n1)
240              continue
230          continue
!
160      continue
150  end do
!
!---------C
!--     --C
!-- FIN --C
!--     --C
!---------C
!
    call jedema()
!
end subroutine
