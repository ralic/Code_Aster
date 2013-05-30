subroutine strmag(nugene, typrof)
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
!    P. RICHARD     DATE 02/11/92
!-----------------------------------------------------------------------
!  BUT:      < REFE 127 >
    implicit none
!
!  CREER LE STOCKAGE DU NUME_DDL_GENE
!-----------------------------------------------------------------------
!
! NUGENE   /I/: NOM K14 DU NUME_DDL_GENE
!
!
    include 'jeveux.h'
!
    include 'asterfort/crsmos.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jevtbl.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/slismo.h'
    include 'asterfort/smosli.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    real(kind=8) :: valr(2)
!
!
!
    character(len=8) :: nomprn, modgen, sst(2)
    character(len=19) :: stomor, stolci, prgene
    character(len=14) :: nugene
    character(len=8) :: bid
    character(len=24) :: typrof
    character(len=1) :: k1bid
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad1, iad1c, iad1l, iad2l, iadc, iadcou
    integer :: iadl, ibid, ifimes, j, jscbl, jscde
    integer :: jscdi, jschc, jscib, k, l, lc, lcolmx
    integer :: lcomoy, lh, ll, lldefl, llnequ, llnueq, llorl
    integer :: llors, llprl, llprs, llref, nbcol, nblig, nbloc
    integer :: nbprno, nbsst, neq, nsstar, ntbloc, nterbl, nterm
    integer :: ntermx, ntprno, nuant, nulia, nusst
    real(kind=8) :: rtbloc
!-----------------------------------------------------------------------
    call jemarq()
    ifimes=iunifi('MESSAGE')
    stomor=nugene//'.SMOS'
    stolci=nugene//'.SLCS'
    prgene=nugene//'.NUME'
    call jeveuo(prgene//'.NEQU', 'L', llnequ)
    neq=zi(llnequ)
!
!
    if (typrof .eq. 'PLEIN' .or. typrof .eq. 'DIAG') then
!        CREATION DES STOCKAGES MORSE ET L_CIEL :
        call crsmos(stomor, typrof, neq)
        rtbloc=jevtbl('TAILLE_BLOC')
        call smosli(stomor, stolci, 'G', rtbloc)
        goto 9999
    endif
!
!
!     -- CAS TYPROF=LIGN_CIEL :
!     --------------------------
    rtbloc=jevtbl('TAILLE_BLOC')
    ntbloc=int(1024*rtbloc)
!
!----------------RECUPERATION DU MODELE GENERALISE----------------------
!          ET NOMBRE DE SOUS-STRUCTURE
    call jeveuo(prgene//'.REFN', 'L', llref)
    modgen=zk24(llref)(1:8)
    call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst, k1bid)
!
!
!---------------DETERMINATION DU PROFIL(LIGNE DE CIEL)------------------
    call wkvect(stolci//'.SCHC', 'G V I', neq, jschc)
!
    call jeveuo(prgene//'.NUEQ', 'L', llnueq)
    call jelira(prgene//'.PRNO', 'NMAXOC', nbprno, k1bid)
    if (typrof .eq. 'LIGN_CIEL') then
!
!  BOUCLE SUR LIGRELS DU PRNO
        do 10 i = 1, nbprno
            call jelira(jexnum(prgene//'.PRNO', i), 'LONMAX', ntprno, bid)
            ntprno=ntprno/2
            call jenuno(jexnum(prgene//'.LILI', i), nomprn)
!
!   CAS DU PRNO &SOUSSTR (MATRICES PROJETEES)
            if (nomprn .eq. '&SOUSSTR') then
                call jeveuo(jexnum(prgene//'.PRNO', i), 'L', llprs)
                do 20 j = 1, ntprno
                    iad1=zi(llprs+(j-1)*2)
                    nblig=zi(llprs+(j-1)*2+1)
!
!   BOUCLE SUR LES COLONNES DE LA MATRICE PROJETEE
                    do 30 k = 1, nblig
                        iadcou=zi(llnueq+iad1-1+k-1)
                        lh=jschc+iadcou-1
                        zi(lh)=max(zi(lh),k)
30                  continue
20              continue
!
!
!    CAS  DU PRNO LAGRANGE D'INTERFACE
            else
                call jeveuo(jexnum(prgene//'.ORIG', i), 'L', llorl)
                call jeveuo(jexnum(prgene//'.PRNO', i), 'L', llprl)
                call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
                call jeveuo(jexnum(prgene//'.ORIG', ibid), 'L', llors)
                call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
                call jeveuo(jexnum(prgene//'.PRNO', ibid), 'L', llprs)
!
                do 40 j = 1, ntprno
                    nulia=zi(llorl+j-1)
                    call jeveuo(jexnum(modgen//'      .MODG.LIDF', nulia), 'L', lldefl)
! RECUPERATION DES 2 SOU-STRUCTURES ASSOCIEES
                    sst(1)=zk8(lldefl)
                    sst(2)=zk8(lldefl+2)
                    iad1l=zi(llprl+(j-1)*2)
                    nblig=zi(llprl+(j-1)*2+1)
                    do 50 k = 1, 2
                        call jenonu(jexnom(modgen//'      .MODG.SSNO', sst(k)), nusst)
!  RECUPERATION NUMERO TARDIF
                        do 60 l = 1, nbsst
                            if (zi(llors+l-1) .eq. nusst) nsstar=l
60                      continue
                        iad1c=zi(llprs+(nsstar-1)*2)
                        nbcol=zi(llprs+(nsstar-1)*2+1)
                        do 70 ll = 1, nblig
                            iadl=zi(llnueq+(iad1l-1)+(ll-1))
                            do 80 lc = 1, nbcol
                                iadc=zi(llnueq+(iad1c-1)+(lc-1))
                                lh=jschc+max(iadc,iadl)-1
                                zi(lh)=max(zi(lh),abs(iadc-iadl)+1)
80                          continue
70                      continue
50                  continue
!
! TRAITEMENT DES MATRICES LAGRANGE-LAGRANGE
!
! RECUPERATION DU NUMERO NOEUD TARDIF ANTAGONISTE
                    do 90 l = 1, ntprno
                        if (zi(llorl+l-1) .eq. nulia .and. l .ne. j) nuant=l
90                  continue
                    iad2l=zi(llprl+(nuant-1)*2)
                    do 95 ll = 1, nblig
                        iadl=zi(llnueq+(iad1l-1)+(ll-1))
                        iadc=zi(llnueq+(iad2l-1)+(ll-1))
! TERME CROISE LAGRANGE LAGRANGE
                        lh=jschc+max(iadc,iadl)-1
                        zi(lh)=max(zi(lh),abs(iadc-iadl)+1)
! TERME DIAGONAL LAGRANGE LAGRANGE
                        lh=jschc+iadl-1
                        zi(lh)=max(zi(lh),1)
95                  continue
!
!
40              continue
            endif
10      end do
    else if (typrof.eq.'PLEIN') then
        write(6,*) 'PROFIL PLEIN!!!!'
        do 15 i = 1, neq
            zi(jschc+i-1)=i
15      continue
    endif
!
!
!---------------DETERMINATION DE LA TAILLE MAX D'UNE COLONNE------------
    lcomoy=0
    lcolmx=0
    do 100 i = 1, neq
        lcolmx=max(lcolmx,zi(jschc+i-1))
        lcomoy=lcomoy+zi(jschc+i-1)
100  end do
!
    lcomoy=lcomoy/neq
!
    if (lcolmx .gt. ntbloc) then
        ntbloc=lcolmx
        valr (1) = rtbloc
        valr (2) = lcolmx/1.d+3
        call u2mesg('I', 'ALGORITH14_66', 0, ' ', 0,&
                    0, 2, valr)
    endif
!
    write(ifimes,*)'+++ HAUTEUR MAXIMUM D''UNE COLONNE: ',lcolmx
    write(ifimes,*)'+++ HAUTEUR MOYENNE D''UNE COLONNE: ',lcomoy
!
!
!----------------DETERMINATION DU NOMBRE DE BLOCS-----------------------
    nbloc=1
    nterbl=0
    ntermx=0
    call wkvect(stolci//'.SCIB', 'G V I', neq, jscib)
    do 110 i = 1, neq
        nterm=nterbl+zi(jschc+i-1)
        if (nterm .gt. ntbloc) then
            nbloc=nbloc+1
            nterbl=zi(jschc+i-1)
            ntermx=max(ntermx,nterbl)
        else
            nterbl=nterm
            ntermx=max(ntermx,nterbl)
        endif
        zi(jscib+i-1)=nbloc
110  end do
!
    write(ifimes,*)'+++ NOMBRE DE BLOCS DU STOCKAGE: ',nbloc
!
!
!-------------DERNIERE BOUCLE SUR LES BLOCS POUR  SCDI ET SCBL----------
!  ON REDUIT LA TAILLE DES BLOCS A LA TAILLE UTILE MAX (GAIN DE PLACE)
!
    ntbloc=ntermx
    call wkvect(stolci//'.SCBL', 'G V I', nbloc+1, jscbl)
    call wkvect(stolci//'.SCDI', 'G V I', neq, jscdi)
!
    nterbl=0
    nbloc=1
    zi(jscbl)=0
!
    do 120 i = 1, neq
        nterm=nterbl+zi(jschc+i-1)
        if (nterm .gt. ntbloc) then
            nterbl=zi(jschc+i-1)
            nbloc=nbloc+1
        else
            nterbl=nterm
        endif
        zi(jscbl+nbloc)=i
        zi(jscdi+i-1)=nterbl
120  end do
!
!
!     -- .SCDE
    call wkvect(stolci//'.SCDE', 'G V I', 6, jscde)
    zi(jscde-1+1)=neq
    zi(jscde-1+2)=ntbloc
    zi(jscde-1+3)=nbloc
    zi(jscde-1+4)=lcolmx
!
!
!     -- ON TRANSFORME LE STOCKAGE LIGNE_CIEL EN STOCKAGE MORSE :
    call slismo(stolci, stomor, 'G')
!
!
9999  continue
    call jedema()
end subroutine
