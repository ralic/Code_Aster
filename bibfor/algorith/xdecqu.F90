subroutine xdecqu(nnose, it, ndim, cnset, jlsn,&
                  jgrlsn, igeom, pinter, ninter, npts,&
                  ainter, pmilie, nmilie, mfis)
    implicit none
!
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/conare.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/infoar.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/loncar.h'
    include 'asterfort/ndcent.h'
    include 'asterfort/padist.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xajpin.h'
    include 'asterfort/xajpmi.h'
    include 'asterfort/xerfis.h'
    include 'asterfort/xintar.h'
    include 'asterfort/xmilac.h'
    include 'asterfort/xmilar.h'
    include 'asterfort/xmilfi.h'
    include 'asterfort/xxmmvd.h'
    integer :: nnose, it, ndim, cnset(*), ninter, igeom, npts, nmilie, mfis
    integer :: jgrlsn, jlsn
    real(kind=8) :: pinter(*), ainter(*), pmilie(*)
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
! TOLE CRS_1404
!                      TROUVER LES PTS D'INTERSECTION ENTRE LES ARETES
!                      ET LE PLAN DE FISSURE
!
!     ENTREE
!       NNOSE    : NOMBRE DE NOEUDS DU SOUS TETRA
!       IT       : INDICE DU TETRA EN COURS
!       CNSET    : CONNECTIVITÉ DES NOEUDS DU TETRA
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       IGEOM    : ADRESSE DES COORDONNÉES DES NOEUDS DE L'ELT PARENT
!       JGRLSN   : VALEURS DU GRADIENT DE LA LEVEL SET NORMALE
!
!     SORTIE
!       PINTER   : COORDONNÉES DES POINTS D'INTERSECTION
!       NINTER   : NB DE POINTS D'INTERSECTION
!       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN
!                  NOEUD SOMMET
!       AINTER   : INFOS ARETE ASSOCIÉE AU POINTS D'INTERSECTION
!       PMILIE   : COORDONNEES DES POINTS MILIEUX
!       NMILIE   : NB DE POINTS MILIEUX
!       MFIS     : NB DE POINTS MILIEUX SUR LA FISSURE
!     ----------------------------------------------------------------
!
    real(kind=8) :: a(ndim), b(ndim), c(ndim), m(ndim), lsna, lsnb, lsnm
    real(kind=8) :: alpha, longar, lonref, tampor(4), tabco(81)
    real(kind=8) :: milfi(ndim), milac(ndim), milara(ndim), milarb(ndim)
    real(kind=8) :: temp(ndim), x(ndim), xlsn, d, crilsn, val, rbid
    real(kind=8) :: tabls(27), tabar(9)
    integer :: ar(12, 3), nbar, nta, ntb, na, nb, ins
    integer :: ia, i, ipi, ipm, ibid, pp, pd, k, tabdir(4)
    integer :: ndime
    integer :: j, r, ip, ipp, a1, a2, nx
    integer :: ptmax, pmmax(2)
    integer :: ntm, nm, inm, nptm, nnop
    integer :: zxain
    character(len=8) :: typma, elrefe, elrese(2), tymase(2), elrefp
    logical :: cut
!
    parameter       (crilsn=1.d-4)
    parameter       (ptmax=3)
    data            elrese /'SE3','TR6'/
    data            tymase /'SEG3','TRIA6'/
    data            pmmax  / 2,  6 /
! --------------------------------------------------------------------
!
    call jemarq()
!
    zxain = xxmmvd('ZXAIN')
    call elref1(elrefp)
    call elref4(' ', 'RIGI', ndime, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!     VECTEUR REEL A 4 COMPOSANTES, POUR CHAQUE PT D'INTER :
!     - NUMERO ARETE CORRESPONDANTE (0 SI C'EST UN NOEUD SOMMET)
!     - VRAI NUMERO NOEUD CORRESPONDANT (SERT QUE POUR NOEUD SOMMET)
!     - LONGUEUR DE L'ARETE
!     - POSITION DU PT SUR L'ARETE
!
    elrefe=elrese(ndime)
    typma =tymase(ndime)
!
!     CALCUL DES COORDONNEES ET LSN DU NOEUD 9
    if (nnop .eq. 8) call ndcent(igeom, zr(jlsn), x, xlsn)
!
    nx=0
!     TABLEAU DES COORDONNEES DES NOEUDS DE L'ELEMENT ENFANT
    do 50 j = 1, nnose
        do 51 i = 1, ndim
            if (cnset(nnose*(it-1)+j) .eq. 9) then
                nx=j
                val=x(i)
            else
                val=zr(igeom-1+ndim*(cnset(nnose*(it-1)+j)-1)+i)
            endif
            tabco(ndim*(j-1)+i)=val
51      continue
50  end do
!
!     TABLEAU DES LSN DES NOEUDS DE L'ELEMENT ENFANT
    do 40 j = 1, nnose
        if (cnset(nnose*(it-1)+j) .eq. 9) then
            val=xlsn
        else
            val=zr(jlsn-1+cnset(nnose*(it-1)+j))
        endif
        tabls(j)=val
40  end do
!
!     L'ELEMENT EST IL TRAVERSE PAR LA FISSURE?
    cut=.false.
    i=1
!     (1) RECHERCHE D'UN NOEUD PIVOT (LSN NON NULLE)
 1  continue
    if (i .lt. nnose) then
        if (i .eq. nx .or. tabls(i) .eq. 0.d0) then
            i=i+1
            goto 1
        endif
    endif
!     (2) PRODUIT DE CE PIVOT PAR LES AUTRES LSN
    k=i+1
30  continue
    if (k .le. nnose) then
        if (tabls(i)*tabls(k) .lt. 0.d0) then
            cut=.true.
        else
            k=k+1
            goto 30
        endif
    endif
!
    call conare(typma, ar, nbar)
!     COMPTEUR DE POINT INTERSECTION = NOEUD SOMMET
    ins=0
!     COMPTEUR DE POINT INTERSECTION = NOEUD MILIEU
    inm=0
!     COMPTEUR DE POINT INTERSECTION = TOUS TYPES CONFONDUS
    ipi=0
!     LONGUEUR D'ARETE MAXIMALE DE L'ELEMENT (DU SE3 OU TR6)
    lonref=0.d0
!
!     BOUCLE SUR LES ARETES POUR DETERMINER LES POINTS D'INTERSECTION
    do 100 ia = 1, nbar
!
!       RECUPERATION NUM ENFANT DES NOEUDS DE L'ARETE
        nta=ar(ia,1)
        ntb=ar(ia,2)
        ntm=ar(ia,3)
!
!       RECUPERATION NUM PARENT DES NOEUDS DE L'ARETE
        na=cnset(nnose*(it-1)+nta)
        nb=cnset(nnose*(it-1)+ntb)
        nm=cnset(nnose*(it-1)+ntm)
!
!       RECUPERATION LSN DES NOEUDS EXTREMITE DE L'ARETE
        lsna=tabls(nta)
        lsnb=tabls(ntb)
        lsnm=tabls(ntm)
!
        call vecini(ndim, 0.d0, a)
        call vecini(ndim, 0.d0, b)
        call vecini(ndim, 0.d0, m)
!
!       RECUPERATION COORDONNEES DES NOEUDS EXTREMITE DE L'ARETE
        do 101 i = 1, ndim
            a(i)=tabco(ndim*(nta-1)+i)
            b(i)=tabco(ndim*(ntb-1)+i)
            m(i)=tabco(ndim*(ntm-1)+i)
101      continue
!
!     BLINDAGE PARTIEL : FISSURE RENTRANTE SUR UNE ARETE
        if (lsna*lsnm .lt. 0 .and. lsnb*lsnm .lt. 0) then
            call u2mess('F', 'XFEM_63')
        endif
!
!       LONGUEUR DE L'ARETE
        longar=padist(ndim,a,b)
!
! DEBUT RECHERCHE COORDONNEES DES POINTS D'INTERSECTION
! UN SEUL POINT INTER NON NOEUD PAR ARETE!
!
!       SI LA FISSURE COUPE L'ARETE
        if ((lsna*lsnb) .le. 0) then
!
!         SI LA FISSURE COUPE L'EXTREMITE A
            if (lsna .eq. 0) then
!           ON AJOUTE A LA LISTE LE POINT A
                call xajpin(ndim, pinter, ptmax, ipi, ins,&
                            a, longar, ainter, 0, na,&
                            0.d0)
            endif
!
!         SI LA FISSURE COUPE L'EXTREMITE B
            if (lsnb .eq. 0) then
!           ON AJOUTE A LA LISTE LE POINT B
                call xajpin(ndim, pinter, ptmax, ipi, ins,&
                            b, longar, ainter, 0, nb,&
                            longar)
            endif
!
!         SI LA FISSURE COUPE LE MILIEU M
!         PETITE TOLERANCE SUR LSNM CAR VALEUR CALCULEE
            d=999.d0
            if (abs(lsnm-lsnb) .gt. r8prem()) d=lsnm/(lsnm-lsnb)
!
            if (abs(lsnm-lsnb) .le. r8prem() .or. abs(d) .le. crilsn) then
                alpha=padist(ndim,a,m)
!
                if (cut) then
                    call xajpin(ndim, pinter, ptmax, ipi, inm,&
                                m, longar, ainter, ia, 0,&
                                alpha)
                else if (.not.cut) then
                    call xajpin(ndim, pinter, ptmax, ipi, inm,&
                                m, longar, ainter, 0, nm,&
                                alpha)
                endif
            endif
!
!         SI LA FISSURE COUPE AILLEURS
            if (lsna .ne. 0 .and. lsnb .ne. 0 .and. lsnm .ne. 0) then
!           INTERPOLATION DES COORDONNEES DE C
                call xintar(elrefe, ndim, ia, tabco, tabls,&
                            c)
!           POSITION DU PT D'INTERSECTION SUR L'ARETE
                alpha=padist(ndim,a,c)
!           ON AJOUTE A LA LISTE LE POINT C
                call xajpin(ndim, pinter, ptmax, ipi, ibid,&
                            c, longar, ainter, ia, 0,&
                            alpha)
            endif
!
        endif
!
100  end do
!
!       NB DE POINTS D'INTERSECTION
    ninter=ipi
!       NB DE POINTS D'INTERSECTION = NOEUD SOMMET
    npts  =ins
!       NB DE POINTS D'INTERSECTION = NOEUD MILIEU
    nptm  =inm
!
!     TRI DES POINTS D'INTERSECTION PAR ORDRE CROISSANT DES ARETES
    do 200 pd = 1, ninter-1
        pp=pd
        do 201 i = pp, ninter
            if (ainter(zxain*(i-1)+1) .lt. ainter(zxain*(pp-1)+1)) pp=i
201      continue
        do 202 k = 1, 4
            tampor(k)=ainter(zxain*(pp-1)+k)
            ainter(zxain*(pp-1)+k)=ainter(zxain*(pd-1)+k)
            ainter(zxain*(pd-1)+k)=tampor(k)
202      continue
        do 203 k = 1, ndim
            tampor(k)=pinter(ndim*(pp-1)+k)
            pinter(ndim*(pp-1)+k)=pinter(ndim*(pd-1)+k)
            pinter(ndim*(pd-1)+k)=tampor(k)
203      continue
200  continue
!
! DEBUT RECHERCHE COORDONNEES DES POINTS MILIEUX
    if (.not.cut) goto 999
!
! ON NE DEVRAIT AVOIR QUE
!       TR6    NINTER=2 NPTS=0         ---> NSE=3
!       TR6    NINTER=2 NPTS=1         ---> NSE=2
!       TR6    NINTER=3 NPTS=1 NPTM=1  ---> NSE=3
!       SE3    NINTER=1 NPTS=0         ---> NSE=2
    call xerfis(ndime, ninter, npts, nptm)
!
!     COMPTEUR DES POINTS INTERSECTION NON CONFONDUS AVEC ND SOMMET
    ip=0
!     COMPTEUR DE POINTS MILIEUX
    ipm=0
    inm=0
!     CALCUL D'UNE LONGUEUR CARACTERISTIQUE DE L'ELEMENT
    call loncar(ndim, typma, tabco, lonref)
!
    do 300 r = 1, ninter
        a2=nint(ainter(zxain*(r-1)+1))
        if (a2 .ne. 0) then
            ip = ip+1
            nm=ar(a2,3)
            if (ip .eq. 1) then
                call infoar(ndim, ar, a2, 1, tabco,&
                            tabls, a, b, m, rbid,&
                            rbid, rbid)
            else if (ip.gt.1) then
                do 301 i = 1, 2
                    do 302 j = 1, 2
                        if (ar(a2,i) .eq. ar(a1,j)) then
                            call infoar(ndim, ar, a2, i, tabco,&
                                        tabls, a, b, m, rbid,&
                                        rbid, rbid)
                            tabdir(1)=ar(a2,i)
                            tabdir(2)=ar(a1,3-j)
                            tabdir(3)=ar(a2,3-i)
                            tabdir(4)=ar(6-(a1+a2),3)
                            if (j .eq. 2) then
                                do 304 k = 1, ndim
                                    temp(k)=pmilie((ip-2)*ndim+k)
                                    pmilie((ip-2)*ndim+k)=pmilie((ip-&
                                    1)*ndim+k)
                                    pmilie((ip-1)*ndim+k)=temp(k)
304                              continue
                            endif
                        endif
302                  continue
301              continue
            endif
!
            call vecini(ndim, 0.d0, milara)
            call vecini(ndim, 0.d0, milarb)
            if (nm .eq. nx) then
                do 306 k = 1, ndim
                    milara(k)=(pinter(ndim*(r-1)+k)+b(k))/2
                    milarb(k)=(pinter(ndim*(r-1)+k)+a(k))/2
306              continue
            else
!         TABLEAU DES COORDONNEES DES NOEUDS DE L'ARETE AB : B,A,E
                do 307 i = 1, ndim
                    tabar(i)=b(i)
                    tabar(ndim+i)=a(i)
                    tabar(2*ndim+i)=m(i)
307              continue
!           INTERPOLATION DES COORDONNEES DES POINTS MILIEUX MA ET MB
                call xmilar(ndim, pinter, tabar, r, milara,&
                            milarb)
            endif
!
!         STOCKAGE PMILIE
            call xajpmi(pmilie, pmmax(ndim), ipm, inm, milara,&
                        lonref)
            call xajpmi(pmilie, pmmax(ndim), ipm, inm, milarb,&
                        lonref)
!
            if (r .eq. ninter .and. r .gt. 1) then
                ipp=r-1
!           CALCUL DU POINT MILIEU DE 101-102
                call xmilfi(elrefp, ndim, nnop, pinter, igeom,&
                            jlsn, ipp, r, milfi)
                mfis=mfis+1
!           STOCKAGE PMILIE
                call xajpmi(pmilie, pmmax(ndim), ipm, inm, milfi,&
                            lonref)
            endif
!
            if (ip .eq. 2) then
!           CALCUL DU POINT MILIEU DE 101-C
                call xmilac(ndim, igeom, pinter, tabco, tabdir,&
                            jgrlsn, ip, r, pmilie, milac)
!
!           STOCKAGE PMILIE
                call xajpmi(pmilie, pmmax(ndim), ipm, inm, milac,&
                            lonref)
            endif
!
            a1=a2
!
        endif
300  end do
!
!     NB DE POINTS MILIEUX
    nmilie = ipm
!
999  continue
!
    call jedema()
end subroutine
