subroutine xsifle(ndim, ifa, jptint, jaint, cface,&
                  igeom, nfh, singu, nfe, ddlc,&
                  ddlm, jlst, ipres, ipref, itemps,&
                  idepl, nnop, valres, basloc, ithet,&
                  nompar, presn, option, igthet, jbasec)
!
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
! person_in_charge: samuel.geniaut at edf.fr
! TOLE CRP_21 CRS_1404
!
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8pi.h'
    include 'asterfort/assert.h'
    include 'asterfort/chauxi.h'
    include 'asterfort/conare.h'
    include 'asterfort/confac.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/indent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xjacf2.h'
    include 'asterfort/xjacff.h'
    include 'asterfort/xxmmvd.h'
    character(len=8) :: nompar(4)
    character(len=16) :: option
    integer :: ndim, ifa, cface(5, 3), jaint, igeom, nfh, singu, jlst, ipres
    integer :: nfe, ddlc, ipref, itemps, nnop, ithet, jptint, igthet, idepl
    integer :: ddlm, jbasec
    real(kind=8) :: valres(3)
    real(kind=8) :: basloc(9*nnop), presn(27)
!
!
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE POST-TRAITEMENT
!                          EN MÉCANIQUE DE LA RUPTURE
!                          SUR LES LEVRES DES FISSURES X-FEM
!
!
!
! OUT IGTHET  : G (OPTION CALC_G) ET K1, K2, K3 (SI OPTION CALC_K_G)
!
!
!
    integer :: i, nli, in(3), iadzi, iazk24, ibid2(12, 3), ibid, fac(6, 4), nbf
    integer :: ar(12, 3), nbar, cpt, ino, nnof, npgf, ipoidf, ivff, idfdef
    integer :: ipgf, ier, ilev, k, j, zxain
    integer :: indi, ddld, ddls, nnops
    real(kind=8) :: mult, xg(4), jac, ff(27), nd(3), lst, lsn, rr(2), rb9(3, 3)
    real(kind=8) :: rb
    real(kind=8) :: forrep(3, 2), pres, cisa, depla(3), angl(2), r3bid(3)
    real(kind=8) :: e, nu, mu, ka, coeff, coeff3, r27bid(27)
    real(kind=8) :: u1l(3), u2l(3), u3l(3), u1(3), u2(3), u3(3)
    real(kind=8) :: e1(3), e2(3), e3(3), norme, p(3, 3), dfdi(nnop, ndim), divt
    real(kind=8) :: jm
    real(kind=8) :: theta(3), dtdm(3, 3), he(2), dfor(3), g, k1, k2, k3
    real(kind=8) :: dpredi(3, 3)
    character(len=8) :: elref, typma, fpg, elc, elrefc
    data     he / -1.d0 , 1.d0/
!
    call jemarq()
!
    zxain = xxmmvd('ZXAIN')
!     PAR CONVENTION :
!     LEVRE INFERIEURE (HE=-1) EST LA LEVRE 1, DE NORMALE SORTANTE  ND
!     LEVRE SUPERIEURE (HE=+1) EST LA LEVRE 2, DE NORMALE SORTANTE -ND
    angl(1) = -r8pi()
    angl(2) = r8pi()
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim*(1+nfh+nfe)
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls=ddld+ddlc
!
    call elref1(elref)
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)
!
    if (ndim .eq. 3) then
        call confac(typma, ibid2, ibid, fac, nbf)
        elc='TR3'
        fpg='XCON'
    else if (ndim.eq.2) then
        call conare(typma, ar, nbar)
        elc='SE2'
        fpg='MASS'
    endif
!
!     PETIT TRUC EN PLUS POUR LES FACES EN DOUBLE
    mult=1.d0
    do 101 i = 1, ndim
        nli=cface(ifa,i)
        in(i)=nint(zr(jaint-1+zxain*(nli-1)+2))
101  end do
!     SI LES 2/3 SOMMETS DE LA FACETTE SONT DES NOEUDS DE L'ELEMENT
    if (ndim .eq. 3) then
        if (in(1) .ne. 0 .and. in(2) .ne. 0 .and. in(3) .ne. 0) then
            do 102 i = 1, nbf
                cpt=0
                do 103 ino = 1, 4
                    if (in(1) .eq. fac(i,ino) .or. in(2) .eq. fac(i,ino) .or. in(3) .eq.&
                        fac(i,ino)) cpt=cpt+1
103              continue
                if (cpt .eq. 3) then
                    mult=0.5d0
                    goto 104
                endif
102          continue
        endif
    else if (ndim .eq. 2) then
        if (in(1) .ne. 0 .and. in(2) .ne. 0) then
            do 1021 i = 1, nbar
                cpt=0
                do 1031 ino = 1, 2
                    if (in(1) .eq. ar(i,ino) .or. in(2) .eq. ar(i,ino)) cpt=cpt+1
1031              continue
                if (cpt .eq. 2) then
                    mult=0.5d0
                    goto 104
                endif
1021          continue
        endif
    endif
104  continue
!
    call elref4(elc, fpg, ibid, nnof, ibid,&
                npgf, ipoidf, ivff, idfdef, ibid)
!
!     MATÉRIAU HOMOGENE
!     ON PEUT PAS LE RECUPERER SUR LES POINTS DE GAUSS DES FACETTES
!     CAR LA FAMILLE CONCATENEE DES PG DES FACETTES N'EXISTE PAS
    e = valres(1)
    nu = valres(2)
    mu = e / (2.d0*(1.d0+nu))
    ka = 3.d0 - 4.d0*nu
    coeff = e / (1.d0-nu*nu)
    coeff3 = 2.d0 * mu
!
!     ----------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
    do 900 ipgf = 1, npgf
!
!       CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
!       ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
!       ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
!       ET DE XG : COORDONNEES REELLES DU POINT DE GAUSS
!       ET DE DFDI : DERIVVES DES FF PARENT
        elrefc='NON'
        if (ndim .eq. 3) then
            call xjacff(elref, elrefc, elc, ndim, fpg,&
                        jptint, ifa, cface, ipgf, nnop,&
                        igeom, jbasec, xg, jac, ff,&
                        r27bid, dfdi, nd, r3bid, r3bid)
        else if (ndim.eq.2) then
            call xjacf2(elref, elrefc, elc, ndim, fpg,&
                        jptint, ifa, cface, ndim, ipgf,&
                        nnop, igeom, jbasec, xg, jac,&
                        ff, r27bid, dfdi, nd, r3bid)
        endif
!
!       BASE LOCALE ET LEVEL SETS AU POINT DE GAUSS
!       DIMENSIONNEMENT A 3 ET NON NDIM POUR POUVOIR UTILISER NORMEV.F
        call vecini(3, 0.d0, e1)
        call vecini(3, 0.d0, e2)
        lsn=0.d0
        lst=0.d0
        do 100 ino = 1, nnop
            lsn = lsn + zr(jlst-1+ino)*ff(ino)
            lst = lst + zr(jlst-1+ino)*ff(ino)
            do 110 i = 1, ndim
                e1(i) = e1(i) + basloc(3*ndim*(ino-1)+i+ndim) * ff( ino)
                e2(i) = e2(i) + basloc(3*ndim*(ino-1)+i+2*ndim) * ff( ino)
110          continue
100      continue
!
!       NORMALISATION DE LA BASE
        call normev(e1, norme)
        call normev(e2, norme)
        call provec(e1, e2, e3)
!
!       CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
        call vecini(9, 0.d0, p)
        do 120 i = 1, ndim
            p(i,1)=e1(i)
            p(i,2)=e2(i)
            p(i,3)=e3(i)
120      continue
!
!       CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
        call assert(lst.lt.0.d0)
        rr(1)=-sqrt(-lst)
        rr(2)= sqrt(-lst)
!
!       -----------------------------------------------
!       1) CALCUL DES FORCES SUIVANT LES OPTIONS
!       -----------------------------------------------
!
        call vecini(3*2, 0.d0, forrep)
!
        if ((option.eq.'CALC_K_G') .or. (option.eq.'CALC_G')) then
!
!         CALCUL DE LA PRESSION AUX POINTS DE GAUSS
            pres = 0.d0
            cisa = 0.d0
            do 240 ino = 1, nnop
                if (ndim .eq. 3) pres = pres + zr(ipres-1+ino) * ff(ino)
                if (ndim .eq. 2) then
                    pres = pres + zr(ipres-1+2*(ino-1)+1) * ff(ino)
                    cisa = cisa + zr(ipres-1+2*(ino-1)+2) * ff(ino)
                endif
240          continue
            do 250 j = 1, ndim
                forrep(j,1) = -pres * nd(j)
                forrep(j,2) = -pres * (-nd(j))
250          continue
            if (ndim .eq. 2) then
                forrep(1,1) = forrep(1,1)- cisa * nd(2)
                forrep(2,1) = forrep(2,1)+ cisa * nd(1)
                forrep(1,2) = forrep(1,2)- cisa * (-nd(2))
                forrep(2,2) = forrep(2,2)+ cisa * (-nd(1))
            endif
!
            elseif ((option.eq.'CALC_K_G_F') .or. (option.eq.'CALC_G_F'))&
        then
!
!         VALEUR DE LA PRESSION
            xg(ndim+1) = zr(itemps)
            call fointe('FM', zk8(ipref), ndim+1, nompar, xg,&
                        pres, ier)
            if (ndim .eq. 2) call fointe('FM', zk8(ipref+1), ndim+1, nompar, xg,&
                                         cisa, ier)
            do 260 j = 1, ndim
                forrep(j,1) = -pres * nd(j)
                forrep(j,2) = -pres * (-nd(j))
260          continue
            if (ndim .eq. 2) then
                forrep(1,1) = forrep(1,1)- cisa * nd(2)
                forrep(2,1) = forrep(2,1)+ cisa * nd(1)
                forrep(1,2) = forrep(1,2)- cisa * (-nd(2))
                forrep(2,2) = forrep(2,2)+ cisa * (-nd(1))
            endif
!
        else
            call u2mess('F', 'XFEM_15')
        endif
!
!       -----------------------------------
!       2) CALCUL DE THETA ET DE DIV(THETA)
!       -----------------------------------
!
        divt= 0.d0
        call vecini(9, 0.d0, dtdm)
!
        do 390 i = 1, ndim
!
            theta(i)=0.d0
            do 301 ino = 1, nnop
                theta(i) = theta(i) + ff(ino) * zr(ithet-1+ndim*(ino- 1)+i)
301          continue
!
            do 310 j = 1, ndim
                do 311 ino = 1, nnop
                    dtdm(i,j) = dtdm(i,j) + zr(ithet-1+ndim*(ino-1)+i) * dfdi(ino,j)
311              continue
310          continue
!
            divt = divt + dtdm(i,i)
!
390      continue
!
!       BOUCLE SUR LES DEUX LEVRES
        do 300 ilev = 1, 2
!
!         ---------------------------------------------
!         3) CALCUL DU DEPLACEMENT
!         ---------------------------------------------
!
            call vecini(ndim, 0.d0, depla)
            do 200 ino = 1, nnop
                call indent(ino, ddls, ddlm, nnops, indi)
                cpt=0
!           DDLS CLASSIQUES
                do 201 i = 1, ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + ff(ino)*zr(idepl-1+indi+cpt)
201              continue
!           DDLS HEAVISIDE
                do 202 i = 1, nfh*ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + he(ilev) * ff(ino) * zr( idepl-1+indi+cpt)
202              continue
!           DDL ENRICHIS EN FOND DE FISSURE
                do 204 i = 1, singu*ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + rr(ilev) * ff(ino) * zr( idepl-1+indi+cpt)
204              continue
200          continue
!
!         --------------------------------
!         4) CALCUL DES CHAMPS AUXILIAIRES
!         --------------------------------
!
            if (option(1:8) .eq. 'CALC_K_G') then
!           CHAMPS AUXILIARES DANS LA BASE LOCALE : U1L,U2L,U3L
                call vecini(9, 0.d0, rb9)
                call chauxi(ndim, mu, ka, -lst, angl(ilev),&
                            rb9, .false., rb, rb9, rb9,&
                            rb9, u1l, u2l, u3l)
!
!           CHAMPS AUXILIARES DANS LA BASE GLOBALE : U1,U2,U3
                call vecini(ndim, 0.d0, u1)
                call vecini(ndim, 0.d0, u2)
                call vecini(ndim, 0.d0, u3)
                do 510 i = 1, ndim
                    do 511 j = 1, ndim
                        u1(i) = u1(i) + p(i,j) * u1l(j)
                        u2(i) = u2(i) + p(i,j) * u2l(j)
                        if (ndim .eq. 3) u3(i) = u3(i) + p(i,j) * u3l(j)
511                  continue
510              continue
            endif
!
!         -----------------------------------------
!         5) CALCUL DE 'DFOR' =  D(PRES)/DI . THETA
!         -----------------------------------------
!
            call vecini(9, 0.d0, dpredi)
            call vecini(3, 0.d0, dfor)
!
            do 400 i = 1, ndim
                do 410 j = 1, ndim
                    do 411 ino = 1, nnop
                        if ((option.eq.'CALC_K_G') .or. ( option.eq.'CALC_G')) then
                            dpredi(i,j) = dpredi(i,j) + he(ilev) * dfdi(ino,j) * zr(ipres-1+ino) &
                                          &* nd(i)
                        endif
                        if ((option.eq.'CALC_K_G_F') .or. ( option.eq.'CALC_G_F')) then
                            dpredi(i,j) = dpredi(i,j) + he(ilev) * dfdi(ino,j) * presn(ino) * nd(&
                                          &i)
                        endif
411                  continue
410              continue
400          continue
            do 312 i = 1, ndim
                do 313 k = 1, ndim
                    dfor(i) = dpredi(i,k) * theta(k)
313              continue
312          continue
!
!         -----------------------------------
!         6) CALCUL EFFECTIF DE G, K1, K2, K3
!         -----------------------------------
            g = 0.d0
            k1 = 0.d0
            k2 = 0.d0
            k3 = 0.d0
!
            do 520 j = 1, ndim
                g = g + (forrep(j,ilev) * divt + dfor(j)) * depla(j)
                if (option(1:8) .eq. 'CALC_K_G') then
                    k1 = k1 + (forrep(j,ilev) * divt + dfor(j)) * u1( j)
                    k2 = k2 + (forrep(j,ilev) * divt + dfor(j)) * u2( j)
                    if (ndim .eq. 3) k3 = k3 + ( forrep(j,ilev) * divt + dfor(j)) * u3(j )
                endif
520          continue
!
            jm = jac*mult*0.5d0
!
            if (ndim .eq. 3) then
!
                zr(igthet-1+1)=zr(igthet-1+1)+g*jac*mult
!
                if (option(1:8) .eq. 'CALC_K_G') then
                    zr(igthet-1+2)=zr(igthet-1+2)+k1*jm*sqrt(coeff)
                    zr(igthet-1+3)=zr(igthet-1+3)+k2*jm*sqrt(coeff)
                    zr(igthet-1+4)=zr(igthet-1+4)+k3*jm*sqrt(coeff3)
                    zr(igthet-1+5)=zr(igthet-1+5)+k1*jm*coeff
                    zr(igthet-1+6)=zr(igthet-1+6)+k2*jm*coeff
                    zr(igthet-1+7)=zr(igthet-1+7)+k3*jm*coeff3
                endif
!
            else if (ndim.eq.2) then
!
                zr(igthet-1+1)=zr(igthet-1+1)+g*jac*mult
!
                if (option(1:8) .eq. 'CALC_K_G') then
                    zr(igthet-1+2)=zr(igthet-1+2)+k1*jm*sqrt(coeff)
                    zr(igthet-1+3)=zr(igthet-1+3)+k2*jm*sqrt(coeff)
                    zr(igthet-1+4)=zr(igthet-1+4)+k1*jm*coeff
                    zr(igthet-1+5)=zr(igthet-1+5)+k2*jm*coeff
                endif
!
            endif
!
300      continue
!       FIN DE BOUCLE SUR LES DEUX LEVRES
!
900  end do
!     FIN DE BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
!     ----------------------------------------------------------------
    call jedema()
end subroutine
