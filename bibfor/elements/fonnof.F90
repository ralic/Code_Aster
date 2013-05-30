subroutine fonnof(resu, noma, typfon, nbnoff)
!
    implicit   none
!
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterfort/cgnop0.h'
    include 'asterfort/dfflon.h'
    include 'asterfort/dfftan.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/gmgnre.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/oreino.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: noma, resu, typfon
    integer :: nbnoff
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
!-----------------------------------------------------------------------
! FONCTION REALISEE (OPERATEUR DEFI_FOND_FISS) :
!
!      REMPLISSAGE DES OBJETS .SUPNORM.NOEU ET .INFNORM.NOEU
!
!     ENTREES:
!        RESU       : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
!        NOMA       : NOM DU MAILLAGE
!        TYPFON     : TYPE DE FOND
!                     IL PEUT VALOIR OUVERT/FERME/INOFF/SUP
!        NBNOFF     : NOMBRE DE NOEUDS EN FOND DE FISSURE
!-----------------------------------------------------------------------
!
    integer :: nbma, jlima, im, n1, igeom, nbnoe, iret, jsup, jnols, inols
    integer :: idlino, nbnols, jcoors, in, nbnoft, inoff, ifm, niv, jnofo
    integer :: nuno, jints, nbtrls, iera
    integer :: numfin, isym, inoli, jinf, jnoli, nbnoli, jcoori, jinti
    integer :: nbtrli, ino, inos, nbs, numun, jts, jti, nbi, inoi
    integer :: jnofos, irlev, jbaslo, ndim, iarg
    real(kind=8) :: x0(3), x1, x2, y1, y2, z1, z2, d, vplan(3), dmin
    real(kind=8) :: dmax, prec, preco, ps, vectan(3), precn
    character(len=6) :: nompro
    character(len=8) :: k8b, critn
    character(len=24) :: msup, minf, fonnoe, nomnoe
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
    nompro = 'FONNOF'
    critn = 'RELATIF'
    call infniv(ifm, niv)
!
!     ------------------------------------------------------------------
!                        LE MAILLAGE, LE FOND
!     ------------------------------------------------------------------
!
    call getvid(' ', 'MAILLAGE', 1, iarg, 1,&
                noma, n1)
!
    call getvr8(' ', 'PREC_NORM', 1, iarg, 1,&
                precn, n1)
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', igeom)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoe,&
                k8b, iret)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8b, iret)
    nomnoe = noma//'.NOMNOE'
    fonnoe = resu//'.FOND.NOEU'
    call jeexin(fonnoe, irlev)
    if (irlev .ne. 0) then
        call jeveuo(fonnoe, 'L', jnofo)
    else
        fonnoe =resu//'.FOND_INF.NOEU'
        call jeveuo(fonnoe, 'L', jnofo)
        fonnoe =resu//'.FOND_SUP.NOEU'
        call jeveuo(fonnoe, 'L', jnofos)
    endif
!
!     BASE LOCALE EN FOND DE FISSURE
    call jeveuo(resu//'.BASEFOND', 'L', jbaslo)
!
!     ------------------------------------------------------------------
!                  VECTEUR RESULTAT
!     ------------------------------------------------------------------
!
    call wkvect(resu//'.SUPNORM.NOEU', 'G V K8', nbnoff*20, inols)
!
    call jeexin(resu//'.LEVREINF.MAIL', isym)
    if (isym .ne. 0) then
        call wkvect(resu//'.INFNORM.NOEU', 'G V K8', nbnoff*20, inoli)
    endif
!
!     ------------------------------------------------------------------
!         GROUP_MA LEVRE_SUP --> GROUP_NO LEVRE_SUP
!     ------------------------------------------------------------------
    msup = resu//'.LEVRESUP.MAIL'
    call jeveuo(msup, 'L', jsup)
    call jelira(msup, 'LONMAX', nbma, k8b)
!
    call wkvect('&&'//nompro//'_TRAV', 'V V I', nbnoe, idlino)
    call wkvect('&&'//nompro//'_NOEU_NORM_SUP', 'V V I', nbnoe, jnols)
!
    call wkvect('&&'//nompro//'_MAILLE_LEV_SUP', 'V V I', nbma, jlima)
    do 10 im = 1, nbma
        call jenonu(jexnom(noma//'.NOMMAI', zk8(jsup-1 + im)), zi( jlima-1 + im))
10  end do
    call gmgnre(noma, nbnoe, zi(idlino), zi(jlima), nbma,&
                zi(jnols), nbnols, 'TOUS')
!
    call wkvect('&&PKFOND_COOR_LEV_SUP', 'V V R', 3*nbnols, jcoors)
    do 11 in = 1, nbnols
        zr(jcoors-1+3*(in-1)+1) = zr(igeom-1+3*(zi(jnols-1+in)-1)+1)
        zr(jcoors-1+3*(in-1)+2) = zr(igeom-1+3*(zi(jnols-1+in)-1)+2)
        zr(jcoors-1+3*(in-1)+3) = zr(igeom-1+3*(zi(jnols-1+in)-1)+3)
11  end do
!
    call jedetr('&&'//nompro//'_TRAV')
!
!     ------------------------------------------------------------------
!         GROUP_MA LEVRE_INF --> GROUP_NO LEVRE_INF
!     ------------------------------------------------------------------
    if (isym .ne. 0) then
        minf = resu//'.LEVREINF.MAIL'
        call jeveuo(minf, 'L', jinf)
        call jelira(minf, 'LONMAX', nbma, k8b)
!
        call wkvect('&&'//nompro//'_TRAV', 'V V I', nbnoe, idlino)
        call wkvect('&&'//nompro//'_NOEU_NORM_INF', 'V V I', nbnoe, jnoli)
        call wkvect('&&'//nompro//'_MAILLE_LEV_INF', 'V V I', nbma, jlima)
        do 20 im = 1, nbma
            call jenonu(jexnom(noma//'.NOMMAI', zk8(jinf-1 + im)), zi(jlima-1 + im))
20      continue
        call gmgnre(noma, nbnoe, zi(idlino), zi(jlima), nbma,&
                    zi(jnoli), nbnoli, 'TOUS')
!
        call wkvect('&&PKFOND_COOR_LEV_INF', 'V V R', 3*nbnoli, jcoori)
        do 21 in = 1, nbnoli
            zr(jcoori-1+3*(in-1)+1) = zr(igeom-1+3*(zi(jnoli-1+in)-1)+ 1)
            zr(jcoori-1+3*(in-1)+2) = zr(igeom-1+3*(zi(jnoli-1+in)-1)+ 2)
            zr(jcoori-1+3*(in-1)+3) = zr(igeom-1+3*(zi(jnoli-1+in)-1)+ 3)
21      continue
        call jedetr('&&'//nompro//'_TRAV')
    endif
!
!     ------------------------------------------------------------------
!        BOUCLE SUR LES NOEUDS DU FOND DE FISSURE
!     ------------------------------------------------------------------
!
!
! ------ CAS DU FOND_FERME: LE PREMIER ET LE DERNIER NOEUD SONT
!                           IDENTIQUES
    if (typfon .eq. 'FERME') then
        nbnoft = nbnoff - 1
    else
        nbnoft = nbnoff
    endif
!
!
    do 200 inoff = 1, nbnoft
!
!        DETERMINATION DU PLAN ORTHOGONAL AU FOND DE FISSURE
!        ET PASSANT PAR LE NOEUD COURANT
!
        call jenonu(jexnom(nomnoe, zk8(jnofo-1+inoff)), nuno)
!
        x0(1) = zr(igeom-1+3*(nuno-1)+1)
        x0(2) = zr(igeom-1+3*(nuno-1)+2)
        x0(3) = zr(igeom-1+3*(nuno-1)+3)
!
!        VPLAN = VECTEUR ORTHOGONAL AU PLAN RECHERCHE
        call dfftan(ndim, zr(jbaslo), inoff, vplan)
!
!        D = LONGUEUR DU SEGMENT DU FOND DE FISSURE
        if (ndim .eq. 3) then
            call dfflon(zr(igeom), zk8(jnofo), nomnoe, inoff, nbnoff,&
                        typfon, d)
        else if (ndim.eq.2) then
!          D N'A PAS DE SENS EN 2D, ON PREND ALORS UNE VALEUR CARACT
            d = sqrt(x0(1)**2+x0(2)**2)
        endif
!
!        PREC = PRECISION POUR LA RECHERCHE DES NOEUDS DES LEVRES
        prec = d * precn
!
! ------ CALCUL DE L'INTERSECTION DU PLAN AVEC LES NOEUDS DES MAILLES
!        DEFINISSANT LA LEVRE INFERIEURE - SAUVEGARDE
!
        if (isym .ne. 0) then
            call wkvect('&&PKFOND_INTERS_INF', 'V V I', nbnoe, jinti)
            call cgnop0(nbnoli, zr(jcoori), x0, vplan, prec,&
                        nbtrli, zi(jinti))
            if (nbtrli .le. 2) then
                call jedetr('&&PKFOND_INTERS_INF')
                goto 200
            endif
            call wkvect('&&PKFOND_TRAV_INF', 'V V I', nbtrli, jti)
!
! ---- ORDRE DES NOEUDS
            numfin = nuno
            dmax = 0.d0
            dmin = 100.d0
            nbi = 1
            inoi = 1
            zi(jti-1 + inoi) = nuno
            x1 = zr(igeom-1+3*(nuno-1)+1)
            y1 = zr(igeom-1+3*(nuno-1)+2)
            z1 = zr(igeom-1+3*(nuno-1)+3)
!
! identification noeuds sur bon cote de la levre (cas fond ferme)
! a partir du noeud le plus proche du fond
            do 310 in = 1, nbtrli
                ino = jnoli+zi(jinti-1 + in)-1
                if (zi(ino) .eq. nuno) goto 310
                x2 = zr(igeom-1+3*(zi(ino)-1)+1)
                y2 = zr(igeom-1+3*(zi(ino)-1)+2)
                z2 = zr(igeom-1+3*(zi(ino)-1)+3)
                d = sqrt( (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2 )
                if (d .lt. dmin) then
                    dmin = d
                    numun = zi(ino)
                endif
310          continue
!
            vectan(1) = zr(igeom-1+3*(numun-1)+1)-x1
            vectan(2) = zr(igeom-1+3*(numun-1)+2)-y1
            vectan(3) = zr(igeom-1+3*(numun-1)+3)-z1
!
            do 320 in = 1, nbtrli
                ino = jnoli+zi(jinti-1 + in)-1
                if (zi(ino) .eq. nuno) goto 320
                x2 = zr(igeom-1+3*(zi(ino)-1)+1)
                y2 = zr(igeom-1+3*(zi(ino)-1)+2)
                z2 = zr(igeom-1+3*(zi(ino)-1)+3)
                d = sqrt( (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2 )
                ps=(x2-x1)*vectan(1)+(y2-y1)*vectan(2)+(z2-z1)*vectan(&
                3)
                if (ps .ge. 0.d0) then
                    nbi = nbi+1
                    inoi = inoi+1
                    zi(jti-1 + inoi) = zi(ino)
                    if (d .gt. dmax) then
                        dmax = d
                        numfin = zi(ino)
                    endif
                endif
320          continue
!
            preco = prec*10
            call oreino(noma, zi(jti), nbi, nuno, numfin,&
                        zr(igeom), critn, preco, iera, iret)
!
            do 330 in = 1, min(nbi, 20)
                call jenuno(jexnum(nomnoe, zi(jti-1+in)), zk8(inoli-1 + 20*(inoff-1)+in))
330          continue
!
            call jedetr('&&PKFOND_INTERS_INF')
            call jedetr('&&PKFOND_TRAV_INF')
        endif
!
! ------ CALCUL DE L'INTERSECTION DU PLAN AVEC LES NOEUDS DES MAILLES
!        DEFINISSANT LA LEVRE SUPERIEURE - SAUVEGARDE
!
        call wkvect('&&PKFOND_INTERS_SUP', 'V V I', nbnoe, jints)
        call cgnop0(nbnols, zr(jcoors), x0, vplan, prec,&
                    nbtrls, zi(jints))
        if (nbtrls .le. 2) then
            call jedetr('&&PKFOND_INTERS_SUP')
            goto 200
        endif
        call wkvect('&&PKFOND_TRAV_SUP', 'V V I', nbtrls, jts)
!
! ---- ORDRE DES NOEUDS
        if (irlev .eq. 0) then
            call jenonu(jexnom(nomnoe, zk8(jnofos-1 + inoff)), nuno)
        endif
        numfin = nuno
        dmax = 0.d0
        dmin = 100.d0
        nbs = 1
        inos = 1
        zi(jts-1 + inos) = nuno
        x1 = zr(igeom-1+3*(nuno-1)+1)
        y1 = zr(igeom-1+3*(nuno-1)+2)
        z1 = zr(igeom-1+3*(nuno-1)+3)
!
! identification noeuds sur bon cote de la levre (cas fond ferme)
! a partir du noeud le plus proche du fond
        do 210 in = 1, nbtrls
            ino = jnols+zi(jints-1 + in)-1
            if (zi(ino) .eq. nuno) goto 210
            x2 = zr(igeom-1+3*(zi(ino)-1)+1)
            y2 = zr(igeom-1+3*(zi(ino)-1)+2)
            z2 = zr(igeom-1+3*(zi(ino)-1)+3)
            d = sqrt( (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2 )
            if (d .lt. dmin) then
                dmin = d
                numun = zi(ino)
            endif
210      continue
!
        vectan(1) = zr(igeom-1+3*(numun-1)+1)-x1
        vectan(2) = zr(igeom-1+3*(numun-1)+2)-y1
        vectan(3) = zr(igeom-1+3*(numun-1)+3)-z1
!
        do 220 in = 1, nbtrls
            ino = jnols+zi(jints-1 +in)-1
            if (zi(ino) .eq. nuno) goto 220
            x2 = zr(igeom-1+3*(zi(ino)-1)+1)
            y2 = zr(igeom-1+3*(zi(ino)-1)+2)
            z2 = zr(igeom-1+3*(zi(ino)-1)+3)
            d = sqrt( (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2 )
            ps=(x2-x1)*vectan(1)+(y2-y1)*vectan(2)+(z2-z1)*vectan(3)
            if (ps .ge. 0.d0) then
                nbs = nbs+1
                inos = inos+1
                zi(jts-1 + inos) = zi(ino)
                if (d .gt. dmax) then
                    dmax = d
                    numfin = zi(ino)
                endif
            endif
220      continue
        preco = prec*10
        call oreino(noma, zi(jts), nbs, nuno, numfin,&
                    zr(igeom), critn, preco, iera, iret)
!
        do 230 in = 1, min(nbs, 20)
            call jenuno(jexnum(nomnoe, zi(jts-1 + in)), zk8(inols-1 + 20*(inoff-1)+in))
230      continue
!
        call jedetr('&&PKFOND_INTERS_SUP')
        call jedetr('&&PKFOND_TRAV_SUP')
!
200  end do
!
    call jedema()
end subroutine
