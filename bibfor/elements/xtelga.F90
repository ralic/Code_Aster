subroutine xtelga(ndim, elrefp, nnop, igeom, tempno,&
                  lonch, cnset, jpintt, lsn, lst,&
                  basloc, heavt, nfh, nfe, temppg)
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
! person_in_charge: sam.cuvilliez at edf.fr
! aslint: disable=W1306
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/elref5.h'
    include 'asterfort/reeret.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xcalf2.h'
    include 'asterfort/xcalfe.h'
    character(len=8) :: elrefp
    integer :: ndim, nnop, igeom, nfh, nfe, jpintt
    integer :: lonch(10), cnset(4*32), heavt(36)
    real(kind=8) :: tempno(nnop*(1+nfh+nfe)), lsn(nnop), lst(nnop)
    real(kind=8) :: basloc(*), temppg(*)
!
!-----------------------------------------------------------------------
!
!     BUT: THERMIQUE LINEAIRE / ELEMENTS X-FEM LINEAIRES
!          CALCUL DE L'OPTION : 'TEMP_ELGA'
!
! IN :
! ---
! NDIM   --> DIMENSION DE L'ESPACE (2 OU 3)
! ELREFP --> NOM DE L'ELT PARENT DE REFERENCE
! NNOP   --> NBRE DE NOEUDS DE L'ELT PARENT DE REFERENCE
! IGEOM  --> ADRESSE DES COORDONEES DES NOEUDS DE L'ELT PARENT
! TEMPNO --> TEMPERATURE AUX NOEUDS
! LONCH  --> LONGUEURS DES CHAMPS UTILISES
! CNSET  --> CONNECTIVITE DES SOUS-ELEMENTS
! JPINTT --> ADRESSE DES COORDONEES DES POINTS D'INTERSECTION
! LSN    --> VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! LST    --> VALEUR DE LA LEVEL SET TANGENTIELLE AUX NOEUDS PARENTS
! BASLOC --> BASE LOCALE AU FOND DE FISSURE
! HEAVT  --> VALEURS DE L'HEAVISIDE SUR LES SS-ELTS
! NFH    --> NBRE DE FONCTION D'ENRICHISSEMENT HEAVISIDE (0 OU 1)
! NFE    --> NBRE DE FONCTION D'ENRICHISSEMENT CRACKTIP  (0 OU 1)
!
! OUT :
! ----
! TEMPPG --> TEMPERATURE AUX POINTS DE GAUSS
!
!-----------------------------------------------------------------------
!
    character(len=8) :: elrese(3), fami(3)
    real(kind=8) :: baslog(3*ndim), tem, lsng, lstg, coorse(81), xg(ndim)
    real(kind=8) :: xe(ndim)
    real(kind=8) :: femec(4), dgdmec(4, ndim), feth, ff(nnop), dfdi(nnop, ndim)
    real(kind=8) :: he
    real(kind=8) :: ffenr(nnop, 1+nfh+nfe)
    integer :: ivf, kpg, ibid, nno, npg, j, iret, nse, ise, inp, in, ino, kddl
    integer :: nbddl
    integer :: mxstac
!
    parameter   (mxstac=1000)
!
    data    elrese /'SE2','TR3','TE4'/
    data    fami   /'BID','XINT','XINT'/
!
!-----------------------------------------------------------------------
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    call assert(nnop.le.mxstac)
!
!     NBRE DE DDLS PAR NOEUD
    nbddl = 1+nfh+nfe
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO,NPG,IVF
    call elref5(elrese(ndim), fami(ndim), ibid, nno, ibid,&
                npg, ibid, ibid, ivf, ibid,&
                ibid, ibid)
!
!     RECUPERATION DE LA SUBDIVISION DE L'ELEMENT EN NSE SOUS ELEMENT
    nse=lonch(1)
!
! ----------------------------------------------------------------------
! --- BOUCLE SUR LES NSE SOUS-ELEMENTS
! ----------------------------------------------------------------------
!
    do 1000 ise = 1, nse
!
!       VALEUR (CSTE) DE LA FONCTION HEAVISIDE SUR LE SS-ELT
        he = 1.d0*heavt(ise)
!
!       BOUCLE SUR LES SOMMETS DU SOUS-TETRA/TRIA -> COORDS NOEUDS
        do 1100 in = 1, nno
            ino=cnset(nno*(ise-1)+in)
            do 1110 j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else
                    call assert(.false.)
                endif
1110          continue
1100      continue
!
! ----------------------------------------------------------------------
! ----- BOUCLE SUR LES POINTS DE GAUSS
! ----------------------------------------------------------------------
!
        do 1200 kpg = 1, npg
!
!         COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
            call vecini(ndim, 0.d0, xg)
            do 1210 j = 1, ndim
                do 1211 in = 1, nno
                    xg(j)=xg(j)+zr(ivf-1+nno*(kpg-1)+in)*coorse(ndim*(&
                    in-1)+j)
1211              continue
1210          continue
!
!         XG -> XE (DANS LE REPERE DE l'ELREFP) ET VALEURS DES FF EN XE
            call vecini(ndim, 0.d0, xe)
            call reeret(elrefp, nnop, zr(igeom), xg, ndim,&
                        'OUI', xe, ff, dfdi)
!
! ------- SI ENRICHISSEMENT SINGULIER
            if (nfe .gt. 0) then
!           BASE LOCALE ET LEVEL SETS AU POINT DE GAUSS
                call vecini(3*ndim, 0.d0, baslog)
                lsng = 0.d0
                lstg = 0.d0
                do 1220 inp = 1, nnop
                    lsng = lsng + lsn(inp) * ff(inp)
                    lstg = lstg + lst(inp) * ff(inp)
                    do 1221 j = 1, 3*ndim
                        baslog(j) = baslog(j) + basloc(3*ndim*(inp-1)+ j) * ff(inp)
1221                  continue
1220              continue
!           FONCTION D'ENRICHISSEMENT (MECA) AU PG ET DÉRIVÉES
                if (ndim .eq. 2) then
                    call xcalf2(he, lsng, lstg, baslog, femec,&
                                dgdmec, iret)
                else if (ndim.eq.3) then
                    call xcalfe(he, lsng, lstg, baslog, femec,&
                                dgdmec, iret)
                endif
!           PB DE CALCUL DES DERIVEES DES FONCTIONS SINGULIERES
!           CAR ON SE TROUVE SUR LE FOND DE FISSURE
                call assert(iret.ne.0)
!           ON NE GARDE QUE LES ENRICHISSEMENTS UTILES EN THERMIQUE
                feth = femec(1)
            endif
! ------- FIN SI ENRICHISSEMENT SINGULIER
!
!         FFENR : TABLEAU DES FF ENRICHIES
            do 1250 inp = 1, nnop
!           DDL CLASSIQUE (TEMP)
                ffenr(inp,1) = ff(inp)
!           DDL HEAVISIDE (H1)
                if (nfh .eq. 1) then
                    ffenr(inp,1+nfh) = he*ff(inp)
                endif
!           DDL CRACK-TIP (E1)
                if (nfe .eq. 1) then
                    ffenr(inp,1+nfh+nfe) = feth*ff(inp)
                endif
1250          continue
!
!         CALCUL DE TEMP AU PG
            tem = 0.d0
            do 1270 inp = 1, nnop
                do 1271 kddl = 1, nbddl
                    tem = tem + tempno(nbddl*(inp-1)+kddl)*ffenr(inp, kddl)
1271              continue
1270          continue
!
!         ECRITURE DE TEMP AU PG
            temppg(npg*(ise-1)+kpg) = tem
!
1200      continue
!
! ----------------------------------------------------------------------
! ----- FIN BOUCLE SUR LES POINTS DE GAUSS
! ----------------------------------------------------------------------
!
1000  continue
!
! ----------------------------------------------------------------------
! --- FIN BOUCLE SUR LES SOUS-ELEMENTS
! ----------------------------------------------------------------------
!
end subroutine
