subroutine xvetth(ndim, elrefp, nnop, imate, itps,&
                  igeom, temper, lonch, cnset, jpintt,&
                  lsn, lst, basloc, heavt, nfh,&
                  nfe, vectt)
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
!.......................................................................
! aslint: disable=W1306
    implicit none
!
!     BUT: SECOND MEMBRE ELEMENTAIRE EN THERMIQUE LINEAIRE CORRESPONDANT
!          A UN PROBLEME TRANSITOIRE ELEMENTS X-FEM LINEAIRES
!
!          OPTION : 'CHAR_THER_EVOL'
!
! IN :
! ---
! NDIM   --> DIMENSION DE L'ESPACE (2 OU 3)
! ELREFP --> NOM DE L'ELT PARENT DE REFERENCE
! NNOP   --> NBRE DE NOEUDS DE L'ELT PARENT DE REFERENCE
! IMATE  --> ADRESSE DU MATERIAU
! ITPS   --> ADRESSE DES PARAMETRES DE LA DICRETISATION EN TEMPS
! IGEOM  --> ADRESSE DES COORDONEES DES NOEUDS DE L'ELT PARENT
! LONCH  --> LONGUEURS DES CHAMPS UTILISES
! CNSET  --> CONNECTIVITE DES SOUS-ELEMENTS
! PINTT  --> ADRESSE DES COORDONEES DES POINTS D'INTERSECTION
! LSN    --> VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! LST    --> VALEUR DE LA LEVEL SET TANGENTIELLE AUX NOEUDS PARENTS
! BASLOC --> BASE LOCALE AU FOND DE FISSURE
! HEAVT  --> VALEURS DE L'HEAVISIDE SUR LES SS-ELTS
! NFH    --> NBRE DE FONCTION D'ENRICHISSEMENT HEAVISIDE (0 OU 1)
! NFE    --> NBRE DE FONCTION D'ENRICHISSEMENT CRACKTIP  (0 OU 1)
!
! OUT :
! ----
! VECTT  --> VECTEUR ELEMENTAIRE
!.......................................................................
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref5.h"
#include "asterfort/lteatt.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/reeref.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalf2.h"
#include "asterfort/xcalfe.h"
!-----------------------------------------------------------------------
!
    character(len=8) :: elrefp
    integer :: ndim, nnop, imate, itps, igeom, nfh, nfe, jpintt
    integer :: lonch(10), cnset(4*32), heavt(36)
    real(kind=8) :: temper(nnop*(1+nfh+nfe)), lsn(nnop), lst(nnop)
    real(kind=8) :: basloc(*), vectt(*)
!
!-----------------------------------------------------------------------
!
    character(len=8) :: nomres(2), elrese(3), fami(3), poum
    character(len=16) :: phenom
    logical :: axi
    real(kind=8) :: baslog(3*ndim), tem, lsng, lstg, coorse(81), xg(ndim)
    real(kind=8) :: xe(ndim)
    real(kind=8) :: femec(4), dgdmec(4, ndim), feth, ff(nnop), dfdi(nnop, ndim)
    real(kind=8) :: he
    real(kind=8) :: ffenr(nnop, 1+nfh+nfe), deltat, valpar(1), valres(2), lambda
    real(kind=8) :: jac, theta, dgdth(ndim), dffenr(nnop, 1+nfh+nfe, ndim)
    real(kind=8) :: pdscal
    real(kind=8) :: rhocp, dtem(ndim), r
    integer :: ivf, kpg, ibid, nno, npg, j, iret, nse, ise, inp, in, ino, kddl
    integer :: nbddl
    integer :: mxstac, icodre(2), spt, ipoids, idfde, idim, ipos, codret
!
    parameter (mxstac=1000)
!     NBRE MAX DE NOEUDS D'UN SOUS-ELEMENT (TRIA3,TETRA4,TRIA6 -> 6)
!
    data    elrese /'SE2','TR3','TE4'/
    data    fami   /'BID','XINT','XINT'/
!
!-----------------------------------------------------------------------
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    ASSERT(nnop.le.mxstac)
!
!     S'AGIT-IL D'UNE MODELISATION AXIS
    axi = .false.
    if (lteatt('AXIS','OUI')) axi = .true.
!
!     NBRE DE DDLS PAR NOEUD
    nbddl = 1+nfh+nfe
!
!     RECUP DONNEES TEMPORELLES (POUR LE THETA SCHEMA)
    deltat = zr(itps-1+2)
    theta = zr(itps-1+3)
!
!     POUR PREPARER L'APPEL A RCVALB
    call rccoma(zi(imate), 'THER', 1, phenom, codret)
    if (codret .ne. 0) then
        call utmess('F', 'ELEMENTS2_63')
    endif
!     POUR L'INSTANT ON NE TRAITE PAS 'THER_ORTH'
    ASSERT(phenom.eq.'THER')
    valpar(1) = zr(itps-1+1)
    spt = 1
    poum = '+'
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO,NPG,IPOIDS,IVF,IDFDE
    call elref5(elrese(ndim), fami(ndim), ibid, nno, ibid,&
                npg, ipoids, ibid, ivf, idfde,&
                ibid, ibid)
!
!     RECUPERATION DE LA SUBDIVISION DE L'ELEMENT EN NSE SOUS ELEMENT
    nse=lonch(1)
!
! ----------------------------------------------------------------------
! --- BOUCLE SUR LES NSE SOUS-ELEMENTS
! ----------------------------------------------------------------------
!
    do ise = 1, nse
!
!       VALEUR (CSTE) DE LA FONCTION HEAVISIDE SUR LE SS-ELT
        he = 1.d0*heavt(ise)
!
!       BOUCLE SUR LES SOMMETS DU SOUS-TETRA/TRIA -> COORDS NOEUDS
        do in = 1, nno
            ino=cnset(nno*(ise-1)+in)
            do j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else
                    ASSERT(.false.)
                endif
            end do
        end do
!
! ----------------------------------------------------------------------
! ----- BOUCLE SUR LES POINTS DE GAUSS
! ----------------------------------------------------------------------
!
        do kpg = 1, npg
!
!         COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
            call vecini(ndim, 0.d0, xg)
            do j = 1, ndim
                do in = 1, nno
                    xg(j)=xg(j)+zr(ivf-1+nno*(kpg-1)+in)*coorse(ndim*(&
                    in-1)+j)
                end do
            end do
!
!         XG -> XE (DANS LE REPERE DE l'ELREFP) ET VALEURS DES FF EN XE
            call reeref(elrefp, nnop, zr(igeom), xg, ndim, xe, ff, dfdi=dfdi)
!
! ------- SI ENRICHISSEMENT SINGULIER
            if (nfe .gt. 0) then
!           BASE LOCALE ET LEVEL SETS AU POINT DE GAUSS
                call vecini(3*ndim, 0.d0, baslog)
                lsng = 0.d0
                lstg = 0.d0
                do inp = 1, nnop
                    lsng = lsng + lsn(inp) * ff(inp)
                    lstg = lstg + lst(inp) * ff(inp)
                    do j = 1, 3*ndim
                        baslog(j) = baslog(j) + basloc(3*ndim*(inp-1)+ j) * ff(inp)
                    end do
                end do
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
                ASSERT(iret.ne.0)
!           ON NE GARDE QUE LES ENRICHISSEMENTS UTILES EN THERMIQUE
                call vecini(ndim, 0.d0, dgdth)
                feth = femec(1)
                do j = 1, ndim
                    dgdth(j) = dgdmec(1,j)
                end do
            endif
! ------- FIN SI ENRICHISSEMENT SINGULIER
!
!         CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!         AVEC LES COORDONNEES DU SOUS-ELEMENT
            if (ndim .eq. 2) then
                call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                            jac)
            else if (ndim.eq.3) then
                call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                            jac)
            endif
!
!         MODIFICATION DU JACOBIEN SI AXI
            if (axi) then
                r = 0.d0
                do inp = 1, nnop
                    r = r + ff(inp)*zr(igeom-1+2*(inp-1)+1)
                end do
                ASSERT(r.gt.0d0)
                jac = jac * r
            endif
!
!         RECUPERER LES PARAMETRES MATERIAUX
            nomres(1) = 'LAMBDA'
            nomres(2) = 'RHO_CP'
            call rcvalb('XFEM', kpg, spt, poum, zi(imate),&
                        ' ', phenom, 1, 'INST', [valpar],&
                        2, nomres, valres, icodre, 1)
            lambda = valres(1)
            rhocp = valres(2)
!
!         FFENR : TABLEAU DES FF ENRICHIES
!         DFFENR : TABLEAU DES DERIVEES DES FF ENRICHIES
            do inp = 1, nnop
!           DDL CLASSIQUE (TEMP)
                ffenr(inp,1) = ff(inp)
                do j = 1, ndim
                    dffenr(inp,1,j) = dfdi(inp,j)
                end do
!           DDL HEAVISIDE (H1)
                if (nfh .eq. 1) then
                    ffenr(inp,1+nfh) = he*ff(inp)
                    do j = 1, ndim
                        dffenr(inp,1+nfh,j) = he*dfdi(inp,j)
                    end do
                endif
!           DDL CRACK-TIP (E1)
                if (nfe .eq. 1) then
                    ffenr(inp,1+nfh+nfe) = feth*ff(inp)
                    do j = 1, ndim
                        dffenr(inp,1+nfh+nfe,j) = feth*dfdi(inp,j) + ff(inp)*dgdth(j)
                    end do
                endif
            end do
!
!         CALCUL DE T-
            tem = 0.d0
            do inp = 1, nnop
                do kddl = 1, nbddl
                    tem = tem + temper(nbddl*(inp-1)+kddl)*ffenr(inp, kddl)
                end do
            end do
!         CALCUL DE GRAD(T-)
            call vecini(ndim, 0.d0, dtem)
            do inp = 1, nnop
                do kddl = 1, nbddl
                    do idim = 1, ndim
                        dtem(idim) = dtem(idim) + temper(nbddl*(inp-1) +kddl)* dffenr(inp,kddl,id&
                                     &im)
                    end do
                end do
            end do
!
! ------- REMPLISSAGE DU VECTEUR ELEMENTAIRE
!
            ipos = 0
            do inp = 1, nnop
!
                do kddl = 1, nbddl
!
                    ipos = ipos + 1
!
!             TERME DE MASSE
                    vectt(ipos) = vectt(ipos)+rhocp/deltat*jac* ffenr(inp,kddl)*tem
!
!             TERME DE RIGIDITE
                    pdscal = 0.d0
                    do idim = 1, ndim
                        pdscal = pdscal + dffenr(inp,kddl,idim)*dtem( idim)
                    end do
                    vectt(ipos) = vectt(ipos) - (1.0d0-theta)* lambda*jac*pdscal
!
                end do
            end do
!
        end do
!
! ----------------------------------------------------------------------
! ----- FIN BOUCLE SUR LES POINTS DE GAUSS
! ----------------------------------------------------------------------
!
    end do
!
! ----------------------------------------------------------------------
! --- FIN BOUCLE SUR LES SOUS-ELEMENTS
! ----------------------------------------------------------------------
!
end subroutine
