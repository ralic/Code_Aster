subroutine xrechp(ndim, elrefp, nnop, igeom, itps,&
                  ihechp, jptint, jcface, jlonch,&
                  jlst, jbasec, nfh, nfe, fonree,&
                  imattt, heavn)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT: THERMIQUE LINEAIRE / ELEMENTS PRINCIPAUX X-FEM LINEAIRES
!          ECHANGE_PAROI POUR FISSURES X-FEM
!
!          CALCUL DE 'RIGI_THER_PARO_F' ET 'RIGI_THER_PARO_R' (SOUS-TE)
!
! IN :
! ---
! NDIM   --> DIMENSION DE L'ESPACE (2 OU 3)
! ELREFP --> NOM DE L'ELT PARENT DE REFERENCE
! NNOP   --> NBRE DE NOEUDS DE L'ELT PARENT DE REFERENCE
! IGEOM  --> ADRESSE DES COORDONEES DES NOEUDS DE L'ELT PARENT
! ITEMPS --> ADRESSE DES PARAMETRES DE LA DICRETISATION EN TEMPS
! IHECHP --> ADRESSE DU COEFFICIENT ENCHANGE PAROI
! JPTINT --> ADRESSE DU VECTEUR DES POINTS D'INTERSECTION (*)
! JAINT  --> ADRESSE DU VECTEUR DES ARRETES INTERSECTEES
! JCFACE --> ADRESSE DU VECTEUR DE CONNECTIVITE DES FACETTES
! JLONCH --> ADRESSE DU VECTEUR DE LONGUEUR DES CHAMPS
! JLST   --> ADRESSE DE LA LEVEL SET TANGENTIELLE
! JBASEC --> ADRESSE DU VECTEUR DE LA BASE COVARIANTE DES FACETTES
! NFH    --> NBRE DE FONCTION D'ENRICHISSEMENT HEAVISIDE (0 OU 1)
! NFE    --> NBRE DE FONCTION D'ENRICHISSEMENT CRACKTIP  (0 OU 1)
! FONREE --> 'FONC' OU 'REEL'
!
! OUT :
! ----
! IMATTT --> ADRESSE DE LA MATRICE ELEMENTAIRE
!.......................................................................
#include "asterf_types.h"
#include "jeveux.h"
!-----------------------------------------------------------------------
!
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/lteatt.h"
#include "asterfort/xjacf2.h"
#include "asterfort/xjacff.h"
#include "asterfort/xxmmvd.h"
#include "asterfort/xcalc_saut.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_heav.h"
    character(len=4) :: fonree
    character(len=8) :: elrefp
    integer :: ndim, nnop, igeom, itps, ihechp, jptint, jcface, jlonch
    integer :: jlst, jbasec, nfh, nfe, imattt, heavn(27,5)
!
!-----------------------------------------------------------------------
!
    character(len=8) :: fpg, elc, elrefc, nompar(4)
    aster_logical :: axi
    integer :: nbddl, zxain
    integer :: cface(30, 6), ninter, nface, nptf
    integer :: i, j, ifa, nnof, npgf, ipoidf, ivff, idfdef
    integer :: ipgf, ilev, inp, jnp, kddl, lddl, ind1, ind2, iddlma, ier
    integer :: mxstac, hea_fa(2), ifh, ife
!
    parameter (mxstac=1000)
!
    real(kind=8) :: theta, he(2), xg(4), jac, ff(27), r27bid(27), nd(3)
    real(kind=8) :: dfbid(27, 3), r3bid(3), lst, rr(2), ffenr(nnop, 1+nfh+nfe)
    real(kind=8) :: hechp, r8tmp, r
!
!     PAR CONVENTION :
!     LEVRE INFERIEURE (HE=-1) EST LA LEVRE 1, DE NORMALE SORTANTE  ND
!     LEVRE SUPERIEURE (HE=+1) EST LA LEVRE 2, DE NORMALE SORTANTE -ND
    data    he / -1.d0 , 1.d0/
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    ASSERT(nnop.le.mxstac .and. 1+nfh+nfe.le.mxstac)
!
!    DEFINITION A LA MAIN DE LA TOPOLOGIE DE SOUS-DOMAINE PAR FACETTE (SI NFISS=1)
    do ilev=1,2
      hea_fa(ilev)=xcalc_code(1,he_real=[he(ilev)])
    enddo
!
!     S'AGIT-IL D'UNE MODELISATION AXIS
    axi = .false.
    if (lteatt('AXIS','OUI')) axi = .true.
!
!     RECUP DU PARAMETRE THETA (POUR LE THETA SCHEMA)
    theta = zr(itps-1+3)
!
!     NBRE DE DDLS PAR NOEUD PARENT
    nbddl = 1+nfh+nfe
!
!     LONGUEUR FIXE DU VECTEUR D'ADRESSE JAINT
    zxain = xxmmvd('ZXAIN')
!
!     ELREFE ET FPG POUR LES FACETTES
    if (ndim .eq. 3) then
        elc='TR3'
        fpg='XCON'
    else if (ndim.eq.2) then
        elc='SE2'
        fpg='MASS'
    endif
!
!     RECUPERATIONS DES DONNEES SUR LA TOPOLOGIE DES FACETTES
    ninter=zi(jlonch-1+1)
    nface =zi(jlonch-1+2)
    nptf  =zi(jlonch-1+3)
    if (ninter .lt. ndim) goto 999
    do 11 i = 1, nface
        do 12 j = 1, nptf
            cface(i,j)=zi(jcface-1+ndim*(i-1)+j)
 12     continue
 11 continue
!
!-----------------------------------------------------------------------
!     BOUCLE SUR LES FACETTES
!-----------------------------------------------------------------------
!
    do 100 ifa = 1, nface
!
        call elrefe_info(elrefe=elc,fami=fpg,nno=nnof,&
  npg=npgf,jpoids=ipoidf,jvf=ivff,jdfde=idfdef)
!
!-----------------------------------------------------------------------
!       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
!-----------------------------------------------------------------------
!
        do 200 ipgf = 1, npgf
!
!         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
!         ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
!         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
!         ET DE XG : COORDONNEES REELLES DU POINT DE GAUSS
            elrefc='NON'
            if (ndim .eq. 3) then
                call xjacff(elrefp, elrefc, elc, ndim, fpg,&
                            jptint, ifa, cface, ipgf, nnop,&
                            igeom, jbasec, xg, jac, ff,&
                            r27bid, dfbid, nd, r3bid, r3bid)
            else if (ndim.eq.2) then
                call xjacf2(elrefp, elrefc, elc, ndim, fpg,&
                            jptint, ifa, cface, nptf, ipgf,&
                            nnop, igeom, jbasec, xg, jac,&
                            ff, r27bid, dfbid, nd, r3bid)
            endif
!
!         CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
            if (nfe .eq. 1) then
                lst=0.d0
                do 210 i = 1, nnop
                    lst=lst+zr(jlst-1+i)*ff(i)
210             continue
                ASSERT(lst.lt.0.d0)
                rr(1)=-sqrt(-lst)
                rr(2)= sqrt(-lst)
            endif
!
!         VALEUR DU COEFFICIENT ECHANGE PAROI
            if (fonree .eq. 'REEL') then
                hechp = zr(ihechp)
            else if (fonree.eq.'FONC') then
                nompar(1)='X'
                nompar(2)='Y'
                if (ndim .eq. 3) nompar(3)='Z'
                if (ndim .eq. 3) nompar(4)='INST'
                if (ndim .eq. 2) nompar(3)='INST'
                xg(ndim+1) = zr(itps)
                call fointe('FM', zk8(ihechp), ndim+1, nompar, xg,&
                            hechp, ier)
            else
                ASSERT(.false.)
            endif
!
!         MODIFICATION DU JACOBIEN SI AXI
            if (axi) then
                r = 0.d0
                do 220 inp = 1, nnop
                    r = r + ff(inp)*zr(igeom-1+2*(inp-1)+1)
220             continue
                ASSERT(r.gt.0d0)
                jac = jac * r
            endif
!
!-----------------------------------------------------------------------
!         BOUCLE SUR LES (DEUX) LEVRES DE LA FISSURES
!-----------------------------------------------------------------------
!
            do 300 ilev = 1, 2
!
!           FFENR : TABLEAU DES FF ENRICHIES
                do 310 i = 1, nnop
!             DDL CLASSIQUE (TEMP)
                    ffenr(i,1) = ff(i)
!             DDL HEAVISIDE (H1)
                    if (nfh .eq. 1) then
                        ffenr(i,1+nfh) = xcalc_heav(heavn(i,1),hea_fa(ilev),heavn(i,5))*ff(i)
                    endif
!             DDL CRACK-TIP (E1)
                    if (nfe .eq. 1) then
                        ffenr(i,1+nfh+nfe) = rr(ilev)*ff(i)
                    endif
310             continue
!
!           REMPLISSAGE DE LA MATRICE
                do 400 inp = 1, nnop
                    do 410 kddl = 1, nbddl
!
                        ind1 = (nbddl*(inp-1)+kddl-1) * (nbddl*(inp-1) +kddl ) /2
!
                        do 510 jnp = 1, inp
!
!                   IDDLMA : NUMERO DE DDL MAX PR NE PAS DEPASSER LA
!                   DIGAONALE (ON STOCKE LA PARTIE TRIANGULAIRE INF)
                            if (jnp .eq. inp) then
                                 iddlma = kddl
                            else
                                 iddlma = nbddl
                            endif
!
                            do ifh = 1, nfh
                                lddl=1+ifh
                                if (lddl .le. iddlma) then
!                     ON ECRIT PROPREMENT LES TERMES DE COUPLAGE COMPTE TENU
!                      * DU SAUT DE DEPLACEMENT SUR LES TERMES HEAVISIDES
!                      * DE LA CONVENTION D ECRITURE DU SAUT SUR CHAQUE FRONTIERE 
!                               - FRONTIERE 1 : 1-->2
!                               - FRONTIERE 2 : 2-->1
                                      if (ilev .eq. 1) then
                                       r8tmp = (xcalc_heav(heavn(jnp,1),hea_fa(1),heavn(jnp,5))&
                                               -xcalc_heav(heavn(jnp,1),hea_fa(2),heavn(jnp,5))&
                                              )*ff(jnp)
                                      elseif (ilev .eq. 2) then
                                       r8tmp = (xcalc_heav(heavn(jnp,1),hea_fa(2),heavn(jnp,5))&
                                               -xcalc_heav(heavn(jnp,1),hea_fa(1),heavn(jnp,5))&
                                              )*ff(jnp)
                                      endif
                                      ind2 = ind1 + nbddl*(jnp-1)+lddl
                                      zr(imattt-1+ind2) = zr(imattt-1+ ind2) + theta*hechp*jac&
                                                        &* ffenr(inp,kddl)*r8tmp
                                  endif
                              enddo
!
                              do ife = 1, nfe
                                lddl=1+nfh+ife
                                if (lddl .le. iddlma) then
                                      r8tmp = 2.d0*rr(ilev)*ff(jnp)
                                      ind2 = ind1 + nbddl*(jnp-1)+lddl
                                      zr(imattt-1+ind2) = zr(imattt-1+ ind2) + theta*hechp*jac&
                                                        &* ffenr(inp,kddl)*r8tmp
                                endif
                              enddo
!
510                     continue
410                 continue
400             continue
!
300         continue
!
!-----------------------------------------------------------------------
!         FIN BOUCLE SUR LES (DEUX) LEVRES DE LA FISSURES
!-----------------------------------------------------------------------
!
200     continue
!-----------------------------------------------------------------------
!       FIN BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
!-----------------------------------------------------------------------
!
100 continue
!-----------------------------------------------------------------------
!     FIN BOUCLE SUR LES FACETTES
!-----------------------------------------------------------------------
!
999 continue
!
end subroutine
