subroutine te0366(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit   none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elelin.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/matini.h"
#include "asterfort/ttprsm.h"
#include "asterfort/vecini.h"
#include "asterfort/xlacti.h"
#include "asterfort/xmelet.h"
#include "asterfort/xmmaa0.h"
#include "asterfort/xmmaa1.h"
#include "asterfort/xmmab0.h"
#include "asterfort/xmmab1.h"
#include "asterfort/xmmab2.h"
#include "asterfort/xmmjac.h"
#include "asterfort/xmmjeu.h"
#include "asterfort/xmoffc.h"
#include "asterfort/xmpint.h"
#include "asterfort/xtcaln.h"
#include "asterfort/xtdepm.h"
#include "asterfort/xtedd2.h"
#include "asterfort/xtform.h"
#include "asterfort/xtlagc.h"
#include "asterfort/xtlagf.h"
    character(len=16) :: option, nomte
!
! ----------------------------------------------------------------------
!  CALCUL CALCUL DES MATRICES DE CONTACT ET DE FROTTEMENT
!  DE COULOMB STANDARD POUR LA METHODE XFEM EN GRANDS GLISSEMENTS
!
!  OPTION : 'RIGI_CONT' (CALCUL DES MATRICES DE CONTACT )
!           'RIGI_FROT' (CALCUL DES MATRICES DE FROTTEMENT STANDARD)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!
!
!
    integer :: n
    parameter    (n=336)
!
    integer :: i, j, ij, nfaes
    integer :: ndim, nddl, nnc
    integer :: nsinge, nsingm
    integer :: imatt, jpcpo
    integer :: jpcpi, jpcai, jpccf, jstno
    integer :: indnor, ifrott, indco
    integer :: cface(5, 3)
    integer :: jdepde, jdepm, jgeom, jheafa, jheano
    real(kind=8) :: mmat(n, n), tau1(3), tau2(3), norm(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: coore(3), coorm(3), coorc(2)
    real(kind=8) :: ffe(20), ffm(20), ffc(8), dffc(2, 8)
    real(kind=8) :: jacobi, hpg
    character(len=8) :: elrees, elrema, elreco, typmai, typmec
    integer :: inadh, nvit, lact(8), nlact, ninter
    real(kind=8) :: geopi(9), dvitet(3)
    real(kind=8) :: coefff, coefcr, coeffr, coeffp
    real(kind=8) :: coefcp, rese(3), nrese
    real(kind=8) :: rre, rrm, jeu, r8bid
    real(kind=8) :: ddeple(3), ddeplm(3), dlagrc, dlagrf(2)
    logical :: lpenaf, lesclx, lmaitx, lcontx, lpenac
    integer :: contac, ibid, npte
    integer :: ndeple, nne(3), nnm(3), ddle(2), ddlm(2), nfhe, nfhm
    real(kind=8) :: ffec(8)
    logical :: lmulti
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INFOS SUR LA MAILLE DE CONTACT
!
    call xmelet(nomte, typmai, elrees, elrema, elreco,&
                ndim, nddl, nne, nnm, nnc,&
                ddle, ddlm, contac, ndeple, nsinge,&
                nsingm, nfhe, nfhm)
!
    call assert(nddl.le.n)
    lmulti = .false.
    if (nfhe .gt. 1 .or. nfhm .gt. 1) lmulti = .true.
!
! --- INITIALISATIONS
!
    call matini(n, n, 0.d0, mmat)
    call vecini(9, 0.d0, geopi)
    call vecini(2, 0.d0, dlagrf)
    call vecini(3, 0.d0, ddeple)
    call vecini(3, 0.d0, ddeplm)
    dlagrc = 0.d0
    jeu = 0.d0
!
! --- RECUPERATION DES DONNEES DE LA CARTE CONTACT 'POINT' (VOIR MMCART)
!
    call jevech('PCAR_PT', 'L', jpcpo)
    coorc(1) = zr(jpcpo-1+1)
    coorc(2) = zr(jpcpo-1+10)
    tau1(1) = zr(jpcpo-1+4)
    tau1(2) = zr(jpcpo-1+5)
    tau1(3) = zr(jpcpo-1+6)
    tau2(1) = zr(jpcpo-1+7)
    tau2(2) = zr(jpcpo-1+8)
    tau2(3) = zr(jpcpo-1+9)
    indco = nint(zr(jpcpo-1+11))
    ninter = nint(zr(jpcpo-1+31))
    coefcr = zr(jpcpo-1+13)
    coeffr = zr(jpcpo-1+14)
    coefff = zr(jpcpo-1+15)
    coefcp = zr(jpcpo-1+33)
    coeffp = zr(jpcpo-1+34)
    ifrott = nint(zr(jpcpo-1+16))
    indnor = nint(zr(jpcpo-1+17))
    hpg = zr(jpcpo-1+19)
! --- LE NUMERO DE LA FACETTE DE CONTACT ESCLAVE
    npte = nint(zr(jpcpo-1+12))
    nfaes = nint(zr(jpcpo-1+22))
! --- LES COORDONNEES ESCLAVE ET MAITRES DANS L'ELEMENT PARENT
    coore(1) = zr(jpcpo-1+24)
    coore(2) = zr(jpcpo-1+25)
    coore(3) = zr(jpcpo-1+26)
    coorm(1) = zr(jpcpo-1+27)
    coorm(2) = zr(jpcpo-1+28)
    coorm(3) = zr(jpcpo-1+29)
! --- POINT D'INTEGRATION VITAL OU PAS
    nvit = nint(zr(jpcpo-1+30))
! --- SQRT LSN PT ESCLAVE ET MAITRE
    rre = zr(jpcpo-1+18)
    rrm = zr(jpcpo-1+23)
    if (nnm(1) .eq. 0) rre = 2*rre
!
    lpenaf=((coeffr.eq.0.d0).and.(coeffp.ne.0.d0))
    lpenac=((coefcr.eq.0.d0).and.(coefcp.ne.0.d0))
!
! --- RECUPERATION DES DONNEES DE LA CARTE CONTACT PINTER (VOIR XMCART)
!
    call jevech('PCAR_PI', 'L', jpcpi)
!
! --- RECUPERATION DES DONNEES DE LA CARTE CONTACT AINTER (VOIR XMCART)
!
    call jevech('PCAR_AI', 'L', jpcai)
!
! --- RECUPERATION DES DONNEES DE LA CARTE CONTACT CFACE (VOIR XMCART)
!
    call jevech('PCAR_CF', 'L', jpccf)
!
! --- RECUPERATION DE LA GEOMETRIE ET DES CHAMPS DE DEPLACEMENT
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PDEPL_P', 'L', jdepde)
    call jevech('PDEPL_M', 'L', jdepm)
!
    if (lmulti) then
!
! --- RECUPERATION DES FONCTION HEAVISIDES SUR LES FACETTES
!
        call jevech('PHEAVFA', 'L', jheafa)
!
! --- RECUPERATION DE LA PLACE DES LAGRANGES
!
        call jevech('PHEAVNO', 'L', jheano)
    else
        jheafa=1
        jheano=1
    endif
!
! --- ON CONSTRUIT LA MATRICE DE CONNECTIVITÉ CFACE (MAILLE ESCLAVE)
! --- CE QUI SUIT N'EST VALABLE QU'EN 2D POUR LA FORMULATION QUADRATIQUE
! --- EN 3D ON UTILISE SEULEMENT LA FORMULATION AUX NOEUDS SOMMETS,
! --- CETTE MATRICE EST DONC INUTILE, ON NE LA CONSTRUIT PAS !!!
!
    cface(1,1) = 1
    cface(1,2) = 2
!
! --- CALCUL DES COORDONNEES REELLES DES POINTS D'INTERSECTION ESCLAVES
!
    call xmpint(ndim, npte, nfaes, jpcpi, jpccf,&
                geopi)
!
! --- FONCTIONS DE FORME
!
    call xtform(ndim, elrees, elrema, elreco, ndeple,&
                nnm(1), nnc, coore, coorm, coorc,&
                ffe, ffm, dffc)
!
! --- FONCTION DE FORMES POUR LES LAGRANGIENS
! --- SI ON EST EN LINEAIRE, ON IMPOSE QUE LE NB DE NOEUDS DE CONTACTS
! --- ET LES FFS LAGRANGES DE CONTACT SONT IDENTIQUES A CEUX
! --- DES DEPLACEMENTS DANS LA MAILLE ESCLAVE POUR LE CALCUL DES CONTRIB
!
    if (contac .eq. 1) then
        nnc = nne(2)
        call xlacti(typmai, ninter, jpcai, lact, nlact)
        call xmoffc(lact, nlact, nnc, ffe, ffc)
    else if (contac.eq.3) then
        nnc = nne(2)
        call elelin(contac, elrees, typmec, ibid, ibid)
        call elrfvf(typmec, coore, nnc, ffec, ibid)
        call xlacti(typmai, ninter, jpcai, lact, nlact)
        call xmoffc(lact, nlact, nnc, ffec, ffc)
    else
        call assert(contac.eq.0)
    endif
!
! --- JACOBIEN POUR LE POINT DE CONTACT
!
    call xmmjac(elreco, geopi, dffc, jacobi)
!
! --- CALCUL DE LA NORMALE ET DES MATRICES DE PROJECTION
!
    call xtcaln(ndim, tau1, tau2, norm, mprojt)
!
! --- NOEUDS EXCLUS PAR PROJECTION HORS ZONE
!
    if (indnor .eq. 1) then
        indco = 0
    endif
!
! --- CALCUL DES MATRICES DE CONTACT
!
    if (option .eq. 'RIGI_CONT') then
        if (indco .eq. 1) then
!
! --- CALCUL DES MATRICES A, AT, AU - CAS DU CONTACT
!
            call xmmaa1(ndim, nne, ndeple, nnc, nnm,&
                        nfaes, cface, hpg, ffc, ffe,&
                        ffm, jacobi, jpcai, coefcr, coefcp,&
                        lpenac, norm, typmai, nsinge, nsingm,&
                        rre, rrm, contac, ddle, ddlm,&
                        nfhe, nfhm, lmulti, zi(jheano), zi(jheafa),&
                        mmat)
!
        else if (indco .eq. 0) then
            if (nvit .eq. 1) then
!
! --- CALCUL DE LA MATRICE C - CAS SANS CONTACT
!
                call xmmaa0(ndim, nnc, nne, hpg, nfaes,&
                            cface, ffc, jacobi, jpcai, coefcr,&
                            coefcp, lpenac, typmai, ddle, contac,&
                            nfhe, lmulti, zi(jheano), mmat)
!
            endif
        else
            call assert(.false.)
        endif
!
    else if (option.eq.'RIGI_FROT') then
!
! --- CALCUL DES INCREMENTS - LAGRANGE DE CONTACT
!
        call xtlagc(typmai, ndim, nnc, nne, ddle(1),&
                    nfaes, cface, jdepde, jpcai, ffc,&
                    contac, nfhe, lmulti, zi(jheano), dlagrc)
!
        if (coefff .eq. 0.d0) indco = 0
! ON MET LA SECURITE DANS LE CAS PENALISE EGALEMENT
        if (dlagrc .eq. 0.d0) indco = 0
        if (ifrott .ne. 3) indco = 0
!
        if (indco .eq. 0) then
            if (nvit .eq. 1) then
!
! --- CALCUL DE LA MATRICE F - CAS SANS FROTTEMENT
!
                call xmmab0(ndim, nnc, nne, nfaes, jpcai,&
                            hpg, ffc, jacobi, coefcr, lpenac,&
                            typmai, cface, tau1, tau2, ddle,&
                            contac, nfhe, lmulti, zi(jheano), mmat)
            endif
        else if (indco.eq.1) then
!
! --- CALCUL DES INCREMENTS - DEPLACEMENTS
!
            call xtdepm(ndim, nnm, nne, ndeple, nsinge,&
                        nsingm, ffe, ffm, jdepde, rre,&
                        rrm, ddle, ddlm, ddeple, ddeplm)
!
! --- CALCUL DES INCREMENTS - LAGRANGE DE FROTTEMENT
!
            call xtlagf(typmai, ndim, nnc, nne, ddle,&
                        nfaes, cface, jdepde, jpcai, ffc,&
                        contac, nfhe, dlagrf)
!
! --- ON CALCULE L'ETAT DE CONTACT ADHERENT OU GLISSANT
!
            call ttprsm(ndim, ddeple, ddeplm, dlagrf, coeffr,&
                        tau1, tau2, mprojt, inadh, rese,&
                        nrese, coeffp, lpenaf, dvitet)
!
! --- CALCUL DU JEU
!
            jeu =0.d0
            if (ndim .eq. 3 .and. contac .eq. 3) then
                call xmmjeu(ndim, nnm, nne, ndeple, nsinge,&
                            nsingm, ffe, ffm, norm, jgeom,&
                            jdepde, jdepm, rre, rrm, ddle,&
                            ddlm, nfhe, nfhm, lmulti, zi(jheafa),&
                            jeu)
            endif
!
!
!
            if (inadh .eq. 1) then
!
! --- CALCUL DE B, BT, BU - CAS ADHERENT
!
                call xmmab1(ndim, nne, ndeple, nnc, nnm,&
                            nfaes, cface, hpg, ffc, ffe,&
                            ffm, jacobi, jpcai, dlagrc, coefcr,&
                            coefcp, dvitet, coeffr, dlagrf, jeu,&
                            coeffp, coefff, lpenaf, tau1, tau2,&
                            rese, mprojt, norm, typmai, nsinge,&
                            nsingm, rre, rrm, nvit, contac,&
                            ddle, ddlm, nfhe, mmat)
!
            else if (inadh.eq.0) then
!
! --- CALCUL DE B, BT, BU, F - CAS GLISSANT
!
                call xmmab2(ndim, nne, ndeple, nnc, nnm,&
                            nfaes, cface, hpg, ffc, ffe,&
                            ffm, jacobi, jpcai, dlagrc, coefcr,&
                            coefcp, coeffr, dlagrf, jeu, coeffp,&
                            lpenaf, coefff, tau1, tau2, rese,&
                            nrese, mprojt, norm, typmai, nsinge,&
                            nsingm, rre, rrm, nvit, contac,&
                            ddle, ddlm, nfhe, mmat)
            endif
        else
            call assert(.false.)
        endif
    else
        call assert(option.eq.'RIGI_FROT' .or. option.eq.'RIGI_CONT')
    endif
!
! --- SUPPRESSION DES DDLS SUPERFLUS (CONTACT ET XHTC)
!
    lesclx = nsinge.eq.1.and.nnm(1).ne.0
    lmaitx = nsingm.eq.1
    lcontx = (contac.eq.1.or.contac.eq.3).and.nlact.lt.nne(2)
    call jevech('PSTANO', 'L', jstno)
    call xtedd2(ndim, nne, ndeple, nnm, nddl,&
                option, lesclx, lmaitx, lcontx, zi(jstno),&
                lact, ddle, ddlm, nfhe, nfhm,&
                lmulti, zi(jheano), mmat, r8bid)
!
! --- RECOPIE VALEURS FINALES (SYMETRIQUE OU NON)
!
    if (lpenac .or. lpenaf) then
        call jevech('PMATUNS', 'E', imatt)
        do 801 j = 1, nddl
            do 811 i = 1, nddl
                ij = nddl*(i-1) + j
                zr(imatt+ij-1) = mmat(i,j)
811          continue
801      continue
        else if ((option.eq.'RIGI_CONT').or.(coefff.eq.0.d0) .or.(&
    ifrott.ne.3)) then
        call jevech('PMATUUR', 'E', imatt)
        do 700 j = 1, nddl
            do 710 i = 1, j
                ij = (j-1)*j/2 + i
                zr(imatt+ij-1) = mmat(i,j)
710          continue
700      continue
    else
        call jevech('PMATUNS', 'E', imatt)
        do 800 j = 1, nddl
            do 810 i = 1, nddl
                ij = nddl*(i-1) + j
                zr(imatt+ij-1) = mmat(i,j)
810          continue
800      continue
    endif
!
    call jedema()
end subroutine
