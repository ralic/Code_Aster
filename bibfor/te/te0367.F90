subroutine te0367(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elelin.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/normev.h"
#include "asterfort/ttprsm.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xlacti.h"
#include "asterfort/xmelet.h"
#include "asterfort/xmmjac.h"
#include "asterfort/xmmjeu.h"
#include "asterfort/xmoffc.h"
#include "asterfort/xmpint.h"
#include "asterfort/xmvec0.h"
#include "asterfort/xmvec1.h"
#include "asterfort/xmvef0.h"
#include "asterfort/xmvef1.h"
#include "asterfort/xtcaln.h"
#include "asterfort/xtdepm.h"
#include "asterfort/xtedd2.h"
#include "asterfort/xtform.h"
#include "asterfort/xtlagm.h"
!
    character(len=16) :: option, nomte
!
! ----------------------------------------------------------------------
!  CALCUL DES SECONDS MEMBRES DE CONTACT ET DE FROTTEMENT DE COULOMB STD
!   POUR LA METHODE XFEM EN GRANDS GLISSEMENTS
!  OPTION : 'CHAR_MECA_CONT' (CALCUL DU SECOND MEMBRE DE CONTACT)
!           'CHAR_MECA_FROT' (CALCUL DU SECOND MEMBRE DE
!                              FROTTEMENT STANDARD )
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
    integer :: i, nfaes
    integer :: ndim, nddl, nne(3), nnm(3), nnc
    integer :: nsinge, nsingm
    integer :: jpcpo, jpcpi, jpcai, jpccf, ivect, jstno
    integer :: indnor, ifrott, indco
    integer :: jdepde, jdepm, jgeom, jheafa, jheano, jheavn, ncompn, jtab(7), iret
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: coore(3), coorm(3), coorc(2)
    real(kind=8) :: ffe(20), ffm(20), ffc(9), dffc(3, 9)
    real(kind=8) :: jacobi, hpg
    character(len=8) :: elrees, elrema, elreco, typmai, typmec
    integer :: inadh, nvit, lact(8), nlact, ninter
    real(kind=8) :: geopi(18), dvitet(3)
    real(kind=8) :: coefff, coefcr, coeffr, coeffp
    real(kind=8) :: coefcp
    real(kind=8) :: rre, rrm, jeu
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: ddeple(3), ddeplm(3), dlagrc, dlagrf(2)
    aster_logical :: lfrott, lpenaf, lpenac, lesclx, lmaitx, lcontx
    real(kind=8) :: vtmp(n)
    integer :: contac, ibid, npte
    integer :: ndeple, ddle(2), ddlm(2), nfhe, nfhm
    real(kind=8) :: ffec(8)
    aster_logical :: lmulti
!
! ----------------------------------------------------------------------
!
!
! --- INFOS SUR LA MAILLE DE CONTACT
!
    call xmelet(nomte, typmai, elrees, elrema, elreco,&
                ndim, nddl, nne, nnm, nnc,&
                ddle, ddlm, contac, ndeple, nsinge,&
                nsingm, nfhe, nfhm)
!
    ASSERT(nddl.le.n)
    lmulti = .false.
    if (nfhe .gt. 1 .or. nfhm .gt. 1) lmulti = .true.
!
! --- INITIALISATIONS
!
    call vecini(n, 0.d0, vtmp)
    call vecini(9, 0.d0, geopi)
    call vecini(2, 0.d0, dlagrf)
    dlagrc = 0.d0
!
    call vecini(3, 0.d0, ddeple)
    call vecini(3, 0.d0, ddeplm)
!
! --- RECUPERATION DES DONNEES DE LA CARTE CONTACT POINT (VOIR XMCART)
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
    coefcp = zr(jpcpo-1+33)
    coeffp = zr(jpcpo-1+34)
    coefff = zr(jpcpo-1+15)
    ifrott = nint(zr(jpcpo-1+16))
    indnor = nint(zr(jpcpo-1+17))
    hpg = zr(jpcpo-1+19)
! --- LES NUMEROS DES FACETTES DE CONTACT (ESCLAVE ET MAITRE) DONT LE
! --- PTC ET SON PROJETE APPARTIENT
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
    lfrott = ifrott.eq.3
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
    if (nfhe .gt. 0 .or. nfhm .gt. 0) then
      call jevech('PHEA_NO', 'L', jheavn)
      call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7,&
                itab=jtab)
      ncompn = jtab(2)/jtab(3)
    endif
!
    if (lmulti) then
!
! --- RECUPERATION DES FONCTION HEAVISIDES SUR LES FACETTES
!
        call jevech('PHEA_FA', 'L', jheafa)
!
! --- RECUPERATION DE LA PLACE DES LAGRANGES
!
        call jevech('PHEAVNO', 'L', jheano)
    else
        jheafa=1
        jheano=1
    endif
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
        ASSERT(contac.eq.0)
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
! --- CALCUL DES INCREMENTS - LAGRANGE DE CONTACT ET FROTTEMENT
!
    call xtlagm(ndim, nnc, nne, ddle(1),&
                jdepde, ffc,&
                lfrott, nfhe, lmulti, zi(jheano),&
                dlagrc, dlagrf)
!
! --- NOEUDS EXCLUS PAR PROJECTION HORS ZONE
!
    if (indnor .eq. 1) then
        indco = 0
    endif
!
! --- RECUPERATION DES VECTEURS 'OUT' (A REMPLIR DONC MODE ECRITURE)
!
    call jevech('PVECTUR', 'E', ivect)
!
! --- CALCUL DES SECOND MEMBRES DE CONTACT/FROTTEMENT
!
    if (option .eq. 'CHAR_MECA_CONT') then
        if (indco .eq. 1) then
!
! --- VECTEUR SECOND MEMBRE SI CONTACT
!
            call xmmjeu(ndim, nnm, nne, ndeple, nsinge,&
                        nsingm, ffe, ffm, norm, jgeom,&
                        jdepde, jdepm, rre, rrm, ddle,&
                        ddlm, nfhe, nfhm, lmulti, zi(jheavn), zi(jheafa),&
                        jeu)
!
            call xmvec1(ndim, nne, ndeple, nnc, nnm,&
                        hpg, ffc, ffe, ffm,&
                        jacobi, dlagrc, coefcr,&
                        coefcp, lpenac, jeu, norm,&
                        nsinge, nsingm, rre, rrm,&
                        ddle, ddlm, nfhe, nfhm, lmulti,&
                        zi(jheano), zi(jheavn), zi(jheafa), vtmp)
!
        else if (indco .eq. 0) then
            if (nvit .eq. 1) then
!
! --- CALCUL DU VECTEUR - CAS SANS CONTACT
!
                call xmvec0(ndim, nne, nnc, dlagrc,&
                            hpg, ffc, jacobi,&
                            coefcr, coefcp, lpenac, ddle,&
                            nfhe, lmulti, zi(jheano), vtmp)
            endif
        else
            call utmess('F', 'ELEMENTS3_80')
        endif
!
    else if (option.eq.'CHAR_MECA_FROT') then
!              ---------------------
        if (coefff .eq. 0.d0) indco = 0
! ON MET LA SECURITE EN PENALISATION EGALEMENT
        if (dlagrc .eq. 0.d0) indco = 0
        if (ifrott .ne. 3) indco = 0
!
        if (indco .eq. 0) then
            if (nvit .eq. 1) then
!
! --- CALCUL DU VECTEUR - CAS SANS FROTTEMENT
!
!
                call xmvef0(ndim, nne, nnc,&
                            hpg, ffc, jacobi, lpenac,&
                            dlagrf, tau1, tau2,&
                            ddle, nfhe, lmulti, zi(jheano),&
                            vtmp)
            endif
        else if (indco.eq.1) then
!
! --- CALCUL DES INCREMENTS - DEPLACEMENTS
!
            call xtdepm(ndim, nnm, nne, ndeple, nsinge,&
                        nsingm, ffe, ffm, jdepde, rre,&
                        rrm, ddle, ddlm, nfhe, nfhm, lmulti,&
                        zi(jheavn), zi(jheafa),&
                        ddeple, ddeplm)
!
!
! --- ON CALCULE L'ETAT DE CONTACT ADHERENT OU GLISSANT
!
            call ttprsm(ndim, ddeple, ddeplm, dlagrf, coeffr,&
                        tau1, tau2, mprojt, inadh, rese,&
                        nrese, coeffp, lpenaf, dvitet)
!
! --- CALCUL DU JEU
!
            jeu = 0.d0
            if (ndim .eq. 3 .and. contac .eq. 3) then
                call xmmjeu(ndim, nnm, nne, ndeple, nsinge,&
                            nsingm, ffe, ffm, norm, jgeom,&
                            jdepde, jdepm, rre, rrm, ddle,&
                            ddlm, nfhe, nfhm, lmulti, zi(jheavn), zi(jheafa),&
                            jeu)
            endif
!
! --- SI GLISSANT, NORMALISATION RESE
!
            if (inadh .eq. 0) call normev(rese, nrese)
!
! --- VECTEUR FROTTEMENT
!
            call xmvef1(ndim, nne, nnm, ndeple, nnc,&
                        hpg, ffc, ffe,&
                        ffm, jacobi, dlagrc, dlagrf,&
                        coeffr, lpenaf, coefff, tau1,&
                        tau2, rese, mprojt, coefcr,&
                        jeu, nsinge, nsingm, rre,&
                        rrm, nvit, contac, ddle, ddlm,&
                        nfhe, nfhm, lmulti,  zi(jheavn), zi(jheafa),&
                        vtmp)
        endif
    else
        ASSERT(.false.)
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
                lmulti, zi(jheano), vtmp=vtmp)
!
!
! --- RECOPIE VALEURS FINALES
!
    do i = 1, nddl
        zr(ivect-1+i)=vtmp(i)
    end do
!
end subroutine
