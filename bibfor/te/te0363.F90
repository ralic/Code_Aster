subroutine te0363(option, nomte)
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
    include 'jeveux.h'
!
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/elelin.h'
    include 'asterfort/elrfvf.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/mmnorm.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/xlacti.h'
    include 'asterfort/xmelet.h'
    include 'asterfort/xmmjec.h'
    include 'asterfort/xmoffc.h'
    include 'asterfort/xtform.h'
    include 'asterfort/xtlagc.h'
    character(len=16) :: option, nomte
!
! ----------------------------------------------------------------------
!  CONTACT XFEM GRANDS GLISSEMENTS
!  REACTUALISATION DU STATUT DE CONTACT
!
!  OPTION : 'XCVBCA' (X-FEM MISE À JOUR DU STATUT DE CONTACT)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!
!
!
    character(len=8) :: typmai, elrees, elrema, elreco, typmec
    integer :: ndim, nddl, nne(3), nnm(3), nnc
    integer :: nsinge, nsingm, lact(8), nlact, ninter
    integer :: jpcpo, jpcai, jheafa, jheano
    integer :: jgeom, jdepde
    integer :: indco, memco, indnor, igliss, nfaes
    integer :: jout
    integer :: incoca, nfhe, nfhm
    real(kind=8) :: jeuca, tau1(3), tau2(3), norm(3)
    real(kind=8) :: coore(3), coorm(3), coorc(2)
    real(kind=8) :: dlagrc, rhon
    real(kind=8) :: ffc(9), ffe(20), ffm(20), dffc(3, 9)
    real(kind=8) :: prec, noor
    real(kind=8) :: rre, rrm, ffec(8)
    parameter    (prec=1.d-16)
    integer :: cface(5, 3), contac, ddle(2), ddlm(2), ibid, ndeple
    logical :: lmulti
!
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
    call assert(nddl.le.336)
    lmulti = .false.
    if (nfhe .gt. 1 .or. nfhm .gt. 1) lmulti = .true.
!
! --- INITIALISATIONS
!
    dlagrc = 0.d0
! --- INITIALISATION DE LA VARIABLE DE TRAVAIL
    incoca = 0
    call assert(option.eq.'XCVBCA')
!
! --- RECUPERATION DES DONNEES DE LA CARTE CONTACT 'POINT' (VOIR XMCART)
!
    call jevech('PCAR_PT', 'L', jpcpo)
! --- LES COORDONNEES ESCLAVE DANS L'ELEMENT DE CONTACT
    coorc(1) = zr(jpcpo-1+1)
    coorc(2) = zr(jpcpo-1+10)
    tau1(1) = zr(jpcpo-1+4)
    tau1(2) = zr(jpcpo-1+5)
    tau1(3) = zr(jpcpo-1+6)
    tau2(1) = zr(jpcpo-1+7)
    tau2(2) = zr(jpcpo-1+8)
    tau2(3) = zr(jpcpo-1+9)
    rhon = zr(jpcpo-1+13)
    indco = nint(zr(jpcpo-1+11))
    ninter = nint(zr(jpcpo-1+31))
    indnor = nint(zr(jpcpo-1+17))
    igliss = nint(zr(jpcpo-1+20))
    memco = nint(zr(jpcpo-1+21))
    nfaes = nint(zr(jpcpo-1+22))
! --- LES COORDONNEES ESCLAVE ET MAITRES DANS L'ELEMENT PARENT
    coore(1) = zr(jpcpo-1+24)
    coore(2) = zr(jpcpo-1+25)
    coore(3) = zr(jpcpo-1+26)
    coorm(1) = zr(jpcpo-1+27)
    coorm(2) = zr(jpcpo-1+28)
    coorm(3) = zr(jpcpo-1+29)
! --- SQRT LSN PT MAITRE, ESCLAVE
    rre = zr(jpcpo-1+18)
    rrm = zr(jpcpo-1+23)
    if (nnm(1) .eq. 0) rre = 2*rre
!
! --- RECUPERATION DES DONNEES DE LA CARTE CONTACT AINTER (VOIR XMCART)
!
    call jevech('PCAR_AI', 'L', jpcai)
!
! --- RECUPERATION DE LA GEOMETRIE ET DES CHAMPS DE DEPLACEMENT
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PDEPL_P', 'L', jdepde)
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
! --- RECUPERATION DES CHAMPS DE SORTIE
!
    call jevech('PINDCOO', 'E', jout)
!
! --- FONCTIONS DE FORMES
!
    call xtform(ndim, elrees, elrema, elreco, ndeple,&
                nnm(1), nnc, coore, coorm, coorc,&
                ffe, ffm, dffc)
!
! --- ON CONSTRUIT LA MATRICE DE CONNECTIVITÉ CFACE (MAILLE ESCLAVE), CE
! --- QUI SUIT N'EST VALABLE QU'EN 2D POUR LA FORMULATION QUADRATIQUE,
! --- EN 3D ON UTILISE SEULEMENT LA FORMULATION AUX NOEUDS SOMMETS,
! --- CETTE MATRICE EST DONC INUTILE, ON NE LA CONSTRUIT PAS !!!
!
    cface(1,1) = 1
    cface(1,2) = 2
    cface(1,3) = 3
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
! --- CALCUL DE LA NORMALE
!
    if (ndim .eq. 2) then
        call mmnorm(ndim, tau1, tau2, norm, noor)
    else if (ndim.eq.3) then
        call provec(tau1, tau2, norm)
        call normev(norm, noor)
    endif
    if (noor .le. r8prem()) then
        call assert(.false.)
    endif
!
! --- CALCUL DE L'INCREMENT DE REACTION DE CONTACT
!
    call xtlagc(typmai, ndim, nnc, nne, ddle(1),&
                nfaes, cface, jdepde, jpcai, ffc,&
                contac, nfhe, lmulti, zi(jheano), dlagrc)
!
! --- EVALUATION DES JEUX - CAS DU CONTACT
!
    call xmmjec(ndim, nnm, nne, ndeple, nsinge,&
                nsingm, ffe, ffm, norm, jgeom,&
                jdepde, rre, rrm, ddle, ddlm,&
                nfhe, nfhm, lmulti, zi(jheafa), jeuca)
!
!
! --- NOEUDS EXCLUS PAR PROJECTION HORS ZONE
!
    if (indnor .eq. 1) then
        if ((igliss.eq.0) .or. (memco.eq.0)) then
            zi(jout-1+2) = 0
            goto 999
        endif
    endif
!
! --- SI LE CONTACT A ETE POSTULE, ON TESTE LA VALEUR DE LA PRESSION
! --- DE CONTACT
!
    if (indco .eq. 1) then
        if ((dlagrc-rhon*jeuca) .gt. r8prem()) then
            if ((igliss.eq.1) .and. (memco.eq.1)) then
                zi(jout-1+2) = 1
                zi(jout-1+3) = 1
            else if (igliss.eq.0) then
                zi(jout-1+2) = 0
                incoca = 1
            endif
        else
            zi(jout-1+2) = 1
            zi(jout-1+3) = 1
        endif
!
! --- SI LE NON-CONTACT A ETE POSTULÉ, ON TESTE LA VALEUR DU JEU
!
    else if (indco .eq. 0) then
        if (jeuca .gt. prec) then
            zi(jout-1+2) = 1
            zi(jout-1+3) = 1
            incoca = 1
        else
            zi(jout-1+2) = 0
        endif
!
    else
        call assert(.false.)
    endif
!
999  continue
!
! --- ENREGISTREMENT DU CHAMP DE SORTIE
!
    zi(jout-1+1)=incoca
!
    call jedema()
end subroutine
