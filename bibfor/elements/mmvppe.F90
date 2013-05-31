subroutine mmvppe(typmae, typmam, iresog, ndim, nne,&
                  nnm, nnl, nbdm, laxis, ldyna,&
                  lfovit, jeusup, ffe, ffm, ffl,&
                  norm, tau1, tau2, mprojt, jacobi,&
                  wpg, dlagrc, dlagrf, jeu, djeu,&
                  djeut)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit     none
    include      'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/mmdepm.h'
    include 'asterfort/mmform.h'
    include 'asterfort/mmgeom.h'
    include 'asterfort/mmlagm.h'
    include 'asterfort/mmmjac.h'
    include 'asterfort/mmmjeu.h'
    include 'asterfort/mmmjev.h'
    include 'asterfort/mmreac.h'
    include 'asterfort/mmvitm.h'
    character(len=8) :: typmae, typmam
    integer :: iresog
    integer :: ndim, nne, nnm, nnl, nbdm
    real(kind=8) :: ffe(9), ffm(9), ffl(9)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3)
    real(kind=8) :: mprojt(3, 3)
    logical :: laxis, ldyna, lfovit
    real(kind=8) :: jacobi, wpg
    real(kind=8) :: jeusup
    real(kind=8) :: dlagrc, dlagrf(2)
    real(kind=8) :: jeu, djeu(3), djeut(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! PREPARATION DES CALCULS DES VECTEURS - CALCUL DES QUANTITES
! CAS POIN_ELEM
!
! ----------------------------------------------------------------------
!
!
! IN  IRESOG : ALGO. DE RESOLUTION POUR LA GEOMETRIE
!              0 - POINT FIXE
!              1 - NEWTON
! IN  TYPMAE : TYPE DE LA MAILLE ESCLAVE
! IN  TYPMAM : TYPE DE LA MAILLE MAITRE
! IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNL    : NOMBRE DE NOEUDS PORTANT UN LAGRANGE DE CONTACT/FROTT
! IN  NBDM   : NOMBRE DE COMPOSANTES/NOEUD DES DEPL+LAGR_C+LAGR_F
! IN  LAXIS  : .TRUE. SI AXISYMETRIE
! IN  LDYNA  : .TRUE. SI DYNAMIQUE
! IN  LFOVIT : .TRUE. SI FORMULATION EN VITESSE
! IN  JEUSUP : JEU SUPPLEMENTAIRE PAR DIST_ESCL/DIST_MAIT
! OUT FFE    : FONCTIONS DE FORMES DEPL_ESCL
! OUT FFM    : FONCTIONS DE FORMES DEPL_MAIT
! OUT FFL    : FONCTIONS DE FORMES LAGR.
! OUT NORM   : NORMALE
! OUT TAU1   : PREMIER VECTEUR TANGENT
! OUT TAU2   : SECOND VECTEUR TANGENT
! OUT MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! OUT JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! OUT WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! OUT DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! OUT DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! OUT JEU    : JEU NORMAL ACTUALISE
! OUT DJEU   : INCREMENT DEPDEL DU JEU
! OUT DJEUT  : INCREMENT DEPDEL DU JEU TANGENT
!
! ----------------------------------------------------------------------
!
    integer :: jpcf
    integer :: jgeom, jdepde, jdepm
    integer :: jaccm, jvitm, jvitp
    real(kind=8) :: geomae(9, 3), geomam(9, 3)
    real(kind=8) :: geomm(3), geome(3)
    real(kind=8) :: ddeple(3), ddeplm(3)
    real(kind=8) :: deplme(3), deplmm(3)
    real(kind=8) :: accme(3), vitme(3), accmm(3), vitmm(3)
    real(kind=8) :: vitpe(3), vitpm(3)
    real(kind=8) :: dffe(2, 9), ddffe(3, 9)
    real(kind=8) :: dffm(2, 9), ddffm(3, 9)
    real(kind=8) :: dffl(2, 9), ddffl(3, 9)
    real(kind=8) :: xpc, ypc, xpr, ypr
    real(kind=8) :: mprojn(3, 3)
    real(kind=8) :: jeuvit
    real(kind=8) :: mprt1n(3, 3), mprt2n(3, 3)
    real(kind=8) :: gene11(3, 3), gene21(3, 3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DES DONNEES DE PROJECTION
!
    call jevech('PCONFR', 'L', jpcf)
    xpc = zr(jpcf-1+1)
    ypc = zr(jpcf-1+2)
    xpr = zr(jpcf-1+3)
    ypr = zr(jpcf-1+4)
    tau1(1) = zr(jpcf-1+5)
    tau1(2) = zr(jpcf-1+6)
    tau1(3) = zr(jpcf-1+7)
    tau2(1) = zr(jpcf-1+8)
    tau2(2) = zr(jpcf-1+9)
    tau2(3) = zr(jpcf-1+10)
    wpg = zr(jpcf-1+11)
!
! --- RECUPERATION DE LA GEOMETRIE ET DES CHAMPS DE DEPLACEMENT
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PDEPL_P', 'L', jdepde)
    call jevech('PDEPL_M', 'L', jdepm)
    if (ldyna) then
        call jevech('PVITE_P', 'L', jvitp)
        call jevech('PVITE_M', 'L', jvitm)
        call jevech('PACCE_M', 'L', jaccm)
    endif
!
! --- FONCTIONS DE FORMES ET DERIVEES
!
    call mmform(ndim, typmae, typmam, nne, nnm,&
                xpc, ypc, xpr, ypr, ffe,&
                dffe, ddffe, ffm, dffm, ddffm,&
                ffl, dffl, ddffl)
!
! --- JACOBIEN POUR LE POINT DE CONTACT
!
    call mmmjac(typmae, jgeom, ffe, dffe, laxis,&
                nne, ndim, jacobi)
!
! --- REACTUALISATION DE LA GEOMETRIE (MAILLAGE+DEPMOI)
!
    call mmreac(nbdm, ndim, nne, nnm, jgeom,&
                jdepm, geomae, geomam)
!
! --- CALCUL DES COORDONNEES ACTUALISEES
!
    call mmgeom(iresog, ndim, nne, nnm, ffe,&
                ffm, ddffm, geomae, geomam, tau1,&
                tau2, norm, mprojn, mprojt, geome,&
                geomm, mprt1n, mprt2n, gene11, gene21)
!
! --- CALCUL DES INCREMENTS - LAGRANGE DE CONTACT ET FROTTEMENT
!
    call mmlagm(nbdm, ndim, nnl, jdepde, ffl,&
                dlagrc, dlagrf)
!
!
! --- MISE A JOUR DES CHAMPS INCONNUS INCREMENTAUX - DEPLACEMENTS
!
    call mmdepm(nbdm, ndim, nne, nnm, jdepm,&
                jdepde, ffe, ffm, ddeple, ddeplm,&
                deplme, deplmm)
!
! --- CALCUL DES VITESSES/ACCELERATIONS
!
    if (ldyna) then
        call mmvitm(nbdm, ndim, nne, nnm, ffe,&
                    ffm, jvitm, jaccm, jvitp, vitme,&
                    vitmm, vitpe, vitpm, accme, accmm)
    endif
!
! --- CALCUL DU JEU NORMAL
!
    call mmmjeu(ndim, jeusup, norm, geome, geomm,&
                ddeple, ddeplm, mprojt, jeu, djeu,&
                djeut, iresog, tau1, tau2, gene11,&
                gene21)
!
! --- CALCUL DU JEU EN VITESSE NORMALE
!
    if (lfovit) then
        call mmmjev(ndim, norm, vitpe, vitpm, jeuvit)
    endif
!
    call jedema()
!
end subroutine
