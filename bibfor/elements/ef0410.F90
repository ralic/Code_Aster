subroutine ef0410(nomte)
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
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/vdefro.h'
    include 'asterfort/vdrepe.h'
    include 'asterfort/vdxsig.h'
    character(len=16) :: nomte
!
    integer :: npgt, ncoumx, vali(2)
!-----------------------------------------------------------------------
    integer :: jcou, jeffg, jgeom, lzi, nb2
    integer :: nbcou, np, npgsr
!-----------------------------------------------------------------------
    parameter(npgt=10,ncoumx=10)
    integer :: nb1
    real(kind=8) :: effgt(8, 9), sigpg(162*ncoumx)
    real(kind=8) :: matevn(2, 2, npgt), matevg(2, 2, npgt)
! DEB
!
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb2=zi(lzi-1+2)
!
    call jevech('PNBSP_I', 'L', jcou)
    nbcou=zi(jcou)
    vali(1)=ncoumx
    vali(2)=nbcou
    if (nbcou .gt. ncoumx) call u2mesi('F', 'CALCULEL7_4', 2, vali)
!
!     LE TABLEAU SIGPG A ETE ALLOUE DE FACON STATIQUE POUR OPTIMISER
!     LE CPU CAR LES APPELS A WKVECT DANS LES TE SONT COUTEUX.
    call vdxsig(nomte, 'EFGE_ELNO', zr(jgeom), nb1, npgsr,&
                sigpg, effgt)
!
! --- DETERMINATION DES MATRICES DE PASSAGE DES REPERES INTRINSEQUES
! --- AUX NOEUDS ET AUX POINTS D'INTEGRATION DE L'ELEMENT
! --- AU REPERE UTILISATEUR :
!     ---------------------
    call vdrepe(nomte, matevn, matevg)
    call jevech('PEFFORR', 'E', jeffg)
!
!     CES CHAMPS SONT CALCULES AUX NB2 NOEUDS
!
    np=nb2
!
! --- PASSAGE DU VECTEUR DES EFFORTS GENERALISES OU DU VECTEUR
! --- DES DEFORMATIONS-COURBURES DEFINI AUX NOEUDS
! --- DE L'ELEMENT DU REPERE INTRINSEQUE AU REPERE UTILISATEUR :
!     --------------------------------------------------------
    call vdefro(np, matevn, effgt, zr(jeffg))
!
!
end subroutine
