subroutine mmstaf(noma, ndim, chdepd, coefaf, lpenaf,&
                  nummae, aliase, nne, nummam, ksipc1,&
                  ksipc2, ksipr1, ksipr2, mlagf1, mlagf2,&
                  tau1, tau2, norm, indco, indfr,&
                  rese)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_21
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/matini.h'
    include 'asterfort/mcopco.h'
    include 'asterfort/mmvalp.h'
    character(len=8) :: noma, aliase
    integer :: ndim, nne
    real(kind=8) :: ksipc1, ksipc2
    real(kind=8) :: ksipr1, ksipr2
    integer :: nummae, nummam
    character(len=19) :: chdepd
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    real(kind=8) :: coefaf
    logical :: lpenaf
    integer :: indfr, indco
    real(kind=8) :: mlagf1(9), mlagf2(9)
    real(kind=8) :: rese(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CONTRAINTES ACTIVES)
!
! STATUT DU FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  CHDEPD : DEPLACEMENT CUMULE
! IN  COEFAF : COEF_AUGM_FROT
! IN  LPENAF : .TRUE. SI FROTTEMENT PENALISE
! IN  NUMMAE : NUMERO ABSOLU MAILLE ESCLAVE
! IN  ALIASE : NOM D'ALIAS DE L'ELEMENT ESCLAVE
! IN  NNE    : NOMBRE DE NOEUD DE L'ELEMENT ESCLAVE
! IN  NUMMAM : NUMERO ABSOLU MAILLE MAITRE
! IN  KSIPC1 : COORDONNEE KSI1 DU POINT DE CONTACT SUR LA MAILLE ESCLAVE
! IN  KSIPC2 : COORDONNEE KSI2 DU POINT DE CONTACT SUR LA MAILLE ESCLAVE
! IN  KSIPR1 : COORDONNEE KSI1 DU PROJETE DU POINT DE CONTACT SUR LA
!              MAILLE MAITRE
! IN  KSIPR2 : COORDONNEE KSI2 DU PROJETE DU POINT DE CONTACT SUR LA
!              MAILLE MAITRE
! IN  MLAGF1 : MULTIPLICATEUR DE FROTTEMENT 1 SUR LES NOEUDS
! IN  MLAGF2 : MULTIPLICATEUR DE FROTTEMENT 2 SUR LES NOEUDS
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  NORM   : NORMALE
! IN  INDCO  : STATUT DE CONTACT
! OUT INDFR  : INDICATEUR DE FROTTEMENT
!              - INDFR  = 0 SI ADHERENT
!              - INDFR  = 1 SI GLISSANT
! OUT RESE   : MULITPLICATEUR AUGMENTE DU FROTTEMENT (NORMALISE)
!
!
!
!
    integer :: idim, idim1, idim2
    real(kind=8) :: nrese
    real(kind=8) :: dlagrf(2), djeu(3), djeut(3)
    real(kind=8) :: ddeple(3), ddeplm(3)
    real(kind=8) :: mprojt(3, 3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nrese = 0.d0
    indfr = 0
    do 10 idim = 1, 3
        rese(idim) = 0.d0
        djeut(idim) = 0.d0
        djeu(idim) = 0.d0
10  end do
    dlagrf(1) = 0.d0
    dlagrf(2) = 0.d0
    call matini(3, 3, 0.d0, mprojt)
    if (indco .eq. 0) goto 99
!
! --- MATRICE DE PROJECTION TANGENTE
!
    do 121 idim1 = 1, ndim
        do 111 idim2 = 1, ndim
            mprojt(idim1,idim2) = -1.d0*norm(idim1)*norm(idim2)
111      continue
121  end do
    do 330 idim1 = 1, ndim
        mprojt(idim1,idim1) = 1.d0 + mprojt(idim1,idim1)
330  end do
!
! --- MULTIPLICATEUR DE LAGRANGE DE FROTTEMENT DU POINT
!
    call mmvalp(ndim, aliase, nne, 1, ksipc1,&
                ksipc2, mlagf1, dlagrf(1))
    if (ndim .eq. 3) then
        call mmvalp(ndim, aliase, nne, 1, ksipc1,&
                    ksipc2, mlagf2, dlagrf( 2))
    endif
!
! --- INCREMENTS DE DEPLACEMENT
!
    call mcopco(noma, chdepd, ndim, nummae, ksipc1,&
                ksipc2, ddeple)
    call mcopco(noma, chdepd, ndim, nummam, ksipr1,&
                ksipr2, ddeplm)
!
! --- CALCUL DE L'INCREMENT DE JEU
!
    do 5 idim = 1, 3
        djeu(idim) = ddeple(idim) - ddeplm(idim)
 5  end do
!
! --- PROJECTION DE L'INCREMENT DE JEU SUR LE PLAN TANGENT
!
    do 20 idim1 = 1, ndim
        do 25 idim2 = 1, ndim
            djeut(idim1) = mprojt(idim1,idim2)*djeu(idim2)+djeut( idim1)
25      continue
20  end do
!
! --- SEMI-MULTIPLICATEUR DE FROTTEMENT
!
    if (lpenaf) then
        do 32 idim = 1, 3
            rese(idim) = coefaf*djeut(idim)
32      continue
    else
        if (ndim .eq. 2) then
            do 30 idim = 1, 2
                rese(idim) = dlagrf(1)*tau1(idim)+coefaf*djeut(idim)
30          continue
        else if (ndim.eq.3) then
            do 31 idim = 1, 3
                rese(idim) = dlagrf(1)*tau1(idim)+ dlagrf(2)*tau2( idim)+ coefaf*djeut(idim)
31          continue
        else
            call assert(.false.)
        endif
    endif
!
! --- CALCUL DU COEF D'ADHERENCE
!
    do 40 idim = 1, 3
        nrese = rese(idim)*rese(idim) + nrese
40  end do
    nrese = sqrt(nrese)
!
! --- NORMALISATION
!
    if (nrese .ne. 0.d0) then
        do 50 idim = 1, 3
            rese(idim) = rese(idim)/nrese
50      continue
    endif
!
! --- ADHERENCE OU GLISSEMENT ?
!
    if (nrese .le. 1.d0) then
        indfr = 0
    else
        indfr = 1
    endif
!
99  continue
!
    call jedema()
end subroutine
