subroutine te0270(option, nomte)
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_TEXT_F'
!                          ELEMENTS FOURIER
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    include 'asterfort/vff2dn.h'
    character(len=16) :: option, nomte
    integer :: nbres
    parameter (nbres=3)
    character(len=8) :: nompar(nbres)
    real(kind=8) :: valpar(nbres), poids, r, z, nx, ny, tpg, theta, coen, coenp1
    real(kind=8) :: texn, texnp1
    integer :: nno, nnos, jgano, ndim, kp, npg, ipoids, ivf, idfde, igeom
    integer :: itemps, ivectt, k, i, itex, icoefh, itemp, icode
!
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PCOEFHF', 'L', icoefh)
    call jevech('PT_EXTF', 'L', itex)
    call jevech('PVECTTR', 'E', ivectt)
!
!====
! 2. CALCULS TERMES DE MASSE
!====
    theta = zr(itemps+2)
    do 50 kp = 1, npg
        k = (kp-1)*nno
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        r = 0.d0
        z = 0.d0
        tpg = 0.d0
        do 10 i = 1, nno
            r = r + zr(igeom+2*i-2)*zr(ivf+k+i-1)
            z = z + zr(igeom+2*i-1)*zr(ivf+k+i-1)
            tpg = tpg + zr(itemp+i-1)*zr(ivf+k+i-1)
10      continue
        poids = poids*r
        valpar(1) = r
        nompar(1) = 'X'
        valpar(2) = z
        nompar(2) = 'Y'
        nompar(3) = 'INST'
        valpar(3) = zr(itemps)
        call fointe('FM', zk8(icoefh), 3, nompar, valpar,&
                    coenp1, icode)
        valpar(3) = zr(itemps) - zr(itemps+1)
        call fointe('FM', zk8(icoefh), 3, nompar, valpar,&
                    coen, icode)
        valpar(3) = zr(itemps)
        call fointe('FM', zk8(itex), 3, nompar, valpar,&
                    texnp1, icode)
        valpar(3) = zr(itemps) - zr(itemps+1)
        call fointe('FM', zk8(itex), 3, nompar, valpar,&
                    texn, icode)
!
        do 30 i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + poids*zr(ivf+k+i-1)* ( theta*coenp1*texnp1+ (1.0d0-&
                             &theta)*coen* (texn-tpg))
30      continue
50  end do
end subroutine
