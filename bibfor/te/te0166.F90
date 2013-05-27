subroutine te0166(option, nomte)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jevech.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mess.h'
    include 'blas/ddot.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL FORCE DE PESANTEUR POUR MEPOULI
!                          OPTION : 'CHAR_MECA_PESA_R'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: icodre
    real(kind=8) :: rho, a, w(9), l1(3), l2(3), l10(3), l20(3)
    real(kind=8) :: norml1, norml2, norl10, norl20, l0, norm1p, norm2p
    real(kind=8) :: poids(3)
    character(len=8) :: fami, poum
    integer :: i, neu, neum1, kc, ic, ivectu, ipesa, kpg, spt
    integer :: igeom, imate, lsect, idepla, ideplp, iret
!
!
!-----------------------------------------------------------------------
    real(kind=8) :: r8b
!-----------------------------------------------------------------------
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 0, ' ', r8b,&
                1, 'RHO', rho, icodre, 1)
    call jevech('PCACABL', 'L', lsect)
    a = zr(lsect)
!
    call tecach('ONO', 'PDEPLMR', 'L', 1, idepla,&
                iret)
    if (iret .ne. 0) call u2mess('F', 'CALCULEL6_78')
    call jevech('PDEPLPR', 'L', ideplp)
!
    do 10 i = 1, 9
        w(i)=zr(idepla-1+i)+zr(ideplp-1+i)
10  end do
!
    do 21 kc = 1, 3
        l1(kc) = w(kc ) + zr(igeom-1+kc) - w(6+kc) - zr(igeom+5+kc)
        l10(kc) = zr(igeom-1+kc) - zr(igeom+5+kc)
21  end do
    do 22 kc = 1, 3
        l2(kc) = w(3+kc) + zr(igeom+2+kc) - w(6+kc) - zr(igeom+5+kc)
        l20(kc) = zr(igeom+2+kc) - zr(igeom+5+kc)
22  end do
    norml1=ddot(3,l1,1,l1,1)
    norml2=ddot(3,l2,1,l2,1)
    norl10=ddot(3,l10,1,l10,1)
    norl20=ddot(3,l20,1,l20,1)
    norml1 = sqrt (norml1)
    norml2 = sqrt (norml2)
    norl10 = sqrt (norl10)
    norl20 = sqrt (norl20)
    l0 = norl10 + norl20
!
    call jevech('PPESANR', 'L', ipesa)
    call jevech('PVECTUR', 'E', ivectu)
!
    norm1p = norml1 * l0 / (norml1+norml2)
    norm2p = norml2 * l0 / (norml1+norml2)
    poids(1) = rho * a * norm1p * zr(ipesa) / 2.d0
    poids(2) = rho * a * norm2p * zr(ipesa) / 2.d0
    poids(3) = poids(1) + poids(2)
!
!
    do 32 neu = 1, 3
        neum1 = neu - 1
        do 31 ic = 1, 3
            zr(ivectu + 3*neum1 + ic-1) = poids(neu) * zr(ipesa+ic)
31      end do
32  end do
!
end subroutine
