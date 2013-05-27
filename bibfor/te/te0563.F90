subroutine te0563(option, nomte)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tecach.h'
    character(len=16) :: nomte, option
!.----------------------------------------------------------------------
!     OPTION : NORME
!
!     BUT: CALCUL LA NORME L2 AU CARRE
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.----------------------------------------------------------------------
!
!
    integer :: ipoids, icham, inorm, ncmp1, ncmp2, npg, i, j, iret, ncmp3
    integer :: ndim, nno, nnos, ivf, idfdx, jgano, jtab1(2), jtab2(2), icoef
    integer :: jtab3(2), ibid
    real(kind=8) :: resu, vale, poids
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ibid, ivf, idfdx, jgano)
!
    call jevech('PCOORPG', 'L', ipoids)
    call jevech('PCHAMPG', 'L', icham)
    call jevech('PCOEFR', 'L', icoef)
    call jevech('PNORME', 'E', inorm)
!
    call tecach('OON', 'PCHAMPG', 'L', 2, jtab1,&
                iret)
!     NOMBRE DE COMPOSANTES DU CHAMP DE VALEURS (=30) : NCMP1
    ncmp1=jtab1(2)/npg
!
    call tecach('OON', 'PCOORPG', 'L', 2, jtab2,&
                iret)
!     NOMBRE DE COMPOSANTES DU CHAMP COOR_ELGA (3 OU 4) : NCMP2
    ncmp2=jtab2(2)/npg
!
    call tecach('OON', 'PCOEFR', 'L', 2, jtab3,&
                iret)
!     NOMBRE DE COMPOSANTES DU CHAMP DE COEF (=30) : NCMP3
    ncmp3=jtab3(2)
    call assert(ncmp3.eq.ncmp1)
!
    resu=0.d0
    do 10 i = 1, npg
        poids=zr(ipoids+ncmp2*(i-1)+ncmp2-1)
        vale=0.d0
        do 20 j = 1, ncmp1
            vale = vale + zr(icoef+j-1) * zr(icham+ncmp1*(i-1)+j-1)* zr(icham+ncmp1*(i-1)+j-1)
20      continue
        resu=resu+vale*poids
10  end do
!
    zr(inorm)=resu
!
end subroutine
