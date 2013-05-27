subroutine gimpte(resu, rayinf, raysup, theta, nomnoe,&
                  dir, absc, nbno, format, unit)
    implicit   none
    integer :: nbno, unit
    real(kind=8) :: rayinf(*), raysup(*), theta(*), dir(*), absc(*)
    character(len=8) :: resu, nomnoe(*)
    character(len=*) :: format
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_6
!
!     IMPRESSION DES OBJETS DECRIVANT LE CHAMP THETA
!
! IN : NBRE   : NOMBRE-1 DE CHAMPS THETA (=1)
! IN : RAYINF : RAYON INTERNE SUR LE FOND DE FISSURE
! IN : RAYSUP : RAYON EXTERNE SUR LE FOND DE FISSURE
! IN : THETA  : MODULE DE THETA SUR LE FOND DE FISSURE
! IN : NOMNOE : NOMS DES NOEUDS SUR LE FOND DE FISSURE
! IN : DIR    : NORMALE SUR LE FOND DE FISSURE
! IN : NBNO   : NOMBRE DE NOEUDS SUR LE FOND DE FISSURE
!
! ----------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: rinf, rsup, module, nx, ny, nz
    character(len=1) :: bacs
    character(len=9) :: impnoe
!     ------------------------------------------------------------------
!
    bacs = char(92)
!
    write ( unit, 1000) resu
    write ( unit, 1010)
    do 10 i = 1, nbno
        if (format(1:5) .eq. 'AGRAF') then
            impnoe = bacs//nomnoe(i)
        else
            impnoe = nomnoe(i)
        endif
        rinf = rayinf(i)
        rsup = raysup(i)
        module = theta(i)
        nx = dir((i-1)*3+1)
        ny = dir((i-1)*3+2)
        nz = dir((i-1)*3+3)
        write ( unit, 1020) absc(i),impnoe,rinf,rsup,module,nx,ny,nz
10  end do
!
    1000 format( '==> CHAMP THETA : ',a8,/,'    FOND DE FISSURE' )
    1010 format(5x,'ABSC_CURV',5x,'NOEUD',9x,'R_INF',10x,'R_SUP',10x,&
     &       'THETA',10x,'DIR_X',10x,'DIR_Y',10x,'DIR_Z')
    1020 format(1p,3x,e12.5,3x,a9,6(3x,e12.5))
!
end subroutine
