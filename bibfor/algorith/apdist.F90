subroutine apdist(elrefe, coorma, nbno, ksi1, ksi2,&
                  coorpt, dist, vecpm)
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
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/elrfvf.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: nbno
    character(len=8) :: elrefe
    real(kind=8) :: coorma(27), coorpt(3)
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: dist, vecpm(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! DISTANCE POINT - PROJECTION SUR MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  ELREFE : TYPE DE LA MAILLE
! IN  COORMA : COORDONNEES DE LA MAILLE
! IN  NBNO   : NOMBRE DE NOEUDS DE LA MAILLE
! IN  KSI1   : COORD. PARAM. 1 DE LA PROJECTION SUR MAILLE
! IN  KSI2   : COORD. PARAM. 2 DE LA PROJECTION SUR MAILLE
! IN  COORPT : COORD. DU POINT A PROJETER
! OUT VECPM  : VECTEUR POINT DE CONTACT -> SON PROJETE SUR MAILLE
! OUT DIST   : DISTANCE POINT - PROJECTION (NORME DE VECPM)
!
!
!
!
    integer :: ifm, niv
    real(kind=8) :: coorpr(3)
    integer :: idim, ino, ibid
    real(kind=8) :: zero
    parameter    (zero=0.d0)
    real(kind=8) :: ksi(2), ff(9)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- INITIALISATIONS
!
    vecpm(1) = zero
    vecpm(2) = zero
    vecpm(3) = zero
    coorpr(1) = zero
    coorpr(2) = zero
    coorpr(3) = zero
    ksi(1) = ksi1
    ksi(2) = ksi2
    dist = 0
!
! --- RECUP FONCTIONS DE FORME
!
    call elrfvf(elrefe, ksi, nbno, ff, ibid)
!
! --- COORDONNEES DE LA PROJECTION
!
    do 40 idim = 1, 3
        do 30 ino = 1, nbno
            coorpr(idim) = ff(ino)*coorma(3*(ino-1)+idim) + coorpr( idim)
30      continue
40  end do
!
! --- VECTEUR POINT/PROJECTION
!
    do 140 idim = 1, 3
        vecpm(idim) = coorpr(idim) - coorpt(idim)
140  end do
!
! --- CALCUL DE LA DISTANCE
!
    dist = sqrt(vecpm(1)**2+vecpm(2)**2+vecpm(3)**2)
!
    call jedema()
!
end subroutine
