subroutine fgrmax(ncyc, sigmin, sigmax, smin, smax)
!       ================================================================
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
!       ----------------------------------------------------------------
!      COMPTAGE DES CYCLES PAR LA METHODE RAINFLOW AVEC LE CYCLE MAX
!      AU DEBUT DE CHARGEMENT
!       ----------------------------------------------------------------
!      IN  NCYC    NOMBRE  DE  CYCLE
!      IN  SIGMAX  CONTRAINTES MAXIMALES DES CYCLES APRES RAINFLOW
!          SIGMIN  CONTRAINTES MINIMALES DES CYCLES APRES RAINFLOW
!      OUT SMAX  CONTRAINTES MAXIMALES DES CYCLES APRES RAINFLOW_MAX
!          SMIN  CONTRAINTES MINIMALES DES CYCLES APRES RAINFLOW
!       ----------------------------------------------------------------
    implicit none
    include 'asterfort/infniv.h'
    real(kind=8) :: sigmax(*), sigmin(*)
    real(kind=8) :: ampmax, smax(*), smin(*)
    integer :: ncyc, cycmax
!       ----------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ifm, niv
!-----------------------------------------------------------------------
    call infniv(ifm, niv)
!
    cycmax = 1
    ampmax = sigmax(1) - sigmin(1)
!
    do 10 i = 2, ncyc
        if ((sigmax(i) - sigmin(i)) .gt. ampmax) then
            ampmax = sigmax(i) - sigmin(i)
            cycmax = i
        endif
10  continue
!
    smin(1) = sigmin(cycmax)
    smax(1) = sigmax(cycmax)
!
    do 20 i = 2, ncyc
        if (i .lt. cycmax) then
            smin(i) = sigmin(i-1)
            smax(i) = sigmax(i-1)
        else if (i .gt. cycmax) then
            smin(i) = sigmin(i)
            smax(i) = sigmax(i)
        endif
20  continue
    if (cycmax .eq. ncyc) then
        smin(ncyc) = sigmin(ncyc-1)
        smax(ncyc) = sigmax(ncyc-1)
    endif
!
!     --- IMPRESSION DES PICS EXTRAITS DE LA FONCTION ----
    if (niv .eq. 2) then
        write (ifm,*)
        write (ifm,'(1X,A)') 'PICS APRES LE COMPTAGE RAINFLOW_MAX'
        write (ifm,*)
        write (6,*) 'NOMBRE DE CYCLES = ', ncyc
        write (ifm,*)
        write (ifm,'(1X,A)') '     CHARGEMENT_MAX     CHARGEMENT_MIN'
        write (ifm,*)
        write (ifm,'(2(1X,E18.6))') (smax(i),smin(i),i=1,ncyc)
!         DO 106 I = 1,NCYC
!             WRITE (IFM,'(2(1X,E18.6))'), SMAX(I),SMIN(I)
! 106     CONTINUE
!
    endif
!
end subroutine
