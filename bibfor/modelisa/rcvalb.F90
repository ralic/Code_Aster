subroutine rcvalb(fami, kpg, ksp, poum, jmat,&
                  nomat, phenom, nbpar, nompar, valpar,&
                  nbres, nomres, valres, codret, iarret)
    implicit none
    include 'jeveux.h'
    include 'asterfort/rcvala.h'
    include 'asterfort/rcvarc.h'
    integer :: jmat, nbpar, nbres, kpg, ksp, iarret
    real(kind=8) :: valpar(nbpar), valres(nbres)
    integer :: codret(nbres)
    character(len=*) :: nomat, phenom, nompar(nbpar), nomres(nbres)
    character(len=*) :: poum, fami
! person_in_charge: jacques.pellet at edf.fr
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
! ----------------------------------------------------------------------
!  BUT : CHAPEAU A LA ROUTINE RCVALA POUR AJOUTER A LA LISTE DES
!        PARAMETRES DES FONCTIONS LES VARIABLES DE COMMANDE
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: nbcvrc, jvcnom
    common /caii14/nbcvrc,jvcnom
!
    integer :: nbpamx, nbpar2, ipar, nbpart, ier
    parameter (nbpamx=10)
    real(kind=8) :: valpa2(nbpamx), valvrc
    character*(8) :: nompa2(nbpamx), novrc
! DEB ------------------------------------------------------------------
!
!     -- S'IL N'Y A PAS DE VARC, IL N'Y A QU'A APPELER  RCVALA :
    if (nbcvrc .eq. 0) then
        call rcvala(jmat, nomat, phenom, nbpar, nompar,&
                    valpar, nbres, nomres, valres, codret,&
                    iarret)
        goto 9999
    endif
!
!
!     -- SINON, ON AJOUTE LES VARC AU DEBUT DE LA LISTE DES PARAMETRES
!     CAR FOINTA DONNE PRIORITE AUX DERNIERS :
    nbpar2 = 0
    do 1, ipar=1,nbcvrc
    novrc=zk8(jvcnom-1+ipar)
    call rcvarc(' ', novrc, poum, fami, kpg,&
                ksp, valvrc, ier)
    if (ier .eq. 0) then
        nbpar2=nbpar2+1
        nompa2(nbpar2)=novrc
        valpa2(nbpar2)=valvrc
    endif
    1 end do
!
    do 2, ipar=1,nbpar
    nompa2(nbpar2+ipar) = nompar(ipar)
    valpa2(nbpar2+ipar) = valpar(ipar)
    2 end do
!
    nbpart=nbpar+nbpar2
!     CALL ASSERT(NBPART.LE.NBPAMX)
    call rcvala(jmat, nomat, phenom, nbpart, nompa2,&
                valpa2, nbres, nomres, valres, codret,&
                iarret)
!
9999  continue
end subroutine
