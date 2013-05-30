subroutine cdnfon(zimat, kfonc, xx, dn, fxx,&
                  ier)
!
    implicit none
! ======================================================================
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
!======================================================================
!
!     OBTENTION DE LA VALEUR FXX D'UN "ELEMENT" D'UNE RELATION DE
!     COMPORTEMENT D'UN MATERIAU DONNE
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  KFONC : NOM DES RESULTATS (EX: FMEX1,... )
! IN  XX : VALEURS DES PARAMETRES
! IN  DN : CODE
!
! OUT FXX : VALEURS DES RESULTATS APRES RECUPERATION ET INTERPOLATION
! OUT IER : CODE RETOUR
!
    include 'asterfort/rcvalb.h'
    integer :: dn, ier, zimat, kpg, spt
!
    real(kind=8) :: xx, fxx
!
    integer :: codres
    character(len=8) :: kfonc, kaux, fami, poum
    character(len=16) :: phenom
!
    phenom = 'GLRC_DAMAGE'
    ier = 0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    if (dn .eq. 0) then
        call rcvalb(fami, kpg, spt, poum, zimat,&
                    ' ', phenom, 1, 'X ', xx,&
                    1, kfonc, fxx, codres, 0)
    else if (dn .eq. 1) then
        write (kaux,'(A1,A7)') 'D',kfonc(1:7)
        call rcvalb(fami, kpg, spt, poum, zimat,&
                    ' ', phenom, 1, 'X ', xx,&
                    1, kaux, fxx, codres, 0)
    else if (dn .eq. 2) then
        write (kaux,'(A2,A6)') 'DD',kfonc(1:6)
        call rcvalb(fami, kpg, spt, poum, zimat,&
                    ' ', phenom, 1, 'X ', xx,&
                    1, kaux, fxx, codres, 0)
    else
        ier = 3
    endif
!
    if (codres .ne. 0) then
        fxx = 0.d0
        ier = 2
    endif
!
end subroutine
