subroutine copcvn(nb, vec1, vec2, indir, fact)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 19/02/91
!-----------------------------------------------------------------------
!  BUT:  COPIER UN VECTEUR DE REELS DANS UN AUTRE EN LE MULTIPLIANT
!   PAR UN FACTEUR AVEC UNE INDIRECTION ENTRE LES ADRESSES DES DEUX
!        VECTEURS
!
!-----------------------------------------------------------------------
!
! NB       /I/: NOMBRE REELS A COPIER
! IVEC1    /I/: VECTEUR A COPIER
! IVEC2    /O/: VECTEUR RESULTAT
! INDIR    /I/: VECTEUR DES INDIRECTIONS DE RANG
! FACT     /I/: FACTEUR
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: vec1(*), vec2(nb)
    integer :: indir(nb)
    integer :: i, nb
    real(kind=8) :: fact
!-----------------------------------------------------------------------
!
    if (nb .eq. 0) goto 9999
!
    do 10 i = 1, nb
        if (indir(i) .gt. 0) then
            vec2(i)=vec1(indir(i))*fact
        else
            vec2(i)=0.d0
        endif
10  end do
!
9999  continue
end subroutine
