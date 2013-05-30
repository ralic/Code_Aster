subroutine rccoma(jmat, pheno, iarret, phenom, icodre)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/u2mesk.h'
    integer :: jmat, iarret, icodre
    character(len=*) :: pheno, phenom
! ----------------------------------------------------------------------
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
!
!     OBTENTION DU NOM DU MOT CLE FACTEUR CORRESPONDANT A UN NOM "COURT"
!
!     EXEMPLES :
!      'ELAS' -> / 'ELAS'
!                / 'ELAS_ISTR'
!                / 'ELAS_GONF'
!                / ...
!      'ECRO' -> / 'ECRO_PUIS'
!                / 'ECRO_LINE'
!
!     ARGUMENTS D'ENTREE:
!        JMAT   : ADRESSE DU MATERIAU CODE
!        PHENO  : NOM "COURT" RECHERCHE
!        IARRET : = 0 ON REMPLIT ICODRE ET ON SORT SANS MESSAGE.
!                 = 1 ON S'ARRETE AU PREMIER PROBLEME AVEC UN MESSAGE
!     ARGUMENTS DE SORTIE:
!        PHENOM : NOM DU MOT-CLE FACTEUR (SANS SON EVENTUEL SUFFIXE _FO)
!                 DONT LE NOM COMMENCE PAR LE NOM "COURT" (PHENO)
!        ICODRE : = 0 SI ON A TROUVE 1 PHENOM UNIQUE
!                 = 1 SI ON N'A TROUVE AUCUN PHENOM
!                 = 2 SI ON A TROUVE PLUSIEURS PHENOM
!
! ----------------------------------------------------------------------
!
    integer :: nbmat, im, imat, icomp, ind
    character(len=16) :: feno
!
! ----------------------------------------------------------------------
!
    call assert(jmat.ne.1)
    call assert((iarret.eq.0) .or. (iarret.eq.1))
!
    feno = pheno
!
!     -- PHENO NE DOIT PAS CONTENIR LE SUFFIXE _FO :
    ind = index(feno,'_FO')
    call assert(ind.eq.0)
!
!
    icodre = 1
    phenom = ' '
    nbmat=zi(jmat)
    do 20 im = 1, nbmat
        imat = jmat+zi(jmat+nbmat+im)
        do 10 icomp = 1, zi(imat+1)
            if (pheno .eq. zk16(zi(imat)+icomp-1)(1:len(pheno))) then
                if (phenom .eq. ' ') then
                    phenom=zk16(zi(imat)+icomp-1)
                    icodre = 0
                else
                    if (iarret .eq. 1) then
                        call u2mesk('F', 'MODELISA6_56', 1, feno)
                    else
                        icodre = 2
                    endif
                endif
            endif
10      continue
20  end do
    if (( icodre .eq. 1 ) .and. ( iarret .eq. 1 )) then
        call u2mesk('F', 'MODELISA6_57', 1, feno)
    endif
!
end subroutine
