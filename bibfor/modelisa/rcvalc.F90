subroutine rcvalc(jmat, phenom, nbres, nomres, valres,&
                  icodre, iarret)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/rcvals.h"
#include "asterfort/utmess.h"
!
    integer :: imat, nbres, jmat, nbmat
    character(len=*) :: phenom, nomres(nbres)
    integer :: iarret
    integer :: icodre(nbres)
    complex(kind=8) :: valres(nbres)
! ----------------------------------------------------------------------
!     OBTENTION DE LA VALEUR VALRES C D'UN "ELEMENT" D'UNE RELATION DE
!     COMPORTEMENT D'UN MATERIAU DONNE (NOUVELLE FORMULE RAPIDE)
!
!     ARGUMENTS D'ENTREE:
!        IMAT   : ADRESSE DU MATERIAU CODE
!        PHENOM : NOM DU PHENOMENE
!        NBRES  : NOMBRE DE RESULTATS
!        NOMRES : NOM DES RESULTATS (EX: E,NU,... )
!                 TELS QU'IL FIGURENT DANS LA COMMANDE MATERIAU
!     ARGUMENTS DE SORTIE:
!     VALRES : VALEURS DES RESULTATS APRES RECUPERATION ET INTERPOLATION
!     ICODRE : POUR CHAQUE RESULTAT, 0 SI ON A TROUVE, 1 SINON
!
!
!
!
!
!
    character(len=32) :: nomphe
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
! DEB ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: icomp, idf, ik, ipi, ir, ires, ivalc
    integer :: ivalk, nbc, nbf, nbobj, nbr, nbt
!-----------------------------------------------------------------------
    nbmat=zi(jmat)
    ASSERT(nbmat.eq.1)
    imat = jmat+zi(jmat+nbmat+1)
!
!
    do 130 ires = 1, nbres
        icodre(ires) = 1
130  end do
    nomphe = phenom
    do 10 icomp = 1, zi(imat+1)
        if (nomphe .eq. zk32(zi(imat)+icomp-1)) then
            ipi = zi(imat+2+icomp-1)
            goto 11
        endif
10  end do
    call utmess('A', 'ELEMENTS2_63')
    goto 9999
11  continue
!
    nbobj = 0
    nbr = zi(ipi )
    nbc = zi(ipi+1)
    ivalk = zi(ipi+3)
    ivalc = zi(ipi+5)
    nbt = nbr + nbc
    do 150 ir = 1, nbt
        do 140 ires = 1, nbres
            if (nomres(ires) .eq. zk16(ivalk+ir-1)) then
                valres(ires) = zc(ivalc-1+ir)
                icodre(ires) = 0
                nbobj = nbobj + 1
            endif
140      continue
150  end do
    if (nbobj .ne. nbres) then
        idf = zi(ipi)+zi(ipi+1)
        nbf = zi(ipi+2)
        do 170 ires = 1, nbres
            do 160 ik = 1, nbf
                if (nomres(ires) .eq. zk16(ivalk+idf+ik-1)) then
                    call utmess('F', 'MODELISA6_93')
!              CALL FOINTA (IFON,NBPAR,NOMPAR,VALPAR,VALRES(IRES))
                    icodre(ires) = 0
                endif
160          continue
170      continue
    endif
9999  continue
!
    call rcvals(iarret, icodre, nbres, nomres)
!
end subroutine
