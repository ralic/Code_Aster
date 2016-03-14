subroutine cffact(ldscon, isto, nbliac, &
                  indfac, lechec)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/tldlg3.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    integer :: nbliac, indfac
    integer :: ldscon
    integer :: isto
    aster_logical :: lechec
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! FACTORISATION LDLT DE [-A.C-1.AT]
!
! ----------------------------------------------------------------------
!
! ATTENTION : SI ON RAJOUTE DES LIAISONS ON NE FACTORISE QUE
! LA PARTIE RAJOUTEE (LE RESTE EST ENCORE VALABLE, CF. PROPRIETES
! MAGIQUES DES FACTORISATIONS).
! SI ON ENLEVE LA DERNIERE LIAISON (IDEBUT > NBLIAC),PAS BESOIN DE
! REFACTORISER : L'INSTRUCTION ZI(LDSCON+2) = NBLIAC ECRITE PLUS
! LOIN FERA QUE RLDLGG PRENDRA LA BONNE TAILLE DE MATRICE, QUI
! EST DEJA FACTORISEE (SI ON REFACTORISAIT A PARTIR DE 1, ON
! FACTORISERAIT LA FACTORISEE, CE QUI EST GENANT, CAR
! FACTORISATION EN PLACE)
!
! IN  LDSCON : DESCRIPTEUR DE LA MATRICE DE CONTACT
! IN  ISTO   : INDICATEUR D'ARRET EN CAS DE PIVOT NUL
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O INDFAC : INDICE DE DEBUT DE LA FACTORISATION
! OUT LECHEC : .TRUE. SI LA FACTORISATION A ECHOUE (MATRICE SINGULIERE)
!
!
!
!
    integer :: ifm, niv
    integer :: ilideb, ilifin, ier
    integer :: ndeci, isingu, npvneg
!
! ----------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    lechec = .false.
    if (indfac .le. nbliac) then
        if (niv .ge. 2) then
            write(ifm,*)'<CONTACT><CALC> FACTORISATION MATRICE CONTACT '
        endif
        ilideb = indfac
        ilifin = nbliac
        call tldlg3('LDLT',' ',2, ldscon, ilideb, ilifin, 0,&
                    ndeci, isingu, npvneg, ier,' ')
        indfac = ilifin + 1
        if (ier .gt. isto) then
            lechec = .true.
        endif
    endif
!
end subroutine
