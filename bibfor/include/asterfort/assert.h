!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! because macros must be on a single line
! aslint: disable=C1509
#include "asterf.h"

#define ASSERT(cond) call assert(cond, TO_STRING(cond), __FILE__, __LINE__)

#define absent(a)   (.not.present(a))

! Exactly one argument is required
#define UN_PARMI2(a,b)      (present(a).neqv.present(b))
#define UN_PARMI3(a,b,c)    (present(a).neqv.present(b).neqv.present(c))
#define UN_PARMI4(a,b,c,d)  (present(a).neqv.present(b).neqv.present(c).neqv.present(d))

! At least one argument is required
#define AU_MOINS_UN2(a,b)       (present(a).or.present(b))
#define AU_MOINS_UN3(a,b,c)     (present(a).or.present(b).or.present(c))
#define AU_MOINS_UN4(a,b,c,d)   (present(a).or.present(b).or.present(c).or.present(d))

! At most one argument is required (== AU_PLUS_UN)
#define EXCLUS2(a,b)        (absent(a).or.absent(b))
#define EXCLUS3(a,b,c)      ((absent(a).or.(absent(b).and.absent(c))).and.(absent(b).or.(absent(a).and.absent(c))).and.(absent(c).or.(absent(a).and.absent(b))))
#define EXCLUS4(a,b,c,d)    ((absent(a).or.(absent(b).and.absent(c).and.absent(d))).and.(absent(b).or.(absent(a).and.absent(c).and.absent(d))).and.(absent(c).or.(absent(a).and.absent(b).and.absent(d))).and.(absent(d).or.(absent(a).and.absent(b).and.absent(c))))

! 0 or all arguments
#define ENSEMBLE2(a,b)      ((present(a).and.present(b)).or.(absent(a).and.absent(b)))
#define ENSEMBLE3(a,b,c)    ((present(a).and.present(b).and.present(c)).or.(absent(a).and.absent(b).and.absent(c)))
#define ENSEMBLE4(a,b,c,d)  ((present(a).and.present(b).and.present(c).and.present(d)).or.(absent(a).and.absent(b).and.absent(c).and.absent(d)))

interface
    subroutine assert(cond, str_cond, fname, line)
        logical :: cond
        character(len=*) :: str_cond
        character(len=*) :: fname
        integer :: line
    end subroutine assert
end interface
