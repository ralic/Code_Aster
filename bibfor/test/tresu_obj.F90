subroutine tresu_obj(nomobj, type, tbtxt, refi, refr,&
                     epsi, crit, llab, ssigne, ignore, &
                     compare)
    implicit none
#include "asterfort/jeexin.h"
#include "asterfort/tstobj.h"
#include "asterfort/tresu_print.h"
#include "asterfort/utmess.h"
    character(len=24), intent(in) :: nomobj
    character(len=*), intent(in) :: type
    character(len=16), intent(in) :: tbtxt(2)
    integer, intent(in) :: refi
    real(kind=8), intent(in) :: refr
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: crit
    logical, intent(in) :: llab
    character(len=*), intent(in) :: ssigne
    logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
! ----------------------------------------------------------------------
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
!     ENTREES:
!        NOMOBJ : NOM DE L'OBJET JEVEUX QUE L'ON VEUT TESTER
!        TYPE   : TYPE DE VALEUR A TESTER :
!                  /'RESUME' : ENTIER QUI RESUME L'OBJET
!                  /'S_I'    : SOMME ENTIERE DE L'OBJET
!                  /'S_R'    : SOMME REELLE DE L'OBJET
!        TBTXT  : (1) : REFERENCE
!                 (2) : LEGENDE
!        REFI   : VALEUR ENTIERE ATTENDUE POUR L'OBJET
!        REFR   : VALEUR REELLE ATTENDUE POUR L'OBJET
!        CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
!        EPSI   : PRECISION ESPEREE
!        LLAB   : FLAG D IMPRESSION DES LABELS
!     SORTIES:
!      LISTING ...
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
    character(len=3) :: tysc
    real(kind=8) :: sommr
    integer :: resume, sommi, lonuti, lonmax, ni, iret, iret2
    logical :: skip
    real(kind=8) :: ordgrd
!
    skip = .false.
    if (present(ignore)) then
        skip = ignore
    endif
!
    ordgrd = 1.d0
    if (present(compare)) then
        ordgrd = compare
    endif
!
    call tstobj(nomobj, 'NON', resume, sommi, sommr,&
                lonuti, lonmax, tysc, iret, ni)
!
    if (iret .eq. 0) then
        if (type .eq. 'RESUME') then
            call tresu_print(tbtxt(1), tbtxt(2), llab, 1, crit, &
                        epsi, ssigne, refi=[refi], vali=resume)
        else if (type.eq.'I') then
            call tresu_print(tbtxt(1), tbtxt(2), llab, 1, crit, &
                        epsi, ssigne, refi=[refi], vali=sommi)
        else if (type.eq.'R') then
            call tresu_print(tbtxt(1), tbtxt(2), llab, 1, crit, &
                        epsi, ssigne, refr=[refr], valr=sommr, ignore=skip, &
                        compare=ordgrd)
        endif
    else
        call jeexin(nomobj, iret2)
        if (iret2 .le. 0) then
            call utmess('F', 'CALCULEL6_86', sk=nomobj)
        else
            call utmess('F', 'CALCULEL6_87', sk=nomobj)
        endif
    endif
!
end subroutine
