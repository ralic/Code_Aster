subroutine cuimp2(ifm, iliai, typope, typeou, resocu)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
    integer :: ifm
    character(len=24) :: resocu
    character(len=1) :: typope
    character(len=3) :: typeou
    integer :: iliai
!
! ----------------------------------------------------------------------
! ROUTINE APPELEE PAR : ALGOCU
! ----------------------------------------------------------------------
!
! IMPRESSION DE L'ACTIVATION/DESACTIVATION DE LA LIAISON
!
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
! IN  ILIAI  : NUMERO DE LA LIAISON
! IN  DEFICU : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
!
!
!
!
    character(len=8) :: noe, cmp
    character(len=20) :: chaiac
    character(len=24) :: nomnoe, nomcmp
    integer :: jnomno, jnomcm
!
! ----------------------------------------------------------------------
!
!
    nomnoe = resocu(1:14)//'.NOMNOE'
    nomcmp = resocu(1:14)//'.NOMCMP'
    call jeveuo(nomnoe, 'L', jnomno)
    call jeveuo(nomcmp, 'L', jnomcm)
!
    cmp = zk8(jnomcm-1+iliai)
    noe = zk8(jnomno-1+iliai)
!
    if (typope .eq. 'A') then
        chaiac = ' ACTIVEE     (ECART:'
    else if (typope.eq.'S') then
        chaiac = ' DESACTIVEE  (ECART:'
    endif
!
    if (typeou .eq. 'ALG') then
        write (ifm,1000) iliai,'(',noe,' - ',cmp,'): ', chaiac,')'
    else if (typeou.eq.'PIV') then
        chaiac = ' PIVOT NUL         ('
        write (ifm,1001) iliai,'(',noe,' - ',cmp,'): PIVOT NUL '
    else
        ASSERT(.false.)
    endif
!
!
!
    1000 format (' <LIA_UNIL> <> LIAISON ',i5,a1,a8,a3,a8,a3,a20,a1)
    1001 format (' <LIA_UNIL> <> LIAISON ',i5,a1,a8,a4,a8,a13)
!
!
!
end subroutine
